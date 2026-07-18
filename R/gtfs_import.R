#' Coerce a GTFS Input to a List of data.tables
#'
#' Accepts a gtfsio/gtfstools-style object (named list of data.frames keyed by
#' file name) or a path to a GTFS zip (imported via the optional 'gtfsio'
#' package).
#'
#' @param gtfs A GTFS feed object or a path to a GTFS zip file.
#' @return A named list of data.tables.
#' @noRd
read_gtfs_input <- function(gtfs) {
  if (is.character(gtfs) && length(gtfs) == 1L) {
    if (!requireNamespace("gtfsio", quietly = TRUE)) {
      stop(
        "The 'gtfsio' package is required to read GTFS feeds from a path. ",
        "Install it with install.packages('gtfsio'), or pass an already ",
        "imported feed object.",
        call. = FALSE
      )
    }
    gtfs <- gtfsio::import_gtfs(gtfs)
  }
  if (!is.list(gtfs) || is.null(names(gtfs))) {
    stop(
      "'gtfs' must be a GTFS feed object (named list of data.frames) or a ",
      "path to a GTFS zip file.",
      call. = FALSE
    )
  }
  gtfs
}

#' Extract One Table from a GTFS Feed Object
#'
#' @param gtfs A GTFS feed object (named list).
#' @param name File name without extension (e.g. "stops").
#' @param required_cols Character vector of columns that must be present.
#' @return A data.table copy of the requested table.
#' @noRd
get_gtfs_table <- function(gtfs, name, required_cols = character()) {
  tbl <- gtfs[[name]]
  if (is.null(tbl)) {
    stop(
      "GTFS feed is missing required file '",
      name,
      ".txt'.",
      call. = FALSE
    )
  }
  dt <- data.table::as.data.table(tbl)
  validate_required_columns(dt, required_cols, paste0(name, ".txt"))
  dt
}

#' Resolve Trip Endpoints for One Route
#'
#' Internal helper shared by the g2g_*_from_gtfs() functions: filters trips to
#' one route and returns, per trip, the first and last stop_id by
#' stop_sequence.
#'
#' @return A data.table with columns trip_id, first_stop, last_stop.
#' @noRd
gtfs_trip_endpoints <- function(gtfs, route_id) {
  trips <- get_gtfs_table(gtfs, "trips", c("trip_id", "route_id"))
  if (
    !is.character(route_id) && !is.numeric(route_id) || length(route_id) != 1L
  ) {
    stop("'route_id' must be a single route identifier.", call. = FALSE)
  }
  # Computed outside `[` so the argument is not shadowed by the route_id column
  keep_trip <- as.character(trips$route_id) == as.character(route_id)
  route_trips <- trips[which(keep_trip)]
  if (nrow(route_trips) == 0L) {
    stop(
      "route_id '",
      route_id,
      "' not found in trips.txt. Available: ",
      paste(utils::head(unique(as.character(trips$route_id)), 20), collapse = ", "),
      call. = FALSE
    )
  }

  st <- get_gtfs_table(
    gtfs,
    "stop_times",
    c("trip_id", "stop_id", "stop_sequence")
  )
  st <- st[st$trip_id %in% route_trips$trip_id]
  if (nrow(st) == 0L) {
    stop(
      "No stop_times.txt entries found for route_id '",
      route_id,
      "'.",
      call. = FALSE
    )
  }
  st[, stop_sequence := as.numeric(stop_sequence)]
  data.table::setkeyv(st, c("trip_id", "stop_sequence"))

  endpoints <- st[,
    .(
      first_stop = as.character(stop_id[1L]),
      last_stop = as.character(stop_id[.N])
    ),
    by = trip_id
  ]
  endpoints
}

#' Derive Trip Terminals from a Static GTFS Feed
#'
#' Determines the two terminals of a route from a planned (baseline) GTFS
#' feed: the two most frequent trip endpoints (first/last stop per trip in
#' \code{stop_times.txt}). The result feeds directly into
#' \code{terminals_data} of \code{\link{g2g_extract_trips}} and
#' \code{\link{g2g_extract_trips_and_stop_times}}.
#'
#' @param gtfs A GTFS feed object (named list of data.frames, as returned by
#'   \code{gtfsio::import_gtfs()} or \code{gtfstools::read_gtfs()}) or a path
#'   to a GTFS zip file (requires the 'gtfsio' package).
#' @param route_id A single route identifier present in \code{trips.txt}. The
#'   gps2gtfs pipeline models one route (two terminals) at a time.
#' @return A data.table with columns \code{terminal_id}, \code{latitude},
#'   \code{longitude}.
#' @examples
#' gtfs <- list(
#'   trips = data.frame(trip_id = c("t1", "t2"), route_id = "r1"),
#'   stop_times = data.frame(
#'     trip_id = c("t1", "t1", "t2", "t2"),
#'     stop_id = c("A", "B", "B", "A"),
#'     stop_sequence = c(1, 2, 1, 2)
#'   ),
#'   stops = data.frame(
#'     stop_id = c("A", "B"),
#'     stop_lat = c(7.29, 7.31),
#'     stop_lon = c(80.63, 80.65)
#'   )
#' )
#' g2g_terminals_from_gtfs(gtfs, route_id = "r1")
#' @export
g2g_terminals_from_gtfs <- function(gtfs, route_id) {
  gtfs <- read_gtfs_input(gtfs)
  endpoints <- gtfs_trip_endpoints(gtfs, route_id)

  counts <- sort(
    table(c(endpoints$first_stop, endpoints$last_stop)),
    decreasing = TRUE
  )
  if (length(counts) < 2L) {
    stop(
      "route_id '",
      route_id,
      "' has fewer than two distinct trip endpoints (loop route?); ",
      "terminals cannot be derived automatically.",
      call. = FALSE
    )
  }
  terminal_ids <- names(counts)[1:2]
  covered <- sum(counts[terminal_ids]) / sum(counts)
  if (covered < 0.9) {
    warning(
      "Only ",
      round(100 * covered),
      "% of trip endpoints match the two derived terminals (",
      paste(terminal_ids, collapse = ", "),
      "); the route may have variants or branches.",
      call. = FALSE
    )
  }

  stops <- get_gtfs_table(
    gtfs,
    "stops",
    c("stop_id", "stop_lat", "stop_lon")
  )
  stops <- stops[as.character(stops$stop_id) %in% terminal_ids]
  if (nrow(stops) < 2L) {
    stop(
      "Terminal stop ids (",
      paste(terminal_ids, collapse = ", "),
      ") not found in stops.txt.",
      call. = FALSE
    )
  }

  out <- data.table::data.table(
    terminal_id = as.character(stops$stop_id),
    latitude = as.double(stops$stop_lat),
    longitude = as.double(stops$stop_lon)
  )
  # Keep frequency order: most common endpoint first
  out[match(terminal_ids, out$terminal_id)]
}

#' Derive a Stops Table from a Static GTFS Feed
#'
#' Builds the \code{stops_data} input for
#' \code{\link{g2g_extract_trips_and_stop_times}} from a planned (baseline)
#' GTFS feed: all non-terminal stops served by a route, labeled with the
#' direction group they belong to. Direction labels are the \code{terminal_id}
#' a trip starts from, so they map onto the terminals derived by
#' \code{\link{g2g_terminals_from_gtfs}} without a manual
#' \code{stop_direction_map}.
#'
#' @inheritParams g2g_terminals_from_gtfs
#' @return A data.table with columns \code{stop_id}, \code{latitude},
#'   \code{longitude}, \code{direction} (the starting terminal_id of trips
#'   serving the stop in that direction). Stops served in both directions
#'   appear once per direction.
#' @examples
#' gtfs <- list(
#'   trips = data.frame(trip_id = c("t1", "t2"), route_id = "r1"),
#'   stop_times = data.frame(
#'     trip_id = c("t1", "t1", "t1", "t2", "t2", "t2"),
#'     stop_id = c("A", "S1", "B", "B", "S1", "A"),
#'     stop_sequence = c(1, 2, 3, 1, 2, 3)
#'   ),
#'   stops = data.frame(
#'     stop_id = c("A", "S1", "B"),
#'     stop_lat = c(7.29, 7.30, 7.31),
#'     stop_lon = c(80.63, 80.64, 80.65)
#'   )
#' )
#' g2g_stops_from_gtfs(gtfs, route_id = "r1")
#' @export
g2g_stops_from_gtfs <- function(gtfs, route_id) {
  gtfs <- read_gtfs_input(gtfs)
  terminals <- g2g_terminals_from_gtfs(gtfs, route_id)
  terminal_ids <- terminals$terminal_id
  endpoints <- gtfs_trip_endpoints(gtfs, route_id)

  # Direction group of each trip = the terminal it starts from
  endpoints <- endpoints[endpoints$first_stop %in% terminal_ids]
  if (nrow(endpoints) == 0L) {
    stop(
      "No trips of route_id '",
      route_id,
      "' start at a derived terminal; cannot assign stop directions.",
      call. = FALSE
    )
  }

  st <- get_gtfs_table(
    gtfs,
    "stop_times",
    c("trip_id", "stop_id", "stop_sequence")
  )
  st <- st[st$trip_id %in% endpoints$trip_id]
  st[, stop_id := as.character(stop_id)]
  st <- merge(
    st,
    endpoints[, .(trip_id, direction = first_stop)],
    by = "trip_id"
  )

  # Unique (stop, direction) pairs, terminals excluded (the pipeline drops
  # terminal points from stop matching anyway)
  stop_dirs <- unique(st[!(stop_id %in% terminal_ids), .(stop_id, direction)])

  stops <- get_gtfs_table(gtfs, "stops", c("stop_id", "stop_lat", "stop_lon"))
  stops <- stops[, .(
    stop_id = as.character(stop_id),
    latitude = as.double(stop_lat),
    longitude = as.double(stop_lon)
  )]

  out <- merge(stop_dirs, stops, by = "stop_id")
  missing_coords <- setdiff(stop_dirs$stop_id, stops$stop_id)
  if (length(missing_coords) > 0L) {
    warning(
      length(missing_coords),
      " stop(s) referenced in stop_times.txt are missing from stops.txt ",
      "and were dropped.",
      call. = FALSE
    )
  }
  data.table::setcolorder(
    out,
    c("stop_id", "latitude", "longitude", "direction")
  )
  data.table::setkeyv(out, c("direction", "stop_id"))
  out[]
}