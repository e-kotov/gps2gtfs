#' Estimate stop coordinates from vehicle positions
#'
#' Baseline-free helper: when no static GTFS feed exists, stop locations can
#' be estimated from GTFS-Realtime Vehicle Positions that carry a
#' \code{stop_id} annotation. Each stop's coordinates are the median position
#' of the pings observed at it — by default only pings labeled
#' \code{STOPPED_AT}, the most precise signal.
#'
#' The result covers the \code{stop_id}/\code{latitude}/\code{longitude}
#' columns of the \code{stops_data} input to
#' \code{\link{g2g_extract_trips_and_stop_times}}; the \code{direction} label
#' (starting terminal of trips serving the stop) must still be supplied, e.g.
#' from the RT \code{trip_id}/\code{direction_id} annotations or a baseline
#' feed. It also fills the spec-required \code{stop_lat}/\code{stop_lon} of a
#' scaffolded \code{stops.txt}.
#'
#' @param positions A data.frame of vehicle positions (e.g. from
#'   \code{gtfsrealtime::read_gtfsrt_positions()} or \code{g2g_clean_gps()})
#'   with columns \code{stop_id}, \code{latitude}, \code{longitude}, and
#'   optionally \code{current_status}.
#' @param statuses Character vector of \code{current_status} values to use.
#'   Default \code{"STOPPED_AT"}. Ignored (all \code{stop_id}-annotated pings
#'   used, with a message) when the \code{current_status} column is absent.
#' @param min_obs Integer. Minimum number of pings required per stop; stops
#'   with fewer observations are dropped with a warning. Default \code{3}.
#' @return A data.table with columns \code{stop_id}, \code{latitude},
#'   \code{longitude}, \code{n_obs}, sorted by \code{stop_id}.
#' @importFrom stats median
#' @export
g2g_stops_from_positions <- function(
  positions,
  statuses = "STOPPED_AT",
  min_obs = 3L
) {
  dt <- data.table::as.data.table(positions)
  validate_required_columns(
    dt,
    c("stop_id", "latitude", "longitude"),
    "positions"
  )
  if (
    length(min_obs) != 1L || !is.numeric(min_obs) || is.na(min_obs) ||
      min_obs < 1
  ) {
    stop("'min_obs' must be one positive number.", call. = FALSE)
  }

  stop_chr <- as.character(dt$stop_id)
  keep <- !is.na(stop_chr) & nzchar(trimws(stop_chr))
  if ("current_status" %in% names(dt)) {
    keep <- keep & as.character(dt$current_status) %in% statuses
  } else {
    message(
      "[INFO] No 'current_status' column; using all stop_id-annotated pings."
    )
  }
  dt <- dt[which(keep)]
  if (nrow(dt) == 0L) {
    stop(
      "No usable pings: none carry a stop_id",
      if ("current_status" %in% names(positions)) {
        paste0(" with current_status in {", paste(statuses, collapse = ", "), "}")
      },
      ".",
      call. = FALSE
    )
  }

  out <- dt[,
    .(
      latitude = stats::median(as.double(latitude)),
      longitude = stats::median(as.double(longitude)),
      n_obs = .N
    ),
    by = .(stop_id = as.character(stop_id))
  ]

  too_few <- out$n_obs < min_obs
  if (any(too_few)) {
    warning(
      sum(too_few),
      " stop(s) dropped with fewer than ",
      min_obs,
      " observation(s); their estimates would be unreliable.",
      call. = FALSE
    )
    out <- out[which(!too_few)]
  }

  data.table::setkeyv(out, "stop_id")
  out[]
}

#' Build shapes.txt-shaped traces from a pipeline trajectory
#'
#' Converts the ping-level trajectory of extracted trips into a table shaped
#' like the GTFS \code{shapes.txt} file: one shape per trip, points in travel
#' order, with cumulative distance. Unlike planned shapes, these traces record
#' the geometry \emph{actually driven}, including detours.
#'
#' Obtain the trajectory by running
#' \code{g2g_extract_trips_and_stop_times(..., return_trajectory = TRUE)} and
#' using the \code{$trajectory} element of the result.
#'
#' @param trajectory A data.table of trajectory GPS records with columns
#'   \code{trip_id}, \code{latitude}, \code{longitude}, \code{date},
#'   \code{time_str} (as returned in the pipeline's \code{$trajectory}).
#' @param shape_id_prefix Character prefix for generated shape ids. Default
#'   \code{"SHP_"} (shape ids become \code{SHP_<trip_id>}).
#' @return A data.table with GTFS-standard columns \code{shape_id},
#'   \code{shape_pt_lat}, \code{shape_pt_lon}, \code{shape_pt_sequence},
#'   \code{shape_dist_traveled} (meters).
#' @export
g2g_shapes_from_trips <- function(trajectory, shape_id_prefix = "SHP_") {
  dt <- data.table::as.data.table(trajectory)
  validate_required_columns(
    dt,
    c("trip_id", "latitude", "longitude", "date", "time_str"),
    "trajectory"
  )
  projected <- isTRUE(attr(trajectory, "projected"))

  if (nrow(dt) == 0L) {
    return(data.table::data.table(
      shape_id = character(),
      shape_pt_lat = double(),
      shape_pt_lon = double(),
      shape_pt_sequence = integer(),
      shape_dist_traveled = double()
    ))
  }

  dt <- data.table::copy(dt)
  data.table::setkeyv(dt, c("trip_id", "date", "time_str"))

  out <- dt[,
    {
      lat <- as.double(latitude)
      lon <- as.double(longitude)
      n <- length(lat)
      step <- if (n > 1L) {
        if (projected) {
          sqrt(diff(lat)^2 + diff(lon)^2)
        } else {
          haversine_m_r(lat[-n], lon[-n], lat[-1L], lon[-1L])
        }
      } else {
        numeric(0)
      }
      .(
        shape_pt_lat = lat,
        shape_pt_lon = lon,
        shape_pt_sequence = seq_len(n),
        shape_dist_traveled = cumsum(c(0, step))
      )
    },
    by = .(shape_id = paste0(shape_id_prefix, trip_id))
  ]

  out[]
}