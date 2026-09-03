#' Read Vertices out of an sf LINESTRING/MULTILINESTRING Table
#'
#' Converts an sf object carrying route_id and direction columns into the
#' long vertex form the geometry builders work on. MULTILINESTRING parts are
#' concatenated in part order: sf::st_coordinates() emits rows feature by
#' feature and, within a feature, part by part (its L1 column), so the row
#' order it returns already is that concatenation.
#'
#' @return A data.table with columns route_id, direction, vertex_seq,
#'   latitude, longitude.
#' @noRd
geometries_from_sf <- function(geometries, name) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "The 'sf' package is required to read geometries from an 'sf' object. ",
      "Install it with install.packages('sf'), or pass the vertices as a ",
      "data.frame with columns route_id, direction, vertex_seq, latitude, ",
      "longitude.",
      call. = FALSE
    )
  }
  if (!inherits(geometries, "sf")) {
    stop(
      "'",
      name,
      "' is a bare 'sfc' geometry column, which carries no route_id or ",
      "direction; pass an 'sf' object with those two columns.",
      call. = FALSE
    )
  }

  types <- unique(as.character(sf::st_geometry_type(geometries)))
  bad_types <- setdiff(types, c("LINESTRING", "MULTILINESTRING"))
  if (length(bad_types) > 0L) {
    stop(
      "'",
      name,
      "' must hold LINESTRING or MULTILINESTRING geometries; found ",
      paste(bad_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  attrs <- data.table::as.data.table(sf::st_drop_geometry(geometries))
  validate_required_columns(attrs, c("route_id", "direction"), name)

  coords <- sf::st_coordinates(geometries)
  cn <- colnames(coords)
  # LINESTRING: X, Y, L1 = feature. MULTILINESTRING: X, Y, L1 = part within
  # the feature, L2 = feature.
  feature <- as.integer(if ("L2" %in% cn) coords[, "L2"] else coords[, "L1"])

  data.table::data.table(
    route_id = as_id_chr(attrs$route_id)[feature],
    direction = as_id_chr(attrs$direction)[feature],
    # Row order out of st_coordinates() is feature, then part, then vertex,
    # so a running index is the vertex order within every direction.
    vertex_seq = as.double(seq_len(nrow(coords))),
    latitude = as.double(coords[, "Y"]),
    longitude = as.double(coords[, "X"])
  )
}

#' Normalize a Geometry Input to Ordered Long-Form Vertices of One Route
#'
#' The single input normalizer behind both g2g_*_from_geometries() functions:
#' accepts the long vertex data.frame or an sf object, filters to one route,
#' and returns the vertices sorted by direction and vertex_seq.
#'
#' @return A data.table with columns route_id, direction, vertex_seq,
#'   latitude, longitude, sorted by direction then vertex_seq.
#' @noRd
normalize_geometries <- function(geometries, route_id, name = "geometries") {
  if (
    (!is.character(route_id) && !is.numeric(route_id)) || length(route_id) != 1L
  ) {
    stop("'route_id' must be a single route identifier.", call. = FALSE)
  }

  if (inherits(geometries, "sf") || inherits(geometries, "sfc")) {
    dt <- geometries_from_sf(geometries, name)
  } else {
    if (!is.data.frame(geometries)) {
      stop(
        "'",
        name,
        "' must be a data.frame of vertices with columns route_id, ",
        "direction, vertex_seq, latitude, longitude, or an 'sf' object of ",
        "LINESTRING/MULTILINESTRING geometries.",
        call. = FALSE
      )
    }
    dt <- data.table::as.data.table(geometries)
    validate_required_columns(
      dt,
      c("route_id", "direction", "vertex_seq", "latitude", "longitude"),
      name
    )
    if (!is.numeric(dt$vertex_seq)) {
      stop(
        "'",
        name,
        "$vertex_seq' must be numeric: it is the order of the vertices ",
        "within a direction, and character sorting would reorder the line.",
        call. = FALSE
      )
    }
    # The right-hand `route_id` here is the column, not the argument.
    dt <- dt[, .(
      route_id = as_id_chr(route_id),
      direction = as_id_chr(direction),
      vertex_seq = as.double(vertex_seq),
      latitude = as.double(latitude),
      longitude = as.double(longitude)
    )]
  }

  all_ids <- unique(dt$route_id)
  # Computed outside `[` so the argument is not shadowed by the route_id column
  keep <- dt$route_id == as_id_chr(route_id)
  dt <- dt[which(keep)]
  if (nrow(dt) == 0L) {
    stop(
      "route_id '",
      route_id,
      "' not found in '",
      name,
      "'. Available: ",
      paste(utils::head(all_ids, 20), collapse = ", "),
      call. = FALSE
    )
  }

  incomplete <- is.na(dt$latitude) |
    is.na(dt$longitude) |
    is.na(dt$vertex_seq) |
    is.na(dt$direction) |
    !nzchar(trimws(dt$direction))
  if (any(incomplete)) {
    stop(
      sum(incomplete),
      " vertex(es) of route_id '",
      route_id,
      "' have a missing coordinate, vertex_seq or direction. A line cannot ",
      "be ordered or measured with gaps in it; drop or repair those rows.",
      call. = FALSE
    )
  }

  data.table::setorderv(dt, c("direction", "vertex_seq"))
  counts <- table(dt$direction)
  too_short <- names(counts)[counts < 2L]
  if (length(too_short) > 0L) {
    stop(
      "Direction(s) ",
      paste(too_short, collapse = ", "),
      " of route_id '",
      route_id,
      "' have fewer than two vertices; a line needs at least two.",
      call. = FALSE
    )
  }
  dt[]
}

#' Require Exactly Two Direction Groups
#'
#' The gps2gtfs terminal model is one route with two ends. Shared by both
#' geometry builders so the pair cannot disagree about what the input is.
#'
#' @noRd
validate_two_directions <- function(directions, route_id, name = "geometries") {
  if (length(directions) == 2L) {
    return(invisible(directions))
  }
  stop(
    "route_id '",
    route_id,
    "' has ",
    length(directions),
    " direction group(s) in '",
    name,
    "' (",
    paste(directions, collapse = ", "),
    "); exactly two are required, one per direction of travel. Loop routes ",
    "and services with more than two ends have no two terminals at all and ",
    "are segmented with segmentation = \"layover\", which needs no ",
    "terminals_data.",
    call. = FALSE
  )
}

#' Distance in Meters from Points to a Polyline
#'
#' Point-to-segment, not point-to-vertex: a stop halfway along a 300 m
#' segment is on the route, and a nearest-vertex rule would put it 150 m
#' away and drop it. Distances are computed on a local equirectangular
#' projection in meters (cos factor taken at the line's mean latitude), which
#' over a route corridor is accurate to well under the meter scale a stop
#' buffer cares about. Vectorized over points x segments.
#'
#' @return Numeric vector, one distance per point.
#' @noRd
point_polyline_distance_m <- function(plat, plon, vlat, vlon) {
  n <- length(vlat)
  ky <- 6371000 * pi / 180
  kx <- ky * cos(mean(vlat) * pi / 180)

  px <- plon * kx
  py <- plat * ky
  x1 <- vlon[-n] * kx
  y1 <- vlat[-n] * ky
  dx <- vlon[-1L] * kx - x1
  dy <- vlat[-1L] * ky - y1
  len2 <- dx * dx + dy * dy

  # Rows are points, columns are segments.
  wx <- outer(px, x1, "-")
  wy <- outer(py, y1, "-")
  frac <- sweep(wx, 2L, dx, "*") + sweep(wy, 2L, dy, "*")
  frac <- sweep(frac, 2L, ifelse(len2 > 0, len2, 1), "/")
  frac[frac < 0] <- 0
  frac[frac > 1] <- 1
  # A repeated vertex is a point, not a segment: project onto its start.
  frac[, len2 == 0] <- 0

  ex <- wx - sweep(frac, 2L, dx, "*")
  ey <- wy - sweep(frac, 2L, dy, "*")
  d <- sqrt(ex * ex + ey * ey)
  if (nrow(d) == 0L) {
    return(numeric(0))
  }
  d[cbind(seq_len(nrow(d)), max.col(-d, ties.method = "first"))]
}

#' Derive trip terminals from supplied route geometries
#'
#' Determines the two terminals of a route from the route's own linestrings,
#' with no GTFS feed involved: each direction's terminal is the \emph{first}
#' vertex of that direction's line, which is where a vehicle running that
#' direction starts. The result feeds directly into \code{terminals_data} of
#' \code{\link{g2g_extract_trips}} and
#' \code{\link{g2g_extract_trips_and_stop_times}}.
#'
#' The \code{terminal_id} is the direction label itself. That is deliberate:
#' \code{\link{g2g_stops_from_geometries}} labels stops with the same values,
#' so the stop labels already are terminal IDs and no
#' \code{stop_direction_map} is needed (the convention
#' \code{\link{g2g_stops_from_gtfs}} documents).
#'
#' Exactly two directions are required, because the terminal model is one
#' route with two ends. A route drawn as one loop, or with a third branch
#' variant, errors; use \code{segmentation = "layover"} for those. Two first
#' vertices a few meters apart are not two ends of a line either, and warn.
#'
#' @param geometries The route geometries, in either of two forms:
#'   \itemize{
#'     \item a data.frame of vertices in long form, with columns
#'       \code{route_id}, \code{direction}, \code{vertex_seq} (the numeric
#'       order of the vertices within a direction), \code{latitude} and
#'       \code{longitude}; or
#'     \item an \code{sf} object of \code{LINESTRING} or
#'       \code{MULTILINESTRING} geometries with \code{route_id} and
#'       \code{direction} columns (requires the 'sf' package).
#'       \code{MULTILINESTRING} parts are concatenated in part order.
#'   }
#' @param route_id A single route identifier present in \code{geometries}.
#'   The gps2gtfs pipeline models one route (two terminals) at a time.
#' @return A data.table with columns \code{terminal_id}, \code{latitude},
#'   \code{longitude}, \code{direction}, one row per direction.
#'   \code{terminal_id} equals \code{direction}.
#' @seealso \code{\link{g2g_stops_from_geometries}} for the matching
#'   \code{stops_data}, and \code{\link{g2g_terminals_from_gtfs}} for the
#'   same table derived from a planned GTFS feed.
#' @examples
#' geometries <- data.frame(
#'   route_id = "r1",
#'   direction = rep(c("east", "west"), each = 3),
#'   vertex_seq = rep(1:3, times = 2),
#'   latitude = c(7.29, 7.30, 7.31, 7.31, 7.30, 7.29),
#'   longitude = c(80.63, 80.64, 80.65, 80.65, 80.64, 80.63)
#' )
#' g2g_terminals_from_geometries(geometries, route_id = "r1")
#' @export
g2g_terminals_from_geometries <- function(geometries, route_id) {
  geom <- normalize_geometries(geometries, route_id)

  out <- geom[,
    .(latitude = latitude[1L], longitude = longitude[1L]),
    by = direction
  ]
  validate_two_directions(out$direction, route_id)

  out[, terminal_id := direction]
  data.table::setcolorder(
    out,
    c("terminal_id", "latitude", "longitude", "direction")
  )

  # Two starts a few meters apart are not the two ends of a route: they are
  # two platforms of one place, or - the usual cause with supplied geometry -
  # one direction drawn twice. Segmentation would then cut trips at a single
  # location and direction would be meaningless, so say so.
  separation <- haversine_m_r(
    out$latitude[1L],
    out$longitude[1L],
    out$latitude[2L],
    out$longitude[2L]
  )
  if (is.finite(separation) && separation < terminal_separation_floor_m) {
    warning(
      "The two direction terminals (",
      paste(out$terminal_id, collapse = ", "),
      ") are only ",
      round(separation),
      " m apart, so they are probably two platforms of the same place - or ",
      "one direction drawn twice - rather than the two ends of the route. ",
      "Check that the two lines start at opposite ends, or use ",
      "segmentation = \"layover\".",
      call. = FALSE
    )
  }
  out[]
}

#' Derive a stops table from supplied route geometries
#'
#' Builds the \code{stops_data} input for
#' \code{\link{g2g_extract_trips_and_stop_times}} from the route's own
#' linestrings and a plain table of stop coordinates, with no GTFS feed
#' involved: a stop belongs to a direction when it lies within
#' \code{buffer_m} of that direction's line.
#'
#' Distance is measured to the \emph{polyline}, not to its vertices. A stop
#' sitting halfway along a 300 m segment is on the route; a nearest-vertex
#' rule would measure 150 m and drop it.
#'
#' Direction labels are the \code{terminal_id}s that
#' \code{\link{g2g_terminals_from_geometries}} emits for the same input, so
#' the two tables drop into \code{terminals_data} and \code{stops_data}
#' together and no \code{stop_direction_map} is needed.
#'
#' A stop within \code{buffer_m} of both lines - the usual case for the two
#' sides of one street - is returned once per direction, which is what makes
#' the direction labels do any work. Stops farther than \code{buffer_m} from
#' every direction are simply absent, and their count is reported in a
#' message. If that is every stop, the result is a zero-row table and a
#' warning, not an error, so a caller looping over many routes can take that
#' answer for one of them. Terminals are not treated specially and are not
#' removed.
#'
#' @inheritParams g2g_terminals_from_geometries
#' @param stops A data.frame of stop coordinates with columns \code{stop_id},
#'   \code{latitude}, \code{longitude}. Any other columns are dropped. Rows
#'   with a missing coordinate, or a missing or blank \code{stop_id}, are
#'   dropped with a warning giving the counts.
#' @param buffer_m Numeric. Maximum distance in meters from a direction's
#'   line for a stop to be assigned to that direction. Default \code{50}.
#' @return A data.table with columns \code{stop_id}, \code{latitude},
#'   \code{longitude}, \code{direction} (the \code{terminal_id} of the
#'   direction the stop was matched to), sorted by \code{direction} then
#'   \code{stop_id}. Stops matched in both directions appear once per
#'   direction. Zero rows, with the same four columns and types, when no stop
#'   is within \code{buffer_m} of either direction.
#' @seealso \code{\link{g2g_terminals_from_geometries}} for the matching
#'   \code{terminals_data}, and \code{\link{g2g_stops_from_gtfs}} for the
#'   same table derived from a planned GTFS feed.
#' @examples
#' geometries <- data.frame(
#'   route_id = "r1",
#'   direction = rep(c("east", "west"), each = 3),
#'   vertex_seq = rep(1:3, times = 2),
#'   latitude = c(7.29, 7.30, 7.31, 7.31, 7.30, 7.29),
#'   longitude = c(80.63, 80.64, 80.65, 80.65, 80.64, 80.63)
#' )
#' stops <- data.frame(
#'   stop_id = c("S1", "S2"),
#'   latitude = c(7.295, 7.305),
#'   longitude = c(80.635, 80.645)
#' )
#' g2g_stops_from_geometries(geometries, stops, route_id = "r1")
#' @export
g2g_stops_from_geometries <- function(
  geometries,
  stops,
  route_id,
  buffer_m = 50
) {
  validate_positive_radius(buffer_m, "buffer_m")
  geom <- normalize_geometries(geometries, route_id)
  directions <- unique(geom$direction)
  validate_two_directions(directions, route_id)

  if (!is.data.frame(stops)) {
    stop(
      "'stops' must be a data.frame with columns stop_id, latitude, ",
      "longitude.",
      call. = FALSE
    )
  }
  sdt <- data.table::as.data.table(stops)
  validate_required_columns(
    sdt,
    c("stop_id", "latitude", "longitude"),
    "stops"
  )
  sdt <- sdt[, .(
    stop_id = as_id_chr(stop_id),
    latitude = as.double(latitude),
    longitude = as.double(longitude)
  )]

  n_stops_in <- nrow(sdt)
  bad_id <- is.na(sdt$stop_id) | !nzchar(trimws(sdt$stop_id))
  bad_coord <- is.na(sdt$latitude) | is.na(sdt$longitude)
  drop <- bad_id | bad_coord
  if (any(drop)) {
    warning(
      sum(drop),
      " of ",
      n_stops_in,
      " stop(s) dropped before matching: ",
      sum(bad_coord),
      " with a missing coordinate and ",
      sum(bad_id),
      " with a missing or blank stop_id (a row can be counted in both).",
      call. = FALSE
    )
    sdt <- sdt[which(!drop)]
  }
  if (nrow(sdt) == 0L) {
    stop(
      "No usable stops: every row has a missing coordinate or a missing or ",
      "blank stop_id.",
      call. = FALSE
    )
  }

  matched <- lapply(directions, function(d) {
    line <- geom[which(geom$direction == d)]
    dist_m <- point_polyline_distance_m(
      sdt$latitude,
      sdt$longitude,
      line$latitude,
      line$longitude
    )
    keep <- which(dist_m <= buffer_m)
    if (length(keep) == 0L) {
      return(NULL)
    }
    data.table::data.table(
      stop_id = sdt$stop_id[keep],
      latitude = sdt$latitude[keep],
      longitude = sdt$longitude[keep],
      direction = d
    )
  })

  out <- data.table::rbindlist(matched)
  # A route none of the supplied stops reaches is an empty result, not a
  # failure: a caller looping over dozens of routes must be able to take that
  # answer for one of them. It is still worth a warning, because the usual
  # cause is not "this route has no stops".
  if (nrow(out) == 0L) {
    warning(
      "No stop is within ",
      buffer_m,
      " m of any direction of route_id '",
      route_id,
      "'. Check that 'stops' and 'geometries' use the same coordinate ",
      "reference system (both must be lon/lat degrees), or raise 'buffer_m'.",
      call. = FALSE
    )
    out <- data.table::data.table(
      stop_id = character(),
      latitude = double(),
      longitude = double(),
      direction = character()
    )
    data.table::setkeyv(out, c("direction", "stop_id"))
    return(out[])
  }

  n_unmatched <- length(setdiff(sdt$stop_id, out$stop_id))
  if (n_unmatched > 0L) {
    message(
      "[INFO] ",
      n_unmatched,
      " of ",
      nrow(sdt),
      " stop(s) are farther than ",
      buffer_m,
      " m from every direction of route_id '",
      route_id,
      "' and are not in the result."
    )
  }

  data.table::setcolorder(
    out,
    c("stop_id", "latitude", "longitude", "direction")
  )
  data.table::setkeyv(out, c("direction", "stop_id"))
  out[]
}
