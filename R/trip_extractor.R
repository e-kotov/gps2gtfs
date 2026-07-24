#' Pure R Trip ID Assignment
#'
#' Sequential trip ID assignment loop in pure R.
#'
#' @param bus_stops Character vector of terminal IDs.
#' @param dates Character vector of dates.
#' @param device_ids Character vector of device IDs.
#' @return Integer vector of assigned trip IDs.
#' @noRd
assign_trip_ids_pure_r <- function(bus_stops, dates, device_ids) {
  n <- length(bus_stops)
  trip_ids <- integer(n)
  trip_counter <- 0L
  i <- 1
  while (i < n) {
    if (
      bus_stops[i] != bus_stops[i + 1] &&
        dates[i] == dates[i + 1] &&
        device_ids[i] == device_ids[i + 1]
    ) {
      trip_counter <- trip_counter + 1L
      trip_ids[i] <- trip_counter
      trip_ids[i + 1] <- trip_counter
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  return(trip_ids)
}

#' Haversine Distance in Meters (Vectorized, Pure R)
#'
#' @noRd
haversine_m_r <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371000 * 2 * asin(pmin(1, sqrt(a)))
}

#' Nearest Terminal ID for Each Point
#'
#' @noRd
nearest_terminal_id <- function(lat, lon, terminals, projected) {
  ids <- as.character(terminals$terminal_id)
  dmat <- vapply(
    seq_along(ids),
    function(j) {
      if (projected) {
        sqrt(
          (lat - as.double(terminals$latitude[j]))^2 +
            (lon - as.double(terminals$longitude[j]))^2
        )
      } else {
        haversine_m_r(
          lat,
          lon,
          as.double(terminals$latitude[j]),
          as.double(terminals$longitude[j])
        )
      }
    },
    numeric(length(lat))
  )
  dmat <- matrix(dmat, nrow = length(lat))
  ids[max.col(-dmat, ties.method = "first")]
}

#' Extract Trips from Supplied Trip Identities (Fast Path)
#'
#' When the GPS data already carries trip identities (e.g. GTFS-Realtime
#' Vehicle Positions with a \code{trip_id} annotation), segmentation by those
#' identities replaces terminal-buffer detection. Each contiguous run of the
#' same trip value per vehicle and driving session becomes one trip; its
#' first and last pings are treated as terminal entry/exit and matched to
#' the nearest terminal so that direction semantics stay identical to the
#' spatial path.
#'
#' @param cleaned_gps_dt A data.table of cleaned GPS data.
#' @param trip_terminals_df Terminal coordinates with \code{terminal_id}.
#' @param trip_col Name of the column holding the supplied trip identities.
#' @return A data.table shaped like the output of \code{extract_trips_r}:
#'   two rows (entry/exit) per trip with \code{bus_stop} and integer
#'   \code{trip_id}.
#' @noRd
extract_trips_from_ids_r <- function(
  cleaned_gps_dt,
  trip_terminals_df,
  trip_col,
  projected = NULL,
  session_gap = 4 * 3600,
  direction_col = NULL
) {
  missing_cols <- setdiff(c(trip_col, direction_col), names(cleaned_gps_dt))
  if (length(missing_cols) > 0L) {
    stop(
      "column(s) not found in the GPS data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(projected)) {
    projected <- attr(cleaned_gps_dt, "projected")
    if (is.null(projected)) {
      projected <- FALSE
    }
  }

  # Terminals are optional when direction is supplied from the data
  # (direction_col): they are then only used, if present, to drop terminal
  # points. Without direction_col they define the two directions.
  have_terminals <- !is.null(trip_terminals_df) &&
    nrow(data.table::as.data.table(trip_terminals_df)) > 0L
  if (have_terminals) {
    terminals <- normalize_coordinates(
      trip_terminals_df,
      projected = projected,
      name = "trip_terminals_df"
    )$dt
    validate_required_columns(terminals, "terminal_id", "trip_terminals_df")
    validate_identifiers(terminals$terminal_id, "trip_terminals_df 'terminal_id'")
  } else {
    terminals <- data.table::data.table(terminal_id = character(0))
  }

  # Segment-drop counters, stamped onto every return (including the
  # total-failure early returns below) so the diagnostics never lose the count
  # of what was dropped in exactly the runs that dropped everything.
  n_no_identity <- 0L
  n_single <- 0L
  stamp_seg_drops <- function(res) {
    attr(res, "seg_drops") <- c(
      rows_dropped_no_trip_identity = as.integer(n_no_identity),
      segments_dropped_single_ping = as.integer(n_single)
    )
    res
  }
  empty_result <- function() {
    result <- data.table::copy(cleaned_gps_dt[0])
    result[, bus_stop := character()]
    result[, trip_id := integer()]
    if (!is.null(direction_col)) {
      result[, rt_direction := character()]
    }
    stamp_seg_drops(result)
  }
  if (nrow(cleaned_gps_dt) == 0L) {
    return(empty_result())
  }
  if (!have_terminals && is.null(direction_col)) {
    stop(
      "no terminals supplied and no 'direction_col'; supply one so trip ",
      "direction can be assigned.",
      call. = FALSE
    )
  }

  dt <- data.table::copy(cleaned_gps_dt)
  # trip_col may name one column, or several that jointly identify a trip
  # (e.g. the GTFS-RT TripDescriptor route_id/direction_id/start_date/start_time
  # when vehicle positions carry no trip_id). Compose them into one identity;
  # a row is unusable only if every component is missing.
  if (length(trip_col) == 1L) {
    dt[, rt_trip_value := as.character(dt[[trip_col]])]
  } else {
    comp <- lapply(trip_col, function(col) {
      v <- as.character(dt[[col]])
      v[is.na(v)] <- ""
      v
    })
    joined <- do.call(paste, c(comp, sep = "_"))
    any_present <- Reduce(`|`, lapply(comp, nzchar))
    joined[!any_present] <- NA_character_
    dt[, rt_trip_value := joined]
  }
  seg_dt <- dt[!is.na(rt_trip_value) & nzchar(trimws(rt_trip_value))]
  n_no_identity <- nrow(dt) - nrow(seg_dt)
  if (nrow(seg_dt) == 0L) {
    message(
      "[INFO] Column '",
      trip_col,
      "' contains no usable trip identities; no trips extracted."
    )
    return(empty_result())
  }

  data.table::setkeyv(seg_dt, c("vehicle_id", "timestamp"))
  # Segment within driving sessions (ping gaps > session_gap start a new
  # one) instead of calendar days, so an annotated trip crossing midnight
  # stays one segment while a trip value reused the next day still splits.
  seg_dt[,
    trip_session := cumsum(c(0, diff(as.numeric(timestamp))) > session_gap),
    by = vehicle_id
  ]
  seg_dt[, trip_id := data.table::rleid(vehicle_id, trip_session, rt_trip_value)]
  seg_dt[, trip_session := NULL]
  seg_dt[, seg_n := .N, by = trip_id]

  n_single <- seg_dt[seg_n < 2L, data.table::uniqueN(trip_id)]
  if (n_single > 0L) {
    message(
      "[INFO] Skipped ",
      n_single,
      " single-ping trip segment(s) from '",
      trip_col,
      "'."
    )
  }
  seg_dt <- seg_dt[seg_n >= 2L]
  if (nrow(seg_dt) == 0L) {
    return(empty_result())
  }

  # Capture the supplied direction per trip (first non-missing value in the
  # segment) before collapsing to bounds, so direction can come from the data
  # instead of terminal inference.
  if (!is.null(direction_col)) {
    seg_dt[, rt_direction := {
      v <- as.character(.SD[[direction_col]])
      v <- v[!is.na(v) & nzchar(trimws(v))]
      if (length(v) > 0L) v[1L] else NA_character_
    }, by = trip_id, .SDcols = direction_col]
  }

  # Renumber to consecutive integers, then keep first/last ping per trip
  seg_dt[, trip_id := data.table::rleid(trip_id)]
  bounds <- seg_dt[, .SD[c(1L, .N)], by = trip_id]

  # Terminal assignment for direction is only needed in the classic path.
  # With direction_col, direction comes from rt_direction; terminals (if any)
  # are still matched so their points can be dropped from stop_times.
  if (have_terminals) {
    bounds[,
      bus_stop := nearest_terminal_id(
        as.double(latitude),
        as.double(longitude),
        terminals,
        projected
      )
    ]
  } else {
    bounds[, bus_stop := NA_character_]
  }
  # Keep rt_trip_value: it is the caller's official trip identity (e.g. the
  # GTFS-RT trip_id) and must survive to the output so downstream assembly
  # can preserve it. extract_trip_features_r() surfaces it as
  # provided_trip_id.
  bounds[, seg_n := NULL]
  stamp_seg_drops(bounds)[]
}

#' Dwell Run IDs via Greedy Anchor Clustering
#'
#' Assigns consecutive pings to "dwell runs": a run starts at an anchor ping
#' and a ping joins the current run while it stays within \code{radius}
#' meters of that anchor; the first ping beyond the radius becomes the anchor
#' of a new run. Anchoring on the run's first ping (rather than comparing
#' consecutive displacements) absorbs GPS jitter while a vehicle is parked.
#'
#' @param lat,lon Numeric coordinate vectors, time-ordered.
#' @param radius Numeric. Stationarity radius in meters.
#' @param projected Logical. Euclidean distance when TRUE, haversine otherwise.
#' @return Integer vector of 1-based run ids, one per ping.
#' @noRd
dwell_run_ids_r <- function(lat, lon, radius, projected) {
  n <- length(lat)
  if (n == 0L) {
    return(integer(0))
  }
  run <- integer(n)
  run[1L] <- 1L
  current <- 1L
  a_lat <- lat[1L]
  a_lon <- lon[1L]
  for (i in seq_len(n)[-1L]) {
    d <- if (projected) {
      sqrt((lat[i] - a_lat)^2 + (lon[i] - a_lon)^2)
    } else {
      haversine_m_r(lat[i], lon[i], a_lat, a_lon)
    }
    if (d > radius) {
      current <- current + 1L
      a_lat <- lat[i]
      a_lon <- lon[i]
    }
    run[i] <- current
  }
  run
}

#' Extract Trips by Layover Segmentation
#'
#' Segments raw GPS into trips without terminals or supplied trip identities:
#' a trip boundary is a layover, i.e. a dwell longer than \code{layover_gap}
#' anywhere along the route. Two boundary signals are combined per vehicle
#' and driving session: a silent gap between successive pings exceeding
#' \code{layover_gap}, and a stationary dwell (pings staying within
#' \code{layover_radius} of an anchor for longer than \code{layover_gap}).
#' The previous trip ends at the dwell's first ping (arrival) and the next
#' trip starts at its last ping (departure); interior dwell pings belong to
#' no trip. This handles short-turn, loop, and multi-branch services that the
#' two-terminal model cannot; direction is a single group in this mode.
#'
#' @param cleaned_gps_dt A data.table of cleaned GPS data.
#' @param trip_terminals_df Optional terminal coordinates with
#'   \code{terminal_id}; used only to label trip endpoints with the nearest
#'   terminal (cosmetic - direction never derives from it in this mode).
#' @param layover_gap Numeric. Dwells longer than this many seconds cut trips.
#' @param layover_radius Numeric. Stationarity radius in meters.
#' @param session_gap Numeric. Driving-session bound, as elsewhere.
#' @return A data.table shaped like the output of \code{extract_trips_r}:
#'   two rows (start/end ping) per trip with \code{bus_stop}, integer
#'   \code{trip_id}, and \code{rt_direction} set to the single layover
#'   direction level.
#' @noRd
extract_trips_layover_r <- function(
  cleaned_gps_dt,
  trip_terminals_df = NULL,
  layover_gap = 10 * 60,
  layover_radius = 50,
  session_gap = 4 * 3600,
  projected = NULL
) {
  if (is.null(projected)) {
    projected <- attr(cleaned_gps_dt, "projected")
    if (is.null(projected)) {
      projected <- FALSE
    }
  }
  have_terminals <- !is.null(trip_terminals_df) &&
    nrow(data.table::as.data.table(trip_terminals_df)) > 0L
  if (have_terminals) {
    terminals <- normalize_coordinates(
      trip_terminals_df,
      projected = projected,
      name = "trip_terminals_df"
    )$dt
    validate_required_columns(terminals, "terminal_id", "trip_terminals_df")
    validate_identifiers(terminals$terminal_id, "trip_terminals_df 'terminal_id'")
  }

  # Segment-drop counters, stamped onto every return (including the
  # total-failure early returns below) so an all-single-ping or all-stationary
  # run still reports what it dropped.
  n_single <- 0L
  n_stationary <- 0L
  stamp_seg_drops <- function(res) {
    attr(res, "seg_drops") <- c(
      segments_dropped_single_ping = as.integer(n_single),
      segments_dropped_stationary = as.integer(n_stationary)
    )
    res
  }
  empty_result <- function() {
    result <- data.table::copy(cleaned_gps_dt[0])
    result[, bus_stop := character()]
    result[, trip_id := integer()]
    result[, rt_direction := character()]
    stamp_seg_drops(result)
  }
  if (nrow(cleaned_gps_dt) == 0L) {
    return(empty_result())
  }

  seg_dt <- data.table::copy(cleaned_gps_dt)
  data.table::setkeyv(seg_dt, c("vehicle_id", "timestamp"))
  # Layover cuts compose with the coarser session cut: sessions first, then
  # layovers within each session.
  seg_dt[,
    trip_session := cumsum(c(0, diff(as.numeric(timestamp))) > session_gap),
    by = vehicle_id
  ]
  seg_dt[,
    dwell_run := dwell_run_ids_r(
      as.double(latitude),
      as.double(longitude),
      layover_radius,
      projected
    ),
    by = .(vehicle_id, trip_session)
  ]
  seg_dt[,
    `:=`(
      run_n = .N,
      run_span = as.numeric(timestamp[.N]) - as.numeric(timestamp[1L]),
      run_pos = seq_len(.N)
    ),
    by = .(vehicle_id, trip_session, dwell_run)
  ]
  # Short stationary runs (traffic lights, in-service stops) never cut.
  seg_dt[, is_layover_run := run_n >= 2L & run_span > layover_gap]

  # A new segment starts at a ping preceded by a silent layover, or at the
  # departure ping of a stationary layover.
  seg_dt[,
    gap_prev := c(0, diff(as.numeric(timestamp))),
    by = .(vehicle_id, trip_session)
  ]
  seg_dt[,
    new_seg := (gap_prev > layover_gap) | (is_layover_run & run_pos == run_n)
  ]
  seg_dt[, seg := cumsum(new_seg), by = .(vehicle_id, trip_session)]
  # Interior dwell pings (between arrival and departure) belong to no trip.
  seg_dt <- seg_dt[!(is_layover_run & run_pos > 1L & run_pos < run_n)]

  seg_dt[, trip_id := data.table::rleid(vehicle_id, trip_session, seg)]
  seg_dt[,
    `:=`(seg_n = .N, seg_runs = data.table::uniqueN(dwell_run)),
    by = trip_id
  ]

  n_single <- seg_dt[seg_n < 2L, data.table::uniqueN(trip_id)]
  if (n_single > 0L) {
    message(
      "[INFO] Skipped ",
      n_single,
      " single-ping trip segment(s) from layover segmentation."
    )
  }
  # A segment confined to one dwell run never moved beyond layover_radius
  # (e.g. a vehicle parked and pinging until a session gap): not a trip.
  n_stationary <- seg_dt[
    seg_n >= 2L & seg_runs < 2L,
    data.table::uniqueN(trip_id)
  ]
  if (n_stationary > 0L) {
    message(
      "[INFO] Skipped ",
      n_stationary,
      " stationary (no-movement) segment(s) from layover segmentation."
    )
  }
  seg_dt <- seg_dt[seg_n >= 2L & seg_runs >= 2L]
  if (nrow(seg_dt) == 0L) {
    return(empty_result())
  }

  # Renumber to consecutive integers, then keep first/last ping per trip
  seg_dt[, trip_id := data.table::rleid(trip_id)]
  bounds <- seg_dt[, .SD[c(1L, .N)], by = trip_id]

  if (have_terminals) {
    bounds[,
      bus_stop := nearest_terminal_id(
        as.double(latitude),
        as.double(longitude),
        terminals,
        projected
      )
    ]
  } else {
    bounds[, bus_stop := NA_character_]
  }
  bounds[, rt_direction := layover_direction_level]
  bounds[,
    c(
      "trip_session",
      "dwell_run",
      "run_n",
      "run_span",
      "run_pos",
      "is_layover_run",
      "gap_prev",
      "new_seg",
      "seg",
      "seg_n",
      "seg_runs"
    ) := NULL
  ]
  stamp_seg_drops(bounds)[]
}

#' Extract Trips from Cleaned GPS Data
#'
#' Converts GPS data and terminal coordinates to spatial objects, performs a spatial join
#' to find terminal visits, and identifies trips using Rcpp or pure R matching.
#'
#' @param cleaned_gps_dt A data.table containing cleaned GPS data (output of \code{clean_gps_data}).
#' @param trip_terminals_df A data.frame containing trip terminal coordinates with \code{terminal_id}.
#' @param buffer_radius Numeric. Buffer radius around terminals (in meters).
#' @param projected_crs Numeric. The EPSG code of a projected coordinate system to use (default: 5234).
#' @param backend Character. The backend to use: \code{"rcpp"}, \code{"pure_r"}, or \code{"rust"}. Default is \code{"rust"}.
#' @return A data.table containing GPS points at terminal entries/exits with assigned \code{trip_id}.
#' @importFrom data.table as.data.table setkeyv setnames
#' @noRd
extract_trips_r <- function(
  cleaned_gps_dt,
  trip_terminals_df,
  buffer_radius,
  projected_crs = 5234,
  backend = "rust",
  projected = NULL,
  session_gap = 4 * 3600
) {
  backend <- match.arg(backend, c("rcpp", "pure_r", "rust"))
  validate_positive_radius(buffer_radius, "buffer_radius")

  if (is.null(projected)) {
    projected <- attr(cleaned_gps_dt, "projected")
    if (is.null(projected)) {
      projected <- FALSE
    }
  }

  # Normalize and validate terminal coordinates
  norm_terminals <- normalize_coordinates(
    trip_terminals_df,
    projected = projected,
    name = "trip_terminals_df"
  )
  trip_terminals_df <- norm_terminals$dt
  validate_required_columns(
    trip_terminals_df,
    "terminal_id",
    "trip_terminals_df"
  )
  validate_identifiers(
    trip_terminals_df$terminal_id,
    "trip_terminals_df 'terminal_id'"
  )

  if (nrow(cleaned_gps_dt) == 0L || nrow(trip_terminals_df) == 0L) {
    result <- data.table::copy(cleaned_gps_dt[0])
    result[, bus_stop := character()]
    result[, trip_id := integer()]
    return(result)
  }

  if (backend == "rcpp") {
    # Match using C++ helper
    joined_dt <- data.table::copy(cleaned_gps_dt)
    joined_dt[,
      bus_stop := match_points_to_buffers_cpp(
        as.double(latitude),
        as.double(longitude),
        as.double(trip_terminals_df$latitude),
        as.double(trip_terminals_df$longitude),
        as.character(trip_terminals_df$terminal_id),
        buffer_radius,
        0,
        !projected
      )
    ]
    terminal_gps_dt <- joined_dt[!is.na(bus_stop)]
  } else if (backend == "rust") {
    # Match using Rust helper
    joined_dt <- data.table::copy(cleaned_gps_dt)
    joined_dt[,
      bus_stop := match_points_to_buffers_rust(
        as.double(latitude),
        as.double(longitude),
        as.double(trip_terminals_df$latitude),
        as.double(trip_terminals_df$longitude),
        as.character(trip_terminals_df$terminal_id),
        buffer_radius,
        0,
        !projected
      )
    ]
    terminal_gps_dt <- joined_dt[!is.na(bus_stop)]
  } else {
    # Convert raw GPS data to sf and project (or use already projected)
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop(
        "The 'sf' package is required to run the 'pure_r' backend. Please install it using: install.packages('sf')"
      )
    }
    if (projected) {
      gps_sf <- sf::st_as_sf(
        cleaned_gps_dt,
        coords = c("longitude", "latitude"),
        crs = if (is.null(projected_crs)) 32644 else projected_crs,
        remove = FALSE
      )
      terminals_sf <- sf::st_as_sf(
        trip_terminals_df,
        coords = c("longitude", "latitude"),
        crs = if (is.null(projected_crs)) 32644 else projected_crs
      )
    } else {
      gps_sf <- sf::st_as_sf(
        cleaned_gps_dt,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      )
      gps_sf <- sf::st_transform(gps_sf, projected_crs)

      # Convert terminals to sf, project, and buffer
      terminals_sf <- sf::st_as_sf(
        trip_terminals_df,
        coords = c("longitude", "latitude"),
        crs = 4326
      )
      terminals_sf <- sf::st_transform(terminals_sf, projected_crs)
    }
    terminals_buffer <- sf::st_buffer(terminals_sf, buffer_radius)

    # Spatial join to find points within terminal buffers
    gps_joined <- sf::st_join(
      gps_sf,
      terminals_buffer["terminal_id"],
      join = sf::st_intersects
    )

    # Convert back to data.table
    joined_dt <- data.table::as.data.table(gps_joined)

    # Keep only the first terminal match if a point overlaps multiple buffers
    joined_dt <- unique(joined_dt, by = "id")

    # Filter records within terminal buffers
    terminal_gps_dt <- joined_dt[!is.na(terminal_id)]
    data.table::setnames(terminal_gps_dt, "terminal_id", "bus_stop")
  }

  if (nrow(terminal_gps_dt) == 0L) {
    terminal_gps_dt[, trip_id := integer()]
    return(terminal_gps_dt)
  }

  # Group contiguous terminal records for the same device, terminal, and date
  terminal_gps_dt[,
    grouped_terminals := data.table::rleid(vehicle_id, bus_stop, date)
  ]

  # Find indices of min (entry) and max (exit) times in each group
  min_indices <- terminal_gps_dt[,
    .I[which.min(timestamp)],
    by = grouped_terminals
  ]$V1
  max_indices <- terminal_gps_dt[,
    .I[which.max(timestamp)],
    by = grouped_terminals
  ]$V1

  # Assign entry/exit markers
  terminal_gps_dt[, entry_exit := NA_character_]
  terminal_gps_dt[max_indices, entry_exit := "0"] # Exit
  terminal_gps_dt[min_indices, entry_exit := "1"] # Entry

  # Filter down to entries and exits
  trip_terminals_gps_dt <- terminal_gps_dt[!is.na(entry_exit)]

  # Sort before pairing
  data.table::setkeyv(
    trip_terminals_gps_dt,
    c("vehicle_id", "timestamp")
  )

  # Pair terminal events within driving sessions, not calendar days: a
  # session breaks when the vehicle goes unseen for longer than session_gap,
  # so trips crossing midnight stay intact while overnight parking still
  # separates one day's events from the next. The backends only test key
  # equality, so the session key travels through their 'dates' parameter.
  trip_terminals_gps_dt[,
    pairing_session := cumsum(
      c(0, diff(as.numeric(timestamp))) > session_gap
    ),
    by = vehicle_id
  ]

  session_keys <- as.character(trip_terminals_gps_dt$pairing_session)
  device_keys <- as.character(trip_terminals_gps_dt$vehicle_id)
  # Assign trip IDs depending on backend
  if (backend == "rcpp") {
    trip_terminals_gps_dt[,
      trip_id := assign_trip_ids_cpp(
        as.character(bus_stop),
        session_keys,
        device_keys
      )
    ]
  } else if (backend == "rust") {
    trip_terminals_gps_dt[,
      trip_id := assign_trip_ids_rust(
        as.character(bus_stop),
        session_keys,
        device_keys
      )
    ]
  } else {
    trip_terminals_gps_dt[,
      trip_id := assign_trip_ids_pure_r(
        as.character(bus_stop),
        session_keys,
        device_keys
      )
    ]
  }
  trip_terminals_gps_dt[, pairing_session := NULL]

  # Filter out unmatched records
  trips_dt <- trip_terminals_gps_dt[trip_id > 0]

  return(trips_dt)
}

#' Extract Trip Features
#'
#' Computes trip features such as duration, start/end times, direction, and day of week.
#'
#' @param trips_dt A data.table returned by \code{extract_trips_r}.
#' @return A data.table containing one row per trip with calculated features,
#'   including ordered factor day_of_week and logical is_weekday.
#' @importFrom data.table data.table setkeyv
#' @noRd
extract_trip_features_r <- function(
  trips_dt,
  terminal_ids = NULL,
  direction_levels = NULL
) {
  if (nrow(trips_dt) == 0L) {
    return(empty_trip_features(trips_dt$vehicle_id))
  }

  # Sort to ensure start and end are sequential
  data.table::setkeyv(trips_dt, c("trip_id", "timestamp"))

  starts <- trips_dt[seq(1, .N, by = 2)]
  ends <- trips_dt[seq(2, .N, by = 2)]

  if (!is.null(direction_levels) && "rt_direction" %in% names(starts)) {
    # Data-driven direction: map the supplied direction value onto 1..K via
    # the shared level set (no terminal assumption).
    direction_vec <- match(
      as.character(starts$rt_direction),
      as.character(direction_levels)
    )
  } else {
    # Classic path: direction is which of the two terminals the trip started at.
    terminals <- if (is.null(terminal_ids)) {
      unique(as.character(starts$bus_stop))
    } else {
      unique(as.character(terminal_ids))
    }
    if (length(terminals) != 2L) {
      stop(
        "Trip extraction requires exactly two distinct terminal IDs.",
        call. = FALSE
      )
    }
    direction_vec <- ifelse(
      starts$bus_stop == terminals[1],
      1L,
      ifelse(
        length(terminals) > 1 & starts$bus_stop == terminals[2],
        2L,
        NA_integer_
      )
    )
  }

  # Official caller-supplied trip identity (fast path only); NA otherwise.
  provided_trip_id <- if ("rt_trip_value" %in% names(starts)) {
    as.character(starts$rt_trip_value)
  } else {
    NA_character_
  }

  trip_features_dt <- data.table::data.table(
    trip_id = starts$trip_id,
    vehicle_id = starts$vehicle_id,
    date = starts$date,
    start_terminal = starts$bus_stop,
    end_terminal = ends$bus_stop,
    direction = direction_vec,
    start_time = starts$timestamp,
    end_time = ends$timestamp,
    provided_trip_id = provided_trip_id
  )

  # Duration in minutes
  trip_features_dt[,
    duration_in_mins := as.numeric(difftime(
      ends$timestamp,
      starts$timestamp,
      units = "mins"
    ))
  ]

  weekday_features <- make_weekday_features(trip_features_dt$date)
  trip_features_dt[, day_of_week := weekday_features$day_of_week]

  # Hour of day
  trip_features_dt[, hour_of_day := as.integer(format(starts$timestamp, "%H"))]
  trip_features_dt[, is_weekday := weekday_features$is_weekday]

  # Additive, versioned C5 columns (orientation_id / orientation_status /
  # orientation_confidence / pattern_ref + trips-only anchor refs). Appended
  # after every pre-existing column and left in the detector-off empty state:
  # no orientation detector is authorized yet, so `orientation_id` stays NA and
  # `direction` remains the legacy 1..K field downstream reads. See
  # private/terminal-detection-spike.md §3.
  add_trip_orientation_cols(trip_features_dt)

  return(trip_features_dt)
}
