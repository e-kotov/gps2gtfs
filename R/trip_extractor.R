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
#' same trip value per vehicle and date becomes one trip; its first and last
#' pings are treated as terminal entry/exit and matched to the nearest
#' terminal so that direction semantics stay identical to the spatial path.
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
  projected = NULL
) {
  if (!trip_col %in% names(cleaned_gps_dt)) {
    stop(
      "'trip_col' column '",
      trip_col,
      "' not found in the GPS data.",
      call. = FALSE
    )
  }
  if (is.null(projected)) {
    projected <- attr(cleaned_gps_dt, "projected")
    if (is.null(projected)) {
      projected <- FALSE
    }
  }

  norm_terminals <- normalize_coordinates(
    trip_terminals_df,
    projected = projected,
    name = "trip_terminals_df"
  )
  terminals <- norm_terminals$dt
  validate_required_columns(terminals, "terminal_id", "trip_terminals_df")
  validate_identifiers(terminals$terminal_id, "trip_terminals_df 'terminal_id'")

  empty_result <- function() {
    result <- data.table::copy(cleaned_gps_dt[0])
    result[, bus_stop := character()]
    result[, trip_id := integer()]
    result
  }
  if (nrow(cleaned_gps_dt) == 0L || nrow(terminals) == 0L) {
    return(empty_result())
  }

  dt <- data.table::copy(cleaned_gps_dt)
  dt[, rt_trip_value := as.character(dt[[trip_col]])]
  seg_dt <- dt[!is.na(rt_trip_value) & nzchar(trimws(rt_trip_value))]
  if (nrow(seg_dt) == 0L) {
    message(
      "[INFO] Column '",
      trip_col,
      "' contains no usable trip identities; no trips extracted."
    )
    return(empty_result())
  }

  data.table::setkeyv(seg_dt, c("vehicle_id", "date", "time_str"))
  seg_dt[, trip_id := data.table::rleid(vehicle_id, date, rt_trip_value)]
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

  # Renumber to consecutive integers, then keep first/last ping per trip
  seg_dt[, trip_id := data.table::rleid(trip_id)]
  bounds <- seg_dt[, .SD[c(1L, .N)], by = trip_id]

  bounds[,
    bus_stop := nearest_terminal_id(
      as.double(latitude),
      as.double(longitude),
      terminals,
      projected
    )
  ]
  bounds[, c("rt_trip_value", "seg_n") := NULL]
  bounds[]
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
  projected = NULL
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
    c("vehicle_id", "date", "timestamp")
  )

  device_keys <- as.character(trip_terminals_gps_dt$vehicle_id)
  # Assign trip IDs depending on backend
  if (backend == "rcpp") {
    trip_terminals_gps_dt[,
      trip_id := assign_trip_ids_cpp(
        as.character(bus_stop),
        as.character(date),
        device_keys
      )
    ]
  } else if (backend == "rust") {
    trip_terminals_gps_dt[,
      trip_id := assign_trip_ids_rust(
        as.character(bus_stop),
        as.character(date),
        device_keys
      )
    ]
  } else {
    trip_terminals_gps_dt[,
      trip_id := assign_trip_ids_pure_r(
        as.character(bus_stop),
        as.character(date),
        device_keys
      )
    ]
  }

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
extract_trip_features_r <- function(trips_dt, terminal_ids = NULL) {
  if (nrow(trips_dt) == 0L) {
    return(empty_trip_features(trips_dt$vehicle_id))
  }

  # Sort to ensure start and end are sequential
  data.table::setkeyv(trips_dt, c("trip_id", "timestamp"))

  starts <- trips_dt[seq(1, .N, by = 2)]
  ends <- trips_dt[seq(2, .N, by = 2)]

  # Find unique start terminals for direction mapping
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

  trip_features_dt <- data.table::data.table(
    trip_id = starts$trip_id,
    vehicle_id = starts$vehicle_id,
    date = starts$date,
    start_terminal = starts$bus_stop,
    end_terminal = ends$bus_stop,
    direction = direction_vec,
    start_time = starts$time_str,
    end_time = ends$time_str
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

  return(trip_features_dt)
}
