#' Pure R Trip ID Propagation
#'
#' Sequential trip ID propagation loop in pure R.
#'
#' @param trip_ids Integer vector of initial trip IDs.
#' @return Integer vector of propagated trip IDs.
#' @noRd
propagate_trip_ids_pure_r <- function(trip_ids) {
  n <- length(trip_ids)
  result <- integer(n)
  current_trip <- 0L

  for (i in seq_len(n)) {
    val <- trip_ids[i]
    if (val != 0L) {
      if (current_trip == 0L) {
        # Start of a trip
        current_trip <- val
      } else if (val == current_trip) {
        # End of the current trip
        result[i] <- current_trip
        current_trip <- 0L
        next
      }
    }
    result[i] <- current_trip
  }
  return(result)
}

#' Prepare Trajectory Data Frame
#'
#' Propagates trip IDs to intermediate GPS points between terminal entry/exit.
#'
#' @param cleaned_gps_dt A data.table containing cleaned GPS data.
#' @param trips_dt A data.table containing terminal records with assigned trip IDs.
#' @param trip_features_dt A data.table containing trip-level features (with direction).
#' @param backend Character. The backend to use: \code{"rcpp"}, \code{"pure_r"}, or \code{"rust"}. Default is \code{"rust"}.
#' @return A data.table containing GPS records with propagated trip IDs and directions.
#' @importFrom data.table as.data.table setkeyv
#' @noRd
prepare_trajectory_r <- function(
  cleaned_gps_dt,
  trips_dt,
  trip_features_dt,
  backend = "rust"
) {
  backend <- match.arg(backend, c("rcpp", "pure_r", "rust"))
  if (
    nrow(cleaned_gps_dt) == 0L ||
      nrow(trips_dt) == 0L ||
      nrow(trip_features_dt) == 0L
  ) {
    return(empty_trajectory(cleaned_gps_dt))
  }

  # Merge trip_id from trips_dt into cleaned_gps_dt. A passthrough column
  # named trip_id (e.g. the GTFS-RT annotation) would collide with the
  # pipeline's internal trip numbering; drop it here — it has already been
  # consumed by segmentation at this point.
  if ("trip_id" %in% names(cleaned_gps_dt)) {
    cleaned_gps_dt <- data.table::copy(cleaned_gps_dt)
    cleaned_gps_dt[, trip_id := NULL]
  }
  merged_dt <- merge(
    cleaned_gps_dt,
    trips_dt[, .(id, trip_id)],
    by = "id",
    all.x = TRUE
  )
  merged_dt[is.na(trip_id), trip_id := 0L]

  # Sort before propagation
  data.table::setkeyv(merged_dt, c("vehicle_id", "date", "timestamp"))

  # Propagate trip IDs depending on backend
  if (backend == "rcpp") {
    merged_dt[, trip_id := propagate_trip_ids_cpp(trip_id)]
  } else if (backend == "rust") {
    merged_dt[, trip_id := propagate_trip_ids_rust(as.integer(trip_id))]
  } else {
    merged_dt[, trip_id := propagate_trip_ids_pure_r(trip_id)]
  }

  # Remove records that are not part of any trip
  trajectory_dt <- merged_dt[trip_id > 0]

  # Merge direction from trip_features_dt
  trajectory_dt <- merge(
    trajectory_dt,
    trip_features_dt[, .(trip_id, direction)],
    by = "trip_id",
    all.x = TRUE
  )

  return(trajectory_dt)
}

resolve_stop_directions <- function(
  stops_df,
  terminal_ids,
  stop_direction_map = NULL,
  direction_levels = NULL
) {
  validate_required_columns(stops_df, c("stop_id", "direction"), "stops_df")
  validate_identifiers(stops_df$stop_id, "stops_df 'stop_id'")
  validate_identifiers(stops_df$direction, "stops_df 'direction'")

  # Data-driven direction: the stop 'direction' labels are direction ids
  # (e.g. GTFS-RT direction_id), mapped onto 1..K by the shared level set.
  # No terminals, and any number of groups (2 for bidirectional, 1 for a
  # one-way service, more for multi-branch).
  if (!is.null(direction_levels)) {
    levels_chr <- as.character(direction_levels)
    dir <- match(as.character(stops_df$direction), levels_chr)
    if (anyNA(dir)) {
      stop(
        "stops_df 'direction' contains value(s) not in the supplied ",
        "direction set (",
        paste(levels_chr, collapse = ", "),
        "): ",
        paste(unique(setdiff(as.character(stops_df$direction), levels_chr)),
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    return(dir)
  }

  terminal_ids <- unique(as.character(terminal_ids))
  if (length(terminal_ids) != 2L) {
    stop(
      "Stop direction mapping requires exactly two distinct terminal IDs.",
      call. = FALSE
    )
  }

  labels <- unique(as.character(stops_df$direction))
  if (is.null(stop_direction_map)) {
    if (length(labels) != 2L) {
      stop(
        "stops_df must contain exactly two usable direction groups when ",
        "'stop_direction_map' is omitted.",
        call. = FALSE
      )
    }
    if (setequal(labels, terminal_ids)) {
      # Labels are terminal ids themselves (e.g. from g2g_stops_from_gtfs());
      # map each onto itself instead of pairing by order of appearance.
      stop_direction_map <- stats::setNames(labels, labels)
    } else {
      stop_direction_map <- stats::setNames(terminal_ids, labels)
    }
  } else {
    if (
      !is.character(stop_direction_map) ||
        is.null(names(stop_direction_map)) ||
        any(!nzchar(names(stop_direction_map))) ||
        anyDuplicated(names(stop_direction_map))
    ) {
      stop(
        "'stop_direction_map' must be a named character vector with unique, non-empty names.",
        call. = FALSE
      )
    }
    missing_labels <- setdiff(labels, names(stop_direction_map))
    if (length(missing_labels) > 0L) {
      stop(
        "'stop_direction_map' does not cover direction labels: ",
        paste(missing_labels, collapse = ", "),
        call. = FALSE
      )
    }
    unknown_terminals <- setdiff(
      unique(unname(stop_direction_map)),
      terminal_ids
    )
    if (length(unknown_terminals) > 0L) {
      stop(
        "'stop_direction_map' references unknown terminal IDs: ",
        paste(unknown_terminals, collapse = ", "),
        call. = FALSE
      )
    }
  }

  direction <- match(
    unname(stop_direction_map[as.character(stops_df$direction)]),
    terminal_ids
  )
  if (anyNA(direction) || !setequal(unique(direction), c(1L, 2L))) {
    stop(
      "Stop directions must resolve to exactly two terminal-based direction groups.",
      call. = FALSE
    )
  }
  direction
}

#' Extract Stop Times and Dwell Times
#'
#' Performs spatial joins to match trajectory points to stop buffers, filters out terminal stops,
#' and estimates arrival, departure, and dwell times.
#'
#' @param trajectory_dt A data.table containing trajectory GPS records (output of \code{prepare_trajectory_r}).
#' @param stops_df A data.frame containing stop coordinates and directions.
#' @param trip_terminals_df A data.frame containing terminal coordinates (to filter them out).
#' @param buffer_radius Numeric. Stop buffer radius (in meters).
#' @param extended_buffer_radius Numeric. Extended stop buffer radius (in meters).
#' @param projected_crs Numeric. The EPSG code of a projected coordinate system to use (default: 5234).
#' @param backend Character. The backend to use: \code{"rcpp"}, \code{"pure_r"}, or \code{"rust"}. Default is \code{"rust"}.
#' @return A data.table with stop arrival/departure times, dwell times, ordered
#'   factor day_of_week, and logical is_weekday.
#' @importFrom data.table as.data.table setkeyv rleid
#' @noRd
extract_stops_r <- function(
  trajectory_dt,
  stops_df,
  trip_terminals_df,
  buffer_radius,
  extended_buffer_radius,
  projected_crs = 5234,
  backend = "rust",
  projected = NULL,
  stop_direction_map = NULL,
  terminal_ids = NULL,
  direction_levels = NULL
) {
  backend <- match.arg(backend, c("rcpp", "pure_r", "rust"))
  validate_positive_radius(buffer_radius, "buffer_radius")
  validate_positive_radius(extended_buffer_radius, "extended_buffer_radius")

  if (is.null(projected)) {
    projected <- attr(trajectory_dt, "projected")
    if (is.null(projected)) {
      projected <- FALSE
    }
  }

  # Normalize and validate stops
  norm_stops <- normalize_coordinates(
    stops_df,
    projected = projected,
    name = "stops_df"
  )
  stops_df <- norm_stops$dt

  # Terminals are optional in data-driven direction mode: when direction comes
  # from the data (direction_levels) they are only used to drop terminal points
  # if supplied. In the classic terminal mode they define the two directions.
  have_terminals <- !is.null(trip_terminals_df) &&
    nrow(data.table::as.data.table(trip_terminals_df)) > 0L
  if (have_terminals) {
    trip_terminals_df <- normalize_coordinates(
      trip_terminals_df,
      projected = projected,
      name = "trip_terminals_df"
    )$dt
    validate_required_columns(
      trip_terminals_df,
      "terminal_id",
      "trip_terminals_df"
    )
    validate_identifiers(
      trip_terminals_df$terminal_id,
      "trip_terminals_df 'terminal_id'"
    )
    if (is.null(terminal_ids)) {
      terminal_ids <- unique(as.character(trip_terminals_df$terminal_id))
    }
  } else {
    if (is.null(direction_levels)) {
      stop(
        "'trip_terminals_df' is required unless 'direction_levels' is ",
        "supplied (data-driven direction mode).",
        call. = FALSE
      )
    }
    trip_terminals_df <- data.table::data.table(terminal_id = character(0))
  }
  if (nrow(stops_df) == 0L) {
    validate_required_columns(stops_df, c("stop_id", "direction"), "stops_df")
    return(empty_stop_times(trajectory_dt$vehicle_id))
  }
  stops_df[["internal_direction"]] <- resolve_stop_directions(
    stops_df,
    terminal_ids,
    stop_direction_map,
    direction_levels = direction_levels
  )

  if (nrow(trajectory_dt) == 0L) {
    return(empty_stop_times(trajectory_dt$vehicle_id))
  }

  if (backend == "rcpp" || backend == "rust") {
    # Match within each direction group. Groups are the distinct internal
    # direction ids present (2 for a classic bidirectional route mapped onto
    # two terminals, but any number when direction is supplied from the data,
    # e.g. GTFS-RT direction_id, so short-turn/variant routes with >2 terminals
    # and one-way services still work).
    matcher <- if (backend == "rcpp") {
      match_points_to_buffers_cpp
    } else {
      match_points_to_buffers_rust
    }
    groups <- sort(unique(stops_df$internal_direction))
    matched <- lapply(groups, function(g) {
      traj_g <- data.table::copy(trajectory_dt[direction == g])
      stops_g <- stops_df[stops_df$internal_direction == g]
      if (nrow(traj_g) > 0 && nrow(stops_g) > 0) {
        traj_g[,
          bus_stop := matcher(
            as.double(latitude),
            as.double(longitude),
            as.double(stops_g$latitude),
            as.double(stops_g$longitude),
            as.character(stops_g$stop_id),
            buffer_radius,
            extended_buffer_radius,
            !projected
          )
        ]
      } else if (nrow(traj_g) > 0) {
        traj_g[, bus_stop := NA_character_]
      } else {
        traj_g[, bus_stop := character(0)]
      }
      traj_g
    })

    # Combine results
    stops_dt <- data.table::rbindlist(matched)
    stops_dt <- stops_dt[!is.na(bus_stop)]
  } else {
    # Planar sf matching (Pure R)
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop(
        "The 'sf' package is required to run the 'pure_r' backend. Please install it using: install.packages('sf')"
      )
    }
    if (projected) {
      trajectory_sf <- sf::st_as_sf(
        trajectory_dt,
        coords = c("longitude", "latitude"),
        crs = if (is.null(projected_crs)) 32644 else projected_crs,
        remove = FALSE
      )
      stops_sf <- sf::st_as_sf(
        stops_df,
        coords = c("longitude", "latitude"),
        crs = if (is.null(projected_crs)) 32644 else projected_crs
      )
    } else {
      trajectory_sf <- sf::st_as_sf(
        trajectory_dt,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      )
      trajectory_sf <- sf::st_transform(trajectory_sf, projected_crs)

      # Convert stops to sf and project
      stops_sf <- sf::st_as_sf(
        stops_df,
        coords = c("longitude", "latitude"),
        crs = 4326
      )
      stops_sf <- sf::st_transform(stops_sf, projected_crs)
    }
    # Helper to perform spatial matching for one direction group
    match_direction <- function(traj_sf, std_buffer, ext_buffer) {
      if (nrow(traj_sf) == 0) {
        return(data.table::data.table())
      }

      # Standard join
      joined_std <- sf::st_join(
        traj_sf,
        std_buffer["stop_id"],
        join = sf::st_intersects
      )
      joined_std <- data.table::as.data.table(joined_std)
      joined_std <- unique(joined_std, by = "id")

      # Extended join for unmatched
      unmatched_ids <- joined_std[is.na(stop_id), id]
      if (length(unmatched_ids) > 0) {
        unmatched_sf <- traj_sf[traj_sf$id %in% unmatched_ids, ]
        joined_ext <- sf::st_join(
          unmatched_sf,
          ext_buffer["stop_id"],
          join = sf::st_intersects
        )
        joined_ext <- data.table::as.data.table(joined_ext)
        joined_ext <- unique(joined_ext, by = "id")

        # Update matched stop_id
        data.table::setkey(joined_std, id)
        data.table::setkey(joined_ext, id)
        joined_std[joined_ext, stop_id := i.stop_id]
      }

      return(joined_std[!is.na(stop_id)])
    }

    # Match within each direction group present (any number of groups)
    groups <- sort(unique(stops_df$internal_direction))
    matched <- lapply(groups, function(g) {
      stops_g <- stops_sf[stops_sf$internal_direction == g, ]
      traj_g <- trajectory_sf[trajectory_sf$direction == g, ]
      if (nrow(stops_g) == 0L || nrow(traj_g) == 0L) {
        return(data.table::data.table())
      }
      match_direction(
        traj_g,
        sf::st_buffer(stops_g, buffer_radius),
        sf::st_buffer(stops_g, extended_buffer_radius)
      )
    })

    # Combine results
    stops_dt <- data.table::rbindlist(matched)
    data.table::setnames(stops_dt, "stop_id", "bus_stop")
  }

  # Sort to ensure chronological order per trip/device stop visits
  data.table::setkeyv(stops_dt, c("vehicle_id", "trip_id", "timestamp"))

  # Drop terminal points
  terminals_to_drop <- unique(trip_terminals_df$terminal_id)
  stops_dt <- stops_dt[!(bus_stop %in% terminals_to_drop)]

  if (nrow(stops_dt) == 0) {
    return(empty_stop_times(trajectory_dt$vehicle_id))
  }

  # Group contiguous stop visits by device, trip, and stop
  stops_dt[, grouped_ends := data.table::rleid(vehicle_id, trip_id, bus_stop)]

  # Estimate arrival, departure, and dwell times
  stop_times_dt <- stops_dt[,
    {
      zero_idx <- which(speed == 0)

      if (length(zero_idx) > 0) {
        arr_time <- min(timestamp[zero_idx])
        buf_leave_time <- max(timestamp)
        rough_dep_time <- max(timestamp[zero_idx])

        if (
          as.numeric(difftime(buf_leave_time, rough_dep_time, units = "secs")) >
            15
        ) {
          dep_time <- rough_dep_time + 15
        } else {
          dep_time <- buf_leave_time
        }
      } else {
        arr_time <- min(timestamp)
        dep_time <- arr_time
      }

      # Absolute POSIXct times: "HH:MM:SS" strings wrap at midnight, which
      # corrupts post-midnight stops of overnight trips downstream.
      .(
        trip_id = trip_id[1],
        vehicle_id = vehicle_id[1],
        date = date[1],
        direction = direction[1],
        bus_stop = bus_stop[1],
        arrival_time = arr_time,
        departure_time = dep_time,
        dwell_time_in_seconds = as.numeric(difftime(
          dep_time,
          arr_time,
          units = "secs"
        ))
      )
    },
    by = grouped_ends
  ]

  # Add derived features
  weekday_features <- make_weekday_features(stop_times_dt$date)
  stop_times_dt[, day_of_week := weekday_features$day_of_week]
  stop_times_dt[, hour_of_day := as.integer(format(arrival_time, "%H"))]
  stop_times_dt[, is_weekday := weekday_features$is_weekday]

  # Clean columns
  stop_times_dt[, grouped_ends := NULL]

  data.table::setnames(stop_times_dt, "bus_stop", "stop_id")
  stop_times_dt[, stop_id := as.character(stop_id)]
  stop_times_dt
}
