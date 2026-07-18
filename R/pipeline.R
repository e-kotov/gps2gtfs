#' Check if Rcpp Backend is Available
#'
#' @return Logical. \code{TRUE} if the Rcpp backend is compiled and loaded, \code{FALSE} otherwise.
#' @noRd
is_rcpp_available <- function() {
  "gps2gtfs" %in% names(getLoadedDLLs())
}

#' Check if Rust Backend is Compiled/Available
#'
#' @return Logical. \code{TRUE} if the Rust backend is compiled and available, \code{FALSE} otherwise.
#' @noRd
is_rust_available <- function() {
  if (is_rcpp_available()) {
    is_rust_compiled_cpp()
  } else {
    FALSE
  }
}

#' Extract Trips from GPS Trajectories
#'
#' Reads raw GPS and terminal CSV files, extracts trips, and optionally writes results to a CSV.
#'
#' @param gps_data A data.frame or path to the raw GPS CSV.
#' @param terminals_data A data.frame or path to the terminal coordinates CSV.
#' @param terminals_buffer_radius Numeric. Buffer radius for terminals (in meters).
#' @param output_path Character. Optional path to write output trip features as CSV. Default is \code{NULL} (no file written).
#' @param projected_crs Numeric. The EPSG code of a projected coordinate system
#'   used for metric distance calculations. Required when
#'   \code{projected = TRUE}; otherwise a suitable UTM projection is selected
#'   when omitted.
#' @param backend Character. The backend to use: \code{"auto"}, \code{"rust"},
#'   \code{"rcpp"}, or \code{"pure_r"}. Automatic selection prefers Rust, then
#'   Rcpp, then pure R. Explicit unavailable backends produce an error.
#' @param projected Logical. Whether plain-table coordinates are already
#'   projected. Out-of-bounds coordinates require explicit \code{TRUE}.
#' @param vehicle_col Character. Name of the vehicle identifier column in
#'   \code{gps_data}. Default \code{"vehicle_id"} (GTFS-Realtime convention).
#' @param time_col Character. Name of the timestamp column in \code{gps_data}.
#'   Default \code{"timestamp"} (GTFS-Realtime convention).
#' @param trip_col Character. Optional name of a column holding supplied trip
#'   identities (e.g. \code{"trip_id"} from GTFS-Realtime Vehicle Positions).
#'   When given, trips are segmented by those identities (fast path) instead
#'   of inferred from terminal-buffer crossings; the first/last ping of each
#'   segment is matched to the nearest terminal for direction assignment.
#'   Default \code{NULL} (spatial inference).
#' @return A data.table containing extracted trip features.
#' @examples
#' \donttest{
#' data(g2g_data_gps)
#' data(g2g_data_terminals)
#' trips <- g2g_extract_trips(
#'   gps_data = g2g_data_gps,
#'   terminals_data = g2g_data_terminals,
#'   terminals_buffer_radius = 50
#' )
#' }
#' @importFrom data.table fread fwrite as.data.table
#' @importFrom stats median
#' @export
g2g_extract_trips <- function(
  gps_data,
  terminals_data,
  terminals_buffer_radius,
  output_path = NULL,
  projected_crs = NULL,
  backend = "auto",
  projected = NULL,
  vehicle_col = "vehicle_id",
  time_col = "timestamp",
  trip_col = NULL
) {
  backend <- resolve_backend(backend)
  validate_positive_radius(terminals_buffer_radius, "terminals_buffer_radius")
  validate_projected_crs(projected_crs)
  if (isTRUE(projected) && is.null(projected_crs)) {
    stop("'projected_crs' is required when 'projected = TRUE'.", call. = FALSE)
  }
  message("Starting Pipeline for extracting Trip Data using backend: ", backend)

  raw_gps_df <- if (is.character(gps_data) && length(gps_data) == 1L) data.table::fread(gps_data) else data.table::as.data.table(gps_data)
  trip_terminals_df <- if (is.character(terminals_data) && length(terminals_data) == 1L) data.table::fread(terminals_data) else data.table::as.data.table(terminals_data)
  cleaned <- g2g_clean_gps(
    raw_gps_df,
    projected = projected,
    vehicle_col = vehicle_col,
    time_col = time_col
  )
  projected <- attr(cleaned, "projected")
  if (projected && is.null(projected_crs)) {
    stop(
      "'projected_crs' is required for projected coordinates.",
      call. = FALSE
    )
  }

  terminals <- normalize_coordinates(
    trip_terminals_df,
    projected = projected,
    name = "trip_terminals_df"
  )$dt
  validate_required_columns(terminals, "terminal_id", "trip_terminals_df")
  validate_identifiers(terminals$terminal_id, "trip_terminals_df 'terminal_id'")

  if (nrow(cleaned) == 0L) {
    result <- set_backend(empty_trip_features(cleaned$vehicle_id), backend)
    if (!is.null(output_path)) {
      data.table::fwrite(result, output_path)
    }
    return(result)
  }

  if (is.null(projected_crs) && !projected) {
    med_lon <- stats::median(cleaned$longitude, na.rm = TRUE)
    med_lat <- stats::median(cleaned$latitude, na.rm = TRUE)
    zone <- floor((med_lon + 180) / 6) + 1
    is_northern <- med_lat >= 0
    projected_crs <- if (is_northern) (32600 + zone) else (32700 + zone)
    message(
      "[INFO] Auto-detected local UTM projection EPSG:",
      projected_crs,
      " (Zone ",
      zone,
      if (is_northern) "N" else "S",
      ")"
    )
  }

  if (!is.null(trip_col)) {
    message(
      "[INFO] Using supplied trip identities from column '",
      trip_col,
      "' (fast path)."
    )
    trips <- extract_trips_from_ids_r(
      cleaned,
      terminals,
      trip_col,
      projected = projected
    )
  } else {
    trips <- extract_trips_r(
      cleaned,
      terminals,
      terminals_buffer_radius,
      projected_crs = projected_crs,
      backend = backend,
      projected = projected
    )
  }
  trip_features <- extract_trip_features_r(
    trips,
    unique(as.character(terminals$terminal_id))
  )
  trip_features <- set_backend(trip_features, backend)

  if (!is.null(output_path)) {
    data.table::fwrite(trip_features, output_path)
    message("Pipeline finished successfully! Output saved to ", output_path)
  } else {
    message("Pipeline finished successfully!")
  }
  trip_features
}

#' Extract Trips and Stop Times from GPS Trajectories
#'
#' Reads raw GPS, terminal, and stop CSV files, extracts trips and stop times, and optionally writes results.
#'
#' @param gps_data A data.frame or path to the raw GPS CSV.
#' @param terminals_data A data.frame or path to the terminal coordinates CSV.
#' @param stops_data A data.frame or path to the bus stops coordinates CSV.
#' @param terminals_buffer_radius Numeric. Buffer radius for terminals (in meters).
#' @param stops_buffer_radius Numeric. Buffer radius for bus stops (in meters).
#' @param stops_extended_buffer_radius Numeric. Extended buffer radius for bus stops (in meters).
#' @param output_trips_path Character. Optional path to write output trip features as CSV. Default is \code{NULL} (no file written).
#' @param output_stops_path Character. Optional path to write output stop times as CSV. Default is \code{NULL} (no file written).
#' @param projected_crs Numeric. The EPSG code of a projected coordinate system
#'   used for metric distance calculations. Required when
#'   \code{projected = TRUE}; otherwise a suitable UTM projection is selected
#'   when omitted.
#' @param backend Character. The backend to use: \code{"auto"}, \code{"rust"},
#'   \code{"rcpp"}, or \code{"pure_r"}. Automatic selection prefers Rust, then
#'   Rcpp, then pure R. Explicit unavailable backends produce an error.
#' @param projected Logical. Whether plain-table coordinates are already
#'   projected. Out-of-bounds coordinates require explicit \code{TRUE}.
#' @param stop_direction_map Optional named character vector mapping each raw
#'   stop-direction label to its starting terminal ID.
#' @param vehicle_col Character. Name of the vehicle identifier column in
#'   \code{gps_data}. Default \code{"vehicle_id"} (GTFS-Realtime convention).
#' @param time_col Character. Name of the timestamp column in \code{gps_data}.
#'   Default \code{"timestamp"} (GTFS-Realtime convention).
#' @param trip_col Character. Optional name of a column holding supplied trip
#'   identities (e.g. \code{"trip_id"} from GTFS-Realtime Vehicle Positions).
#'   When given, trips are segmented by those identities (fast path) instead
#'   of inferred from terminal-buffer crossings; the first/last ping of each
#'   segment is matched to the nearest terminal for direction assignment.
#'   Default \code{NULL} (spatial inference).
#' @param return_trajectory Logical. Also return the ping-level trajectory
#'   (cleaned GPS records with assigned \code{trip_id} and \code{direction})
#'   as a \code{trajectory} element — the input for
#'   \code{\link{g2g_shapes_from_trips}}. Default \code{FALSE}.
#' @return A list containing two data.tables: \code{trips} and \code{stop_times} (with columns matching GTFS standard naming), plus \code{trajectory} when \code{return_trajectory = TRUE}.
#' @examples
#' \donttest{
#' data(g2g_data_gps)
#' data(g2g_data_terminals)
#' data(g2g_data_stops)
#' result <- g2g_extract_trips_and_stop_times(
#'   gps_data = g2g_data_gps,
#'   terminals_data = g2g_data_terminals,
#'   stops_data = g2g_data_stops,
#'   terminals_buffer_radius = 50,
#'   stops_buffer_radius = 30,
#'   stops_extended_buffer_radius = 50
#' )
#' head(result$trips)
#' head(result$stop_times)
#' }
#' @importFrom data.table fread fwrite as.data.table
#' @importFrom stats median
#' @export
g2g_extract_trips_and_stop_times <- function(
  gps_data,
  terminals_data,
  stops_data,
  terminals_buffer_radius,
  stops_buffer_radius,
  stops_extended_buffer_radius,
  output_trips_path = NULL,
  output_stops_path = NULL,
  projected_crs = NULL,
  backend = "auto",
  projected = NULL,
  stop_direction_map = NULL,
  vehicle_col = "vehicle_id",
  time_col = "timestamp",
  trip_col = NULL,
  return_trajectory = FALSE
) {
  backend <- resolve_backend(backend)
  validate_positive_radius(terminals_buffer_radius, "terminals_buffer_radius")
  validate_positive_radius(stops_buffer_radius, "stops_buffer_radius")
  validate_positive_radius(
    stops_extended_buffer_radius,
    "stops_extended_buffer_radius"
  )
  validate_projected_crs(projected_crs)
  if (isTRUE(projected) && is.null(projected_crs)) {
    stop("'projected_crs' is required when 'projected = TRUE'.", call. = FALSE)
  }
  message(
    "Starting Pipeline for extracting Trip and Bus Stop Data using backend: ",
    backend
  )

  raw_gps_df <- if (is.character(gps_data) && length(gps_data) == 1L) data.table::fread(gps_data) else data.table::as.data.table(gps_data)
  trip_terminals_df <- if (is.character(terminals_data) && length(terminals_data) == 1L) data.table::fread(terminals_data) else data.table::as.data.table(terminals_data)
  stops_df <- if (is.character(stops_data) && length(stops_data) == 1L) data.table::fread(stops_data) else data.table::as.data.table(stops_data)

  cleaned <- g2g_clean_gps(
    raw_gps_df,
    projected = projected,
    vehicle_col = vehicle_col,
    time_col = time_col
  )
  projected <- attr(cleaned, "projected")
  if (projected && is.null(projected_crs)) {
    stop(
      "'projected_crs' is required for projected coordinates.",
      call. = FALSE
    )
  }

  terminals <- normalize_coordinates(
    trip_terminals_df,
    projected = projected,
    name = "trip_terminals_df"
  )$dt
  validate_required_columns(terminals, "terminal_id", "trip_terminals_df")
  validate_identifiers(terminals$terminal_id, "trip_terminals_df 'terminal_id'")
  terminal_ids <- unique(as.character(terminals$terminal_id))

  stops <- normalize_coordinates(
    stops_df,
    projected = projected,
    name = "stops_df"
  )$dt
  if (nrow(stops) > 0L) {
    resolve_stop_directions(stops, terminal_ids, stop_direction_map)
  } else {
    validate_required_columns(stops, c("stop_id", "direction"), "stops_df")
  }

  if (nrow(cleaned) == 0L) {
    result <- list(
      trips = set_backend(empty_trip_features(cleaned$vehicle_id), backend),
      stop_times = empty_stop_times(cleaned$vehicle_id)
    )
    if (isTRUE(return_trajectory)) {
      result$trajectory <- empty_trajectory(cleaned)
    }
    return(set_backend(result, backend))
  }

  if (is.null(projected_crs) && !projected) {
    med_lon <- stats::median(cleaned$longitude, na.rm = TRUE)
    med_lat <- stats::median(cleaned$latitude, na.rm = TRUE)
    zone <- floor((med_lon + 180) / 6) + 1
    is_northern <- med_lat >= 0
    projected_crs <- if (is_northern) (32600 + zone) else (32700 + zone)
    message(
      "[INFO] Auto-detected local UTM projection EPSG:",
      projected_crs,
      " (Zone ",
      zone,
      if (is_northern) "N" else "S",
      ")"
    )
  }

  if (!is.null(trip_col)) {
    message(
      "[INFO] Using supplied trip identities from column '",
      trip_col,
      "' (fast path)."
    )
    trips <- extract_trips_from_ids_r(
      cleaned,
      terminals,
      trip_col,
      projected = projected
    )
  } else {
    trips <- extract_trips_r(
      cleaned,
      terminals,
      terminals_buffer_radius,
      projected_crs = projected_crs,
      backend = backend,
      projected = projected
    )
  }
  trip_features <- extract_trip_features_r(trips, terminal_ids)
  trip_features <- set_backend(trip_features, backend)

  trajectory <- prepare_trajectory_r(
    cleaned,
    trips,
    trip_features,
    backend = backend
  )
  attr(trajectory, "projected") <- projected

  stop_times <- extract_stops_r(
    trajectory,
    stops,
    terminals,
    stops_buffer_radius,
    stops_extended_buffer_radius,
    projected_crs = projected_crs,
    backend = backend,
    projected = projected,
    stop_direction_map = stop_direction_map,
    terminal_ids = terminal_ids
  )

  if ("bus_stop" %in% names(stop_times)) {
    data.table::setnames(stop_times, "bus_stop", "stop_id")
  }

  if (!is.null(output_trips_path)) {
    data.table::fwrite(trip_features, output_trips_path)
  }
  if (!is.null(output_stops_path)) {
    data.table::fwrite(stop_times, output_stops_path)
  }

  if (!is.null(output_trips_path) || !is.null(output_stops_path)) {
    message("Pipeline finished successfully! Outputs saved.")
  } else {
    message("Pipeline finished successfully!")
  }

  result <- list(trips = trip_features, stop_times = stop_times)
  if (isTRUE(return_trajectory)) {
    result$trajectory <- trajectory
  }
  set_backend(result, backend)
}
