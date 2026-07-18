#' Normalize Coordinates to WGS-84 Degrees
#'
#' Internal helper to convert tables/sf objects and detect if coordinates are projected.
#' If the input is an sf object, it extracts the coordinates and handles CRS metadata automatically.
#'
#' @param df A data.frame or sf object.
#' @param projected Logical. Is the data already projected (in meters)? Default is NULL (auto-detect).
#' @param name Character. The name of the dataset (used in error messages).
#' @return A list with `dt` (data.table) and `projected` (logical).
#' @importFrom data.table as.data.table
#' @noRd
normalize_coordinates <- function(df, projected = NULL, name = "dataset") {
  is_sf <- inherits(df, "sf")
  if (is_sf) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop(paste(
        "The 'sf' package is required to process 'sf' objects in",
        name,
        ". Please install it or pass a data.frame/data.table with longitude/latitude columns."
      ))
    }
    if (is.null(projected)) {
      projected <- !sf::st_is_longlat(df)
    }
    coords <- sf::st_coordinates(df)
    df_coords <- sf::st_drop_geometry(df)
    df_coords$longitude <- coords[, 1]
    df_coords$latitude <- coords[, 2]
    df <- df_coords
  }

  dt <- data.table::as.data.table(df)

  validate_required_columns(dt, c("latitude", "longitude"), name)
  if (
    (length(dt$latitude) > 0L && !is.numeric(dt$latitude)) ||
      (length(dt$longitude) > 0L && !is.numeric(dt$longitude))
  ) {
    stop("Coordinates in ", name, " must be numeric.", call. = FALSE)
  }
  dt[, latitude := as.double(latitude)]
  dt[, longitude := as.double(longitude)]
  if (any(!is.finite(dt$latitude)) || any(!is.finite(dt$longitude))) {
    stop(
      "Coordinates in ",
      name,
      " must not contain NA, NaN, or infinite values.",
      call. = FALSE
    )
  }

  outside_bounds <- any(
    dt$latitude < -90 |
      dt$latitude > 90 |
      dt$longitude < -180 |
      dt$longitude > 180
  )

  if (is.null(projected)) {
    if (!is_sf && outside_bounds) {
      stop(
        "Coordinates in ",
        name,
        " are outside valid WGS-84 degree bounds. Set 'projected = TRUE' explicitly.",
        call. = FALSE
      )
    }
    projected <- FALSE
  }

  if (length(projected) != 1L || is.na(projected)) {
    stop("'projected' must be TRUE, FALSE, or NULL.", call. = FALSE)
  }
  if (!projected && outside_bounds) {
    stop(
      "Coordinates in ",
      name,
      " are outside valid WGS-84 degree bounds (Latitude [-90, 90], Longitude [-180, 180]).",
      call. = FALSE
    )
  }

  list(dt = dt, projected = projected)
}

#' Clean Raw GPS Data
#'
#' Cleans raw GPS or GTFS-Realtime vehicle position data: maps input columns to
#' the canonical schema, removes records with zero coordinates or missing
#' vehicle identifiers, deduplicates repeated pings, parses timestamps, and
#' sorts by vehicle, date, and time.
#'
#' The canonical column names follow the GTFS-Realtime convention, so the
#' output of \code{gtfsrealtime::read_gtfsrt_positions()} is accepted directly.
#' Any other source (AVL exports, logger CSVs) can be mapped via
#' \code{vehicle_col} and \code{time_col}. Columns beyond the canonical set
#' (e.g. \code{trip_id}, \code{route_id}, \code{stop_id}) are passed through
#' untouched.
#'
#' @param raw_gps_df A data.frame containing raw GPS data. Must include
#'   \code{latitude} and \code{longitude} (WGS-84 degrees unless
#'   \code{projected = TRUE}), a vehicle identifier column, and a timestamp
#'   column. A \code{speed} column is recommended (used for dwell-time
#'   estimation); GTFS-Realtime reports it in meters per second, but only
#'   \code{speed == 0} is interpreted. An \code{id} row identifier is generated
#'   when absent or not unique.
#' @param projected Logical. Is the coordinates data already projected? Default is NULL (auto-detect).
#' @param vehicle_col Character. Name of the column holding the vehicle
#'   identifier. Default \code{"vehicle_id"} (GTFS-Realtime convention).
#' @param time_col Character. Name of the column holding the observation time.
#'   Default \code{"timestamp"} (GTFS-Realtime convention). \code{POSIXct}
#'   input keeps its timezone (service days split at feed-local midnight);
#'   character input is parsed as UTC.
#' @param dedupe Logical. Drop repeated \code{(vehicle_id, timestamp)} rows,
#'   as produced by archived GTFS-Realtime feeds that re-report unchanged
#'   positions. Default \code{TRUE}.
#' @param drop_missing_vehicle Logical. Drop rows with missing/empty vehicle
#'   identifiers with a warning (\code{TRUE}, default) instead of failing.
#' @return A sorted \code{data.table} with canonical columns \code{id},
#'   \code{vehicle_id}, \code{latitude}, \code{longitude}, \code{timestamp},
#'   \code{speed}, additional \code{date} and \code{time_str} columns, and all
#'   other input columns passed through.
#' @examples
#' \donttest{
#' data(g2g_data_gps)
#' cleaned_gps <- g2g_clean_gps(g2g_data_gps)
#' head(cleaned_gps)
#' }
#' @importFrom data.table as.data.table setkeyv setnames
#' @export
g2g_clean_gps <- function(
  raw_gps_df,
  projected = NULL,
  vehicle_col = "vehicle_id",
  time_col = "timestamp",
  dedupe = TRUE,
  drop_missing_vehicle = TRUE
) {
  # Normalize coordinates and convert to data.table
  norm <- normalize_coordinates(
    raw_gps_df,
    projected = projected,
    name = "raw_gps_df"
  )
  dt <- norm$dt
  projected <- norm$projected

  # Map source columns onto the canonical schema
  for (mapping in list(
    list(vehicle_col, "vehicle_id"),
    list(time_col, "timestamp")
  )) {
    source_col <- mapping[[1]]
    target_col <- mapping[[2]]
    if (
      !is.character(source_col) || length(source_col) != 1L || is.na(source_col)
    ) {
      stop(
        "Column mapping arguments must be single column names.",
        call. = FALSE
      )
    }
    if (source_col != target_col) {
      validate_required_columns(dt, source_col, "raw GPS data")
      if (target_col %in% names(dt)) {
        stop(
          "raw GPS data contains both '",
          source_col,
          "' and '",
          target_col,
          "'; drop or rename one of them.",
          call. = FALSE
        )
      }
      data.table::setnames(dt, source_col, target_col)
    }
  }
  validate_required_columns(dt, c("vehicle_id", "timestamp"), "raw GPS data")

  # Remove rows with zero coordinates (GPS error sentinel)
  dt <- dt[latitude != 0 & longitude != 0]

  # Handle missing vehicle identifiers (common in real GTFS-RT feeds)
  vehicle_chr <- as.character(dt$vehicle_id)
  missing_vehicle <- is.na(vehicle_chr) | !nzchar(trimws(vehicle_chr))
  if (any(missing_vehicle)) {
    if (!isTRUE(drop_missing_vehicle)) {
      stop(
        "raw GPS data 'vehicle_id' must not contain missing or empty identifiers.",
        call. = FALSE
      )
    }
    warning(
      sum(missing_vehicle),
      " row(s) with missing/empty 'vehicle_id' removed.",
      call. = FALSE
    )
    dt <- dt[!missing_vehicle]
  }

  # Parse timestamps; POSIXct input keeps its timezone
  if (!inherits(dt$timestamp, "POSIXct")) {
    dt[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
  }
  if (anyNA(dt$timestamp)) {
    stop(
      "raw GPS data 'timestamp' contains unparseable timestamps.",
      call. = FALSE
    )
  }

  # Speed is optional but degrades dwell-time estimation when absent
  if (!"speed" %in% names(dt)) {
    warning(
      "No 'speed' column found; filling with NA. Stop dwell-time estimation ",
      "relies on speed == 0 and will treat every ping as moving.",
      call. = FALSE
    )
    dt[, speed := NA_real_]
  } else if (nrow(dt) > 0L && all(is.na(dt$speed))) {
    warning(
      "'speed' is entirely NA; stop dwell-time estimation will treat every ",
      "ping as moving.",
      call. = FALSE
    )
  }

  # Drop repeated observations (archived RT feeds re-report unchanged pings)
  if (isTRUE(dedupe)) {
    n_before <- nrow(dt)
    dt <- unique(dt, by = c("vehicle_id", "timestamp"))
    n_dropped <- n_before - nrow(dt)
    if (n_dropped > 0L) {
      message(
        "[INFO] Removed ",
        n_dropped,
        " duplicated (vehicle_id, timestamp) row(s)."
      )
    }
  }

  # Row identifiers: generate when absent or not usable as a unique key
  if (!"id" %in% names(dt)) {
    dt[, id := seq_len(.N)]
  } else {
    id_chr <- as.character(dt$id)
    if (
      anyNA(id_chr) ||
        any(!nzchar(trimws(id_chr))) ||
        anyDuplicated(id_chr) > 0L
    ) {
      message(
        "[INFO] 'id' column has missing or duplicated values; regenerating row ids."
      )
      dt[, id := seq_len(.N)]
    }
  }

  # Extract date and time strings (in the timestamp's own timezone)
  dt[, date := format(timestamp, "%Y-%m-%d")]
  dt[, time_str := format(timestamp, "%H:%M:%S")]

  # Sort by vehicle_id, date, and time_str
  data.table::setkeyv(dt, c("vehicle_id", "date", "time_str"))

  # Store projected flag as attribute
  attr(dt, "projected") <- projected

  return(dt)
}
