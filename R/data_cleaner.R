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
#' Cleans raw GPS data by removing records with latitude or longitude equal to zero,
#' parsing the device time, and sorting by device ID, date, and time.
#'
#' @param raw_gps_df A data.frame containing raw GPS data. Must include columns:
#'   \code{id}, \code{deviceid}, \code{latitude}, \code{longitude}, \code{devicetime}, and \code{speed}.
#' @param projected Logical. Is the coordinates data already projected? Default is NULL (auto-detect).
#' @return A sorted \code{data.table} with additional \code{date} and \code{time_str} columns.
#' @examples
#' \donttest{
#' data(g2g_data_gps)
#' cleaned_gps <- g2g_clean_gps(g2g_data_gps)
#' head(cleaned_gps)
#' }
#' @importFrom data.table as.data.table setkeyv
#' @export
g2g_clean_gps <- function(raw_gps_df, projected = NULL) {
  # Normalize coordinates and convert to data.table
  norm <- normalize_coordinates(
    raw_gps_df,
    projected = projected,
    name = "raw_gps_df"
  )
  dt <- norm$dt
  projected <- norm$projected

  # Required columns check
  required_cols <- c(
    "id",
    "deviceid",
    "latitude",
    "longitude",
    "devicetime",
    "speed"
  )
  validate_required_columns(dt, required_cols, "raw GPS data")
  validate_identifiers(dt$id, "raw GPS data 'id'")
  validate_identifiers(dt$deviceid, "raw GPS data 'deviceid'")

  # Remove rows where latitude and longitude are both zero
  dt <- dt[latitude != 0 & longitude != 0]

  # Convert devicetime to POSIXct
  dt[, devicetime := as.POSIXct(devicetime, tz = "UTC")]
  if (anyNA(dt$devicetime)) {
    stop(
      "raw GPS data 'devicetime' contains unparseable timestamps.",
      call. = FALSE
    )
  }

  # Extract date and time strings
  dt[, date := format(devicetime, "%Y-%m-%d")]
  dt[, time_str := format(devicetime, "%H:%M:%S")]

  # Sort by deviceid, date, and time_str
  data.table::setkeyv(dt, c("deviceid", "date", "time_str"))

  # Store projected flag as attribute
  attr(dt, "projected") <- projected

  return(dt)
}
