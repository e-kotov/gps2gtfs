resolve_backend <- function(backend = "auto") {
  backend <- match.arg(backend, c("auto", "rust", "rcpp", "pure_r"))

  if (backend == "auto") {
    if (is_rust_available()) {
      return("rust")
    }
    if (is_rcpp_available()) {
      return("rcpp")
    }
    return("pure_r")
  }

  if (backend == "rust" && !is_rust_available()) {
    stop("The requested 'rust' backend is not available.", call. = FALSE)
  }
  if (backend == "rcpp" && !is_rcpp_available()) {
    stop("The requested 'rcpp' backend is not available.", call. = FALSE)
  }

  backend
}

validate_positive_radius <- function(x, name) {
  if (
    length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) || x <= 0
  ) {
    stop(name, " must be one positive finite number.", call. = FALSE)
  }
}

validate_projected_crs <- function(projected_crs) {
  if (
    !is.null(projected_crs) &&
      (length(projected_crs) != 1L ||
        !is.numeric(projected_crs) ||
        is.na(projected_crs) ||
        !is.finite(projected_crs))
  ) {
    stop("'projected_crs' must be one finite numeric EPSG code.", call. = FALSE)
  }
}

validate_required_columns <- function(dt, required, name) {
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0L) {
    stop(
      "Missing required columns in ",
      name,
      ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_identifiers <- function(x, name) {
  values <- as.character(x)
  if (anyNA(values) || any(!nzchar(trimws(values)))) {
    stop(name, " must not contain missing or empty identifiers.", call. = FALSE)
  }
}

empty_trip_features <- function(deviceid = character()) {
  data.table::data.table(
    trip_id = integer(),
    deviceid = deviceid[0],
    date = character(),
    start_terminal = character(),
    end_terminal = character(),
    direction = integer(),
    start_time = character(),
    end_time = character(),
    duration_in_mins = double(),
    day_of_week = integer(),
    hour_of_day = integer()
  )
}

empty_stop_times <- function(deviceid = character()) {
  data.table::data.table(
    trip_id = integer(),
    deviceid = deviceid[0],
    date = character(),
    direction = integer(),
    stop_id = character(),
    arrival_time = character(),
    departure_time = character(),
    dwell_time_in_seconds = double(),
    day_of_week = integer(),
    hour_of_day = integer(),
    is_weekday = integer()
  )
}

empty_trajectory <- function(cleaned_gps_dt) {
  result <- data.table::copy(cleaned_gps_dt[0])
  result[, trip_id := integer()]
  result[, direction := integer()]
  attr(result, "projected") <- attr(cleaned_gps_dt, "projected")
  result
}

set_backend <- function(x, backend) {
  attr(x, "backend") <- backend
  x
}
