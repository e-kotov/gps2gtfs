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

validate_session_gap <- function(x) {
  if (
    length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) || x <= 0
  ) {
    stop("'session_gap' must be one positive finite number of seconds.", call. = FALSE)
  }
}

validate_layover_gap <- function(x) {
  if (
    length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) || x <= 0
  ) {
    stop("'layover_gap' must be one positive finite number of seconds.", call. = FALSE)
  }
}

validate_layover_radius <- function(x) {
  if (
    length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) || x <= 0
  ) {
    stop("'layover_radius' must be one positive finite number of meters.", call. = FALSE)
  }
}

# Single synthetic direction level used by layover segmentation (v1 emits one
# direction group; heading-based inference is a planned follow-up). Internal
# only: output tables carry the mapped integer direction (always 1L).
layover_direction_level <- "layover"

resolve_segmentation <- function(segmentation, trip_col, have_terminals) {
  segmentation <- match.arg(segmentation, c("auto", "terminals", "layover"))
  if (!is.null(trip_col)) {
    if (segmentation != "auto") {
      stop(
        "'segmentation' selects the raw-GPS spatial segmenter; it is not ",
        "used with 'trip_col' (supplied trip identities drive segmentation).",
        call. = FALSE
      )
    }
    return("fast")
  }
  if (segmentation == "auto") {
    if (have_terminals) {
      return("terminals")
    }
    message(
      "[INFO] No 'terminals_data' and no 'trip_col'; segmenting trips at ",
      "layovers (dwells longer than 'layover_gap')."
    )
    return("layover")
  }
  if (segmentation == "terminals" && !have_terminals) {
    stop(
      "'terminals_data' is required unless 'direction_col' is supplied or ",
      "segmentation = \"layover\".",
      call. = FALSE
    )
  }
  segmentation
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

weekday_levels <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)

make_weekday_features <- function(date) {
  weekday_index <- as.integer((as.POSIXlt(as.Date(date))$wday + 6L) %% 7L)
  list(
    day_of_week = factor(
      weekday_index,
      levels = 0:6,
      labels = weekday_levels,
      ordered = TRUE
    ),
    is_weekday = weekday_index < 5L
  )
}

empty_trip_features <- function(vehicle_id = character()) {
  data.table::data.table(
    trip_id = integer(),
    vehicle_id = vehicle_id[0],
    date = character(),
    start_terminal = character(),
    end_terminal = character(),
    direction = integer(),
    start_time = as.POSIXct(character(), tz = "UTC"),
    end_time = as.POSIXct(character(), tz = "UTC"),
    provided_trip_id = character(),
    duration_in_mins = double(),
    day_of_week = factor(
      levels = 0:6,
      labels = weekday_levels,
      ordered = TRUE
    ),
    hour_of_day = integer(),
    is_weekday = logical()
  )
}

empty_stop_times <- function(vehicle_id = character()) {
  data.table::data.table(
    trip_id = integer(),
    vehicle_id = vehicle_id[0],
    date = character(),
    direction = integer(),
    stop_id = character(),
    arrival_time = as.POSIXct(character(), tz = "UTC"),
    departure_time = as.POSIXct(character(), tz = "UTC"),
    dwell_time_in_seconds = double(),
    day_of_week = factor(
      levels = 0:6,
      labels = weekday_levels,
      ordered = TRUE
    ),
    hour_of_day = integer(),
    is_weekday = logical(),
    provided_trip_id = character()
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
