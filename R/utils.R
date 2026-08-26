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

validate_debounce_thresholds <- function(min_pings, min_seconds) {
  if (
    length(min_pings) != 1L || !is.numeric(min_pings) || is.na(min_pings) ||
      !is.finite(min_pings) || min_pings < 1 || min_pings != trunc(min_pings)
  ) {
    stop(
      "'direction_debounce_min_pings' must be one whole number of pings >= 1.",
      call. = FALSE
    )
  }
  if (
    length(min_seconds) != 1L || !is.numeric(min_seconds) ||
      is.na(min_seconds) || !is.finite(min_seconds) || min_seconds < 0
  ) {
    stop(
      "'direction_debounce_min_seconds' must be one non-negative finite ",
      "number of seconds.",
      call. = FALSE
    )
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

# Closed vocabulary for the C5 `orientation_status` column (see the additive,
# versioned C5 contract in private/terminal-detection-spike.md §3). This column
# is never NA. Phase 0/1 emits only "none" - no orientation method has been
# applied, the pre-detector state. The remaining levels are RESERVED for the
# (not-yet-authorized) P2 orientation detector: "ok" (a confident 0/1 call),
# "single_group" (layover-style one-direction output, scored as orientation
# unavailable), and "abstain_<reason>" for observable abstentions. A detector
# that introduces a new abstention reason extends this vector in one place so
# every produced table keeps one identical, factor-stable level set.
orientation_status_levels <- c("none", "ok", "single_group")

# Build the C5 orientation_status factor. Defaults to "none" (no detector run).
# Values outside `orientation_status_levels` would become NA, violating the
# never-NA contract, so a detector must extend the level set before emitting a
# new value.
new_orientation_status <- function(value = "none", n = 1L) {
  x <- if (length(value) == 1L) rep(value, n) else value
  factor(x, levels = orientation_status_levels)
}

# The additive C5 orientation/pattern columns carried on BOTH `trips` and
# `stop_times` (orientation_* + pattern_ref). Anchors are trips-only (see
# add_trip_orientation_cols). All default to the detector-off "empty" state:
# orientation_id NA (abstained/unavailable), orientation_status "none",
# orientation_confidence NA, pattern_ref NA (reserved until P4 clears its gate).
# Appended after all pre-existing columns so the legacy schema's column
# positions, names, types, and values are untouched (backward-compatibility
# contract, spike §3.5).
add_shared_orientation_cols <- function(dt) {
  n <- nrow(dt)
  dt[, orientation_id := NA_integer_]
  dt[, orientation_status := new_orientation_status("none", n)]
  dt[, orientation_confidence := NA_real_]
  dt[, pattern_ref := NA_character_]
  dt[]
}

# Trip-level C5 columns: the shared orientation/pattern columns plus the
# turnaround anchor refs, which are trips-only (a turnaround cluster is
# trip-level metadata with no per-stop-event meaning, spike §3.4 / §9).
add_trip_orientation_cols <- function(dt) {
  add_shared_orientation_cols(dt)
  dt[, start_anchor_ref := NA_character_]
  dt[, end_anchor_ref := NA_character_]
  dt[]
}

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

# Column names that the stop-matching stage owns. A GPS frame may carry
# columns of its own with these names (g2g_clean_gps() passes everything
# through), and each one would collide with an internal of the same name -
# silently emptying every direction group, or shadowing the matcher's output.
# prepare_trajectory_r() drops them from the trajectory before matching.
trajectory_reserved_cols <- c(
  "trip_id",
  "direction",
  "bus_stop",
  "stop_id",
  "grouped_ends"
)

# Below this separation, the two most frequent trip endpoints of a route are
# treated as suspicious in g2g_terminals_from_gtfs(): platforms of one place,
# not the two ends of a line. Generous enough that a genuinely short shuttle
# does not trip it, tight enough to catch platform-level stop_id pairs.
terminal_separation_floor_m <- 150

# Coerce an identifier column to character without inviting scientific
# notation. as.character() on a double renders 1e5 as "1e+05", which silently
# breaks every downstream id join; format() with scientific = FALSE does not.
# Integers, characters and factors go through as.character() unchanged.
as_id_chr <- function(x) {
  if (is.double(x)) {
    out <- format(x, scientific = FALSE, trim = TRUE)
    out[is.na(x)] <- NA_character_
    return(out)
  }
  as.character(x)
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
  dt <- data.table::data.table(
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
  # Additive C5 columns, appended after every pre-existing column.
  add_trip_orientation_cols(dt)
}

empty_stop_times <- function(vehicle_id = character()) {
  dt <- data.table::data.table(
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
  # Additive C5 columns (shared set only; anchors are trips-only), appended
  # after every pre-existing column.
  add_shared_orientation_cols(dt)
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
