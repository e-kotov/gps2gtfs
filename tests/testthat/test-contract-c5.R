# Backward-compatibility of the additive, versioned C5 contract change
# (private/terminal-detection-spike.md §3). With no orientation detector
# enabled (the only supported state today), every pre-existing column in
# `trips`/`stop_times` must be identical in name, order, and type to the
# pre-change schema, and the new columns must be appended present-and-empty.

# The frozen pre-change schema. Names in exact order, and the class each column
# must keep. Guards against an accidental reorder or type change of a legacy
# column when new columns are added.
legacy_trip_cols <- c(
  "trip_id", "vehicle_id", "date", "start_terminal", "end_terminal",
  "direction", "start_time", "end_time", "provided_trip_id",
  "duration_in_mins", "day_of_week", "hour_of_day", "is_weekday"
)
legacy_trip_classes <- list(
  trip_id = "integer", date = "character", start_terminal = "character",
  end_terminal = "character", direction = "integer",
  start_time = "POSIXct", end_time = "POSIXct",
  provided_trip_id = "character", duration_in_mins = "numeric",
  day_of_week = "factor", hour_of_day = "integer", is_weekday = "logical"
)
legacy_stop_cols <- c(
  "trip_id", "vehicle_id", "date", "direction", "stop_id", "arrival_time",
  "departure_time", "dwell_time_in_seconds", "day_of_week", "hour_of_day",
  "is_weekday", "provided_trip_id"
)
legacy_stop_classes <- list(
  trip_id = "integer", date = "character", direction = "integer",
  stop_id = "character", arrival_time = "POSIXct",
  departure_time = "POSIXct", dwell_time_in_seconds = "numeric",
  day_of_week = "factor", hour_of_day = "integer", is_weekday = "logical",
  provided_trip_id = "character"
)

# The additive C5 columns, in the order they are appended.
new_shared_cols <- c(
  "orientation_id", "orientation_status", "orientation_confidence",
  "pattern_ref"
)
new_trip_only_cols <- c("start_anchor_ref", "end_anchor_ref")

expect_class1 <- function(x, cls) {
  # inherits() handles POSIXct (c("POSIXct","POSIXt")) and factor cleanly.
  expect_true(inherits(x, cls))
}

check_legacy_prefix <- function(dt, legacy_cols, legacy_classes) {
  # Pre-existing columns are the leading columns, in the frozen order.
  expect_identical(names(dt)[seq_along(legacy_cols)], legacy_cols)
  for (col in names(legacy_classes)) {
    expect_class1(dt[[col]], legacy_classes[[col]])
  }
}

test_that("C5: new trip columns are appended present-and-empty, legacy intact", {
  res <- g2g_extract_trips_and_stop_times(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 50,
    stops_buffer_radius = 30,
    stops_extended_buffer_radius = 50,
    backend = "pure_r"
  )
  trips <- res$trips
  expect_gt(nrow(trips), 0L)

  check_legacy_prefix(trips, legacy_trip_cols, legacy_trip_classes)

  # New columns follow the legacy block, in the documented order.
  expect_identical(
    names(trips)[-seq_along(legacy_trip_cols)],
    c(new_shared_cols, new_trip_only_cols)
  )

  # Present-and-empty (detector-off state).
  expect_true(all(is.na(trips$orientation_id)))
  expect_type(trips$orientation_id, "integer")
  expect_true(all(is.na(trips$orientation_confidence)))
  expect_type(trips$orientation_confidence, "double")
  expect_true(all(is.na(trips$pattern_ref)))
  expect_type(trips$pattern_ref, "character")
  expect_true(all(is.na(trips$start_anchor_ref)))
  expect_true(all(is.na(trips$end_anchor_ref)))

  # orientation_status is a never-NA factor defaulting to "none".
  expect_s3_class(trips$orientation_status, "factor")
  expect_false(anyNA(trips$orientation_status))
  expect_true(all(as.character(trips$orientation_status) == "none"))
  expect_identical(
    levels(trips$orientation_status),
    c("none", "ok", "single_group")
  )
})

test_that("C5: new stop_times columns are appended; anchors stay trips-only", {
  res <- g2g_extract_trips_and_stop_times(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 50,
    stops_buffer_radius = 30,
    stops_extended_buffer_radius = 50,
    backend = "pure_r"
  )
  st <- res$stop_times
  expect_gt(nrow(st), 0L)

  check_legacy_prefix(st, legacy_stop_cols, legacy_stop_classes)

  # The shared set is appended; the anchor columns are NOT propagated to stops.
  expect_identical(names(st)[-seq_along(legacy_stop_cols)], new_shared_cols)
  expect_false(any(new_trip_only_cols %in% names(st)))

  expect_true(all(is.na(st$orientation_id)))
  expect_true(all(is.na(st$orientation_confidence)))
  expect_true(all(is.na(st$pattern_ref)))
  expect_s3_class(st$orientation_status, "factor")
  expect_false(anyNA(st$orientation_status))
  expect_true(all(as.character(st$orientation_status) == "none"))
})

test_that("C5: orientation/pattern propagate from trips onto stop_times by trip_id", {
  # The stop_times labels are copied from their trip (same mechanism as
  # provided_trip_id). Detector-off they are all empty, but the join must hold
  # per trip so the plumbing is correct once a detector populates trips.
  res <- g2g_extract_trips_and_stop_times(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 50,
    stops_buffer_radius = 30,
    stops_extended_buffer_radius = 50,
    backend = "pure_r"
  )
  trips <- res$trips
  st <- res$stop_times
  key <- c("orientation_id", "orientation_status", "orientation_confidence",
           "pattern_ref")
  merged <- merge(
    st[, c("trip_id", key), with = FALSE],
    trips[, c("trip_id", key), with = FALSE],
    by = "trip_id", suffixes = c(".st", ".tr")
  )
  for (k in key) {
    expect_identical(
      as.character(merged[[paste0(k, ".st")]]),
      as.character(merged[[paste0(k, ".tr")]])
    )
  }
})

test_that("C5: empty-result schema matches the populated schema exactly", {
  # The zero-row constructors returned on empty input must carry the identical
  # column set/order/types, so downstream binds never see a schema drift.
  et <- empty_trip_features()
  es <- empty_stop_times()
  expect_identical(
    names(et),
    c(legacy_trip_cols, new_shared_cols, new_trip_only_cols)
  )
  expect_identical(names(es), c(legacy_stop_cols, new_shared_cols))
  expect_s3_class(et$orientation_status, "factor")
  expect_s3_class(es$orientation_status, "factor")
  expect_identical(levels(et$orientation_status), c("none", "ok", "single_group"))
  expect_type(et$orientation_id, "integer")
  expect_type(es$orientation_confidence, "double")
  expect_type(et$start_anchor_ref, "character")
  expect_false("start_anchor_ref" %in% names(es))
})

test_that("C5: columns are present in every segmentation mode", {
  # Fast path (supplied trip identities + data-driven direction).
  rt <- make_rt_trajectory()
  ri <- make_route_inputs()
  res_fast <- g2g_extract_trips_and_stop_times(
    gps_data = rt,
    stops_data = ri$stops,
    stops_buffer_radius = 300,
    stops_extended_buffer_radius = 500,
    trip_col = "trip_id",
    direction_col = "trip_id",
    backend = "pure_r"
  )
  expect_true(all(new_shared_cols %in% names(res_fast$trips)))
  expect_true(all(new_trip_only_cols %in% names(res_fast$trips)))
  expect_true(all(new_shared_cols %in% names(res_fast$stop_times)))

  # Layover path (single direction group, no terminals).
  lay <- make_layover_route("short_turn")
  res_lay <- g2g_extract_trips_and_stop_times(
    gps_data = lay,
    stops_data = ri$stops,
    stops_buffer_radius = 300,
    stops_extended_buffer_radius = 500,
    segmentation = "layover",
    backend = "pure_r"
  )
  expect_true(all(new_trip_only_cols %in% names(res_lay$trips)))
  # Detector-off: even the single-group layover mode reports "none" until a
  # P2 detector is authorized to emit "single_group".
  expect_true(all(as.character(res_lay$trips$orientation_status) == "none"))
})
