test_that("results carry a full-schema diagnostics table", {
  res <- suppressWarnings(suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 50,
    stops_extended_buffer_radius = 100
  )))

  diag <- g2g_diagnostics(res)
  expect_s3_class(diag, "g2g_diagnostics")
  expect_identical(diag$metric, g2g_diagnostics_schema()$metric)
  expect_identical(g2g_diagnostics(res), attr(res, "diagnostics"))
})

test_that("diagnostics count the extraction fixture exactly", {
  res <- suppressWarnings(suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 50,
    stops_extended_buffer_radius = 100
  )))
  diag <- g2g_diagnostics(res)
  val <- function(m) diag$n[diag$metric == m]

  expect_identical(val("pings_in"), 2167L)
  expect_identical(val("pings_dropped_zero_coord"), 28L)
  expect_identical(val("pings_dropped_duplicate"), 56L)
  expect_identical(val("pings_after_cleaning"), 2083L)
  expect_identical(
    val("pings_assigned_to_trips") + val("pings_dropped_not_in_trip"),
    val("pings_after_cleaning")
  )
  expect_identical(val("trips_kept"), nrow(res$trips))
  expect_identical(val("stop_times_kept"), nrow(res$stop_times))
  # Terminal-buffer segmentation does not measure the segment reasons.
  expect_identical(val("segments_dropped_single_ping"), NA_integer_)
})

test_that("g2g_clean_gps stamps per-reason cleaning drops", {
  gps <- data.frame(
    vehicle_id = "b1",
    latitude = c(0, 6.90, 6.90, 6.91),
    longitude = c(0, 79.90, 79.90, 79.91),
    timestamp = c(
      "2026-06-06 08:00:00",
      "2026-06-06 08:01:00",
      "2026-06-06 08:01:00",
      "2026-06-06 08:02:00"
    ),
    speed = 0
  )
  cleaned <- g2g_clean_gps(gps, tz = "UTC")
  expect_identical(
    attr(cleaned, "clean_drops"),
    c(
      pings_dropped_zero_coord = 1L,
      pings_dropped_missing_vehicle = 0L,
      pings_dropped_duplicate = 1L
    )
  )
})

test_that("the fast path counts rows with no usable trip identity", {
  inp <- make_route_inputs()
  res <- suppressWarnings(suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = make_rt_trajectory(),
    terminals_data = inp$terminals,
    stops_data = inp$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 60,
    stops_extended_buffer_radius = 120,
    trip_col = "trip_id"
  )))
  diag <- g2g_diagnostics(res)
  # One un-annotated (NA trip_id) layover ping in the fixture.
  expect_identical(
    diag$n[diag$metric == "rows_dropped_no_trip_identity"],
    1L
  )
  expect_identical(
    diag$n[diag$metric == "segments_dropped_stationary"],
    NA_integer_
  )
})

test_that("total fast-path failure still counts what was dropped", {
  base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
  terminals <- make_route_inputs()$terminals

  # Every row lacks a usable trip identity.
  all_na <- data.frame(
    vehicle_id = "b1",
    latitude = c(7.290, 7.320),
    longitude = c(80.630, 80.660),
    timestamp = base + c(0, 300),
    speed = 0,
    trip_id = NA_character_
  )
  diag_na <- g2g_diagnostics(g2g_extract_trips(
    gps_data = all_na,
    terminals_data = terminals,
    terminals_buffer_radius = 100,
    trip_col = "trip_id"
  ))
  expect_identical(
    diag_na$n[diag_na$metric == "rows_dropped_no_trip_identity"],
    2L
  )
  expect_identical(diag_na$n[diag_na$metric == "trips_kept"], 0L)

  # Every trip identity is a lone ping.
  all_single <- data.frame(
    vehicle_id = "b1",
    latitude = c(7.290, 7.320),
    longitude = c(80.630, 80.660),
    timestamp = base + c(0, 300),
    speed = 0,
    trip_id = c("T1", "T2")
  )
  diag_single <- g2g_diagnostics(g2g_extract_trips(
    gps_data = all_single,
    terminals_data = terminals,
    terminals_buffer_radius = 100,
    trip_col = "trip_id"
  ))
  expect_identical(
    diag_single$n[diag_single$metric == "segments_dropped_single_ping"],
    2L
  )
  expect_identical(
    diag_single$n[diag_single$metric == "rows_dropped_no_trip_identity"],
    0L
  )
  expect_identical(diag_single$n[diag_single$metric == "trips_kept"], 0L)
})

test_that("total layover failure still counts stationary drops", {
  base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
  # Two short stationary clusters separated by a silent gap longer than the
  # layover gap: the vehicle never moves, so neither becomes a trip.
  stationary <- data.frame(
    vehicle_id = "L1",
    latitude = c(7.290, 7.290, 7.290, 7.320, 7.320, 7.320),
    longitude = c(80.630, 80.630, 80.630, 80.660, 80.660, 80.660),
    timestamp = base + c(0, 10, 20, 1220, 1230, 1240),
    speed = 0
  )
  diag <- g2g_diagnostics(g2g_extract_trips(
    gps_data = stationary,
    segmentation = "layover"
  ))
  expect_identical(diag$n[diag$metric == "trips_kept"], 0L)
  expect_identical(
    diag$n[diag$metric == "segments_dropped_stationary"],
    2L
  )
})

test_that("empty input still yields a schema-stable diagnostics table", {
  res <- suppressWarnings(suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = g2g_data_gps[0],
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 50,
    stops_extended_buffer_radius = 100
  )))
  diag <- g2g_diagnostics(res)
  expect_identical(diag$metric, g2g_diagnostics_schema()$metric)
  expect_identical(diag$n[diag$metric == "pings_in"], 0L)
  expect_identical(diag$n[diag$metric == "trips_kept"], 0L)
})

test_that("only concerning losses trigger the coverage warning", {
  # Duplicate removal is routine: no warning.
  routine <- build_diagnostics(c(
    pings_in = 10L,
    pings_dropped_duplicate = 3L,
    trips_kept = 2L
  ))
  expect_null(diagnostics_warning_message(routine))

  # Pings that never entered a trip are a real loss: warn.
  lost <- build_diagnostics(c(
    pings_in = 10L,
    pings_dropped_not_in_trip = 4L,
    trips_kept = 1L
  ))
  expect_type(diagnostics_warning_message(lost), "character")

  expect_no_warning(maybe_warn_diagnostics(lost, enabled = FALSE))
})

test_that("the coverage warning and diagnostics print are stable", {
  diag <- build_diagnostics(c(
    pings_in = 2167L,
    pings_dropped_zero_coord = 28L,
    pings_dropped_duplicate = 56L,
    pings_after_cleaning = 2083L,
    pings_assigned_to_trips = 1397L,
    pings_dropped_not_in_trip = 686L,
    trips_kept = 6L,
    stop_times_kept = 81L
  ))
  expect_snapshot(cat(diagnostics_warning_message(diag)))
  expect_snapshot(print(diag))
})

test_that("max_trip_duration_mins reports the longest extracted trip", {
  # A mis-segmented run merges a whole shift into one "trip" while the trip
  # count stays plausible, so the duration is the metric that shows it. It is
  # reported, never judged: no threshold, no warning.
  inputs <- make_route_inputs()
  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = make_rt_trajectory(),
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150,
    trip_col = "trip_id"
  ))

  diag <- g2g_diagnostics(result)
  longest <- diagnostics_value(diag, "max_trip_duration_mins")
  expect_false(is.na(longest))
  expect_identical(
    longest,
    as.integer(round(max(result$trips$duration_in_mins)))
  )
})

test_that("max_trip_duration_mins is NA when nothing was extracted", {
  expect_identical(max_trip_duration_metric(NULL), NA_integer_)
  expect_identical(max_trip_duration_metric(empty_trip_features()), NA_integer_)
})
