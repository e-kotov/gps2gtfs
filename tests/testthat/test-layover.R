base_ts <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
A <- c(7.290, 80.630)
B <- c(7.320, 80.660)
M <- c(7.300, 80.640)

test_that("layover segmentation recovers short-turn trips", {
  cleaned <- suppressMessages(g2g_clean_gps(make_layover_route("short_turn")))
  bounds <- extract_trips_layover_r(cleaned)

  expect_equal(nrow(bounds), 6L)
  expect_equal(unique(bounds$trip_id), 1:3)
  expect_true(all(is.na(bounds$bus_stop)))
  expect_true(all(bounds$rt_direction == "layover"))

  # Trip 1 ends at the dwell's arrival ping; trip 2 starts at its departure
  # ping; the silent gap at M cuts trips 2/3 the same way.
  expect_equal(bounds[trip_id == 1L]$timestamp, base_ts + c(0, 180))
  expect_equal(bounds[trip_id == 2L]$timestamp, base_ts + c(1260, 1380))
  expect_equal(bounds[trip_id == 3L]$timestamp, base_ts + c(2280, 2400))
  # Trip 2 ends mid-route (the short-turn point), which the two-terminal
  # model cannot represent.
  expect_equal(bounds[trip_id == 2L]$latitude[2], M[1])
})

test_that("layover segmentation detects loop trips (start == end)", {
  cleaned <- suppressMessages(g2g_clean_gps(make_layover_route("loop")))
  bounds <- extract_trips_layover_r(cleaned)

  expect_equal(unique(bounds$trip_id), 1:2)
  # Both trips start and end at A: the terminal-pairing model
  # (bus_stops[i] != bus_stops[i+1]) finds no trips here.
  expect_true(all(bounds$latitude == A[1]))
  expect_true(all(bounds$longitude == A[2]))
})

test_that("layover segmentation handles a 3-terminal branch with labels", {
  gps <- make_layover_route("branch")
  cleaned <- suppressMessages(g2g_clean_gps(gps))
  terminals <- data.frame(
    terminal_id = c("A", "B", "C"),
    latitude = c(7.290, 7.320, 7.290),
    longitude = c(80.630, 80.660, 80.660)
  )
  bounds <- extract_trips_layover_r(cleaned, terminals)

  expect_equal(unique(bounds$trip_id), 1:4)
  starts <- bounds[seq(1, .N, by = 2)]
  ends <- bounds[seq(2, .N, by = 2)]
  expect_equal(starts$bus_stop, c("A", "B", "A", "C"))
  expect_equal(ends$bus_stop, c("B", "A", "C", "A"))
})

test_that("short in-service stops (traffic lights) do not cut trips", {
  gps <- lay_wrap(list(
    lay_leg(A, M, 0, n = 3),
    lay_dwell(M, 150, 90, interval = 30),
    lay_leg(M, B, 270, n = 3)
  ))
  cleaned <- suppressMessages(g2g_clean_gps(gps))
  bounds <- extract_trips_layover_r(cleaned)

  expect_equal(unique(bounds$trip_id), 1L)
  expect_equal(bounds$timestamp, base_ts + c(0, 390))
})

test_that("parked GPS jitter with an outlier yields one boundary", {
  jitter <- 20 / 111000
  outlier <- 66 / 111000
  dwell <- lay_dwell(B, 240, 1200, interval = 60, jitter_lat = jitter)
  dwell$latitude[6] <- B[1] + outlier
  gps <- lay_wrap(list(
    lay_leg(A, B, 0),
    dwell,
    lay_leg(B, A, 1560)
  ))
  cleaned <- suppressMessages(g2g_clean_gps(gps))
  bounds <- extract_trips_layover_r(cleaned)

  expect_equal(unique(bounds$trip_id), 1:2)
  expect_equal(bounds[trip_id == 2L]$timestamp[1], base_ts + 1560)
})

test_that("all-stationary vehicles and empty input produce no trips", {
  gps <- lay_wrap(list(lay_dwell(B, 0, 1800, interval = 120)))
  cleaned <- suppressMessages(g2g_clean_gps(gps))
  expect_message(
    bounds <- extract_trips_layover_r(cleaned),
    "single-ping"
  )
  expect_equal(nrow(bounds), 0L)
  expect_type(bounds$bus_stop, "character")
  expect_type(bounds$trip_id, "integer")
  expect_type(bounds$rt_direction, "character")

  empty <- extract_trips_layover_r(cleaned[0])
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("bus_stop", "trip_id", "rt_direction") %in% names(empty)))
})

test_that("stationary no-movement segments are dropped", {
  # Parked pings spanning less than layover_gap, then a session gap, then a
  # real trip: the parked segment moved nowhere and must not become a trip.
  gps <- lay_wrap(list(
    lay_dwell(B, 0, 480, interval = 120),
    lay_leg(B, A, 480 + 4 * 3600 + 120)
  ))
  cleaned <- suppressMessages(g2g_clean_gps(gps))
  expect_message(
    bounds <- extract_trips_layover_r(cleaned),
    "stationary"
  )
  expect_equal(unique(bounds$trip_id), 1L)
  expect_equal(bounds[1L]$timestamp, base_ts + 480 + 4 * 3600 + 120)
})

test_that("session gaps still bound layover trips", {
  gps <- lay_wrap(list(
    lay_leg(A, B, 0),
    lay_leg(B, A, 18000)
  ))
  cleaned <- suppressMessages(g2g_clean_gps(gps))
  bounds <- extract_trips_layover_r(cleaned)
  expect_equal(unique(bounds$trip_id), 1:2)
})

test_that("g2g_extract_trips layover pipeline emits K=1 inference tables", {
  trips <- suppressMessages(g2g_extract_trips(
    gps_data = make_layover_route("short_turn"),
    segmentation = "layover"
  ))
  expect_equal(nrow(trips), 3L)
  expect_true(all(trips$direction == 1L))
  expect_true(all(is.na(trips$start_terminal)))
  expect_true(all(is.na(trips$end_terminal)))
  expect_true(all(is.na(trips$provided_trip_id)))
  expect_equal(trips$start_time, base_ts + c(0, 1260, 2280))
  expect_equal(trips$end_time, base_ts + c(180, 1380, 2400))
})

test_that("auto segmentation resolves to layover without terminals/trip_col", {
  explicit <- suppressMessages(g2g_extract_trips(
    gps_data = make_layover_route("loop"),
    segmentation = "layover"
  ))
  msgs <- capture_messages(
    auto <- g2g_extract_trips(gps_data = make_layover_route("loop"))
  )
  expect_true(any(grepl("segmenting trips at layovers", msgs)))
  expect_equal(auto, explicit)
})

test_that("auto segmentation with terminals is the classic path, unchanged", {
  data(g2g_data_gps)
  data(g2g_data_terminals)
  t_auto <- suppressMessages(g2g_extract_trips(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    terminals_buffer_radius = 50
  ))
  t_explicit <- suppressMessages(g2g_extract_trips(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    terminals_buffer_radius = 50,
    segmentation = "terminals"
  ))
  expect_identical(t_auto, t_explicit)
})

test_that("layover end-to-end stop times use one direction group", {
  inputs <- make_route_inputs()
  msgs <- capture_messages(
    res <- g2g_extract_trips_and_stop_times(
      gps_data = make_layover_route("short_turn"),
      stops_data = inputs$stops,
      stops_buffer_radius = 100,
      stops_extended_buffer_radius = 150,
      segmentation = "layover"
    )
  )
  expect_true(any(grepl("labels are ignored", msgs)))
  expect_equal(nrow(res$trips), 3L)
  expect_true(nrow(res$stop_times) > 0L)
  expect_true(all(res$stop_times$direction == 1L))
  expect_true(all(is.na(res$stop_times$provided_trip_id)))

  # The stop 'direction' column is optional in layover mode.
  res_nodir <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = make_layover_route("short_turn"),
    stops_data = inputs$stops[, c("stop_id", "latitude", "longitude")],
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150,
    segmentation = "layover"
  ))
  expect_equal(res_nodir$stop_times$stop_id, res$stop_times$stop_id)
})

test_that("layover trips are backend-independent", {
  auto <- suppressMessages(g2g_extract_trips(
    gps_data = make_layover_route("branch"),
    segmentation = "layover"
  ))
  pure <- suppressMessages(g2g_extract_trips(
    gps_data = make_layover_route("branch"),
    segmentation = "layover",
    backend = "pure_r"
  ))
  attr(auto, "backend") <- NULL
  attr(pure, "backend") <- NULL
  expect_equal(auto, pure)
})

test_that("segmentation argument errors and warnings", {
  gps <- make_layover_route("loop")

  expect_error(
    suppressMessages(g2g_extract_trips(
      gps_data = gps,
      segmentation = "terminals"
    )),
    "terminals_data' is required"
  )
  expect_error(
    suppressMessages(g2g_extract_trips(
      gps_data = gps,
      trip_col = "trip_id",
      segmentation = "layover"
    )),
    "not used with 'trip_col'"
  )
  expect_error(
    g2g_extract_trips(gps_data = gps, segmentation = "layover", layover_gap = -1),
    "layover_gap"
  )
  expect_error(
    g2g_extract_trips(gps_data = gps, segmentation = "layover", layover_radius = 0),
    "layover_radius"
  )
  expect_warning(
    suppressMessages(g2g_extract_trips(
      gps_data = gps,
      segmentation = "layover",
      layover_gap = 4 * 3600,
      session_gap = 3600
    )),
    "subsumed by session cuts"
  )
})
