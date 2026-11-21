test_that("g2g_stops_from_positions estimates median stop coordinates", {
  set.seed(42)
  jitter_deg <- function(n) stats::runif(n, -0.0002, 0.0002)
  positions <- data.frame(
    stop_id = c(rep("S1", 5), rep("S2", 5), rep("S1", 3), NA, "S3"),
    latitude = c(
      7.300 + jitter_deg(5),
      7.310 + jitter_deg(5),
      7.300 + jitter_deg(3),
      7.999,
      7.320
    ),
    longitude = c(
      80.640 + jitter_deg(5),
      80.650 + jitter_deg(5),
      80.640 + jitter_deg(3),
      80.999,
      80.660
    ),
    current_status = c(
      rep("STOPPED_AT", 10),
      rep("IN_TRANSIT_TO", 3), # S1 transit pings must be ignored
      "STOPPED_AT", # NA stop_id must be ignored
      "STOPPED_AT" # S3 has n_obs = 1 -> dropped by min_obs
    ),
    stringsAsFactors = FALSE
  )

  expect_warning(
    stops <- g2g_stops_from_positions(positions),
    "dropped with fewer than 3"
  )

  expect_identical(stops$stop_id, c("S1", "S2"))
  expect_identical(stops$n_obs, c(5L, 5L))
  expect_equal(stops[stop_id == "S1", latitude], 7.300, tolerance = 1e-3)
  expect_equal(stops[stop_id == "S2", longitude], 80.650, tolerance = 1e-3)
})

test_that("g2g_stops_from_positions works without current_status", {
  positions <- data.frame(
    stop_id = rep("S1", 3),
    latitude = c(7.30, 7.30, 7.30),
    longitude = c(80.64, 80.64, 80.64)
  )
  expect_message(
    stops <- g2g_stops_from_positions(positions),
    "No 'current_status' column"
  )
  expect_identical(stops$n_obs, 3L)

  expect_error(
    g2g_stops_from_positions(data.frame(latitude = 1, longitude = 1)),
    "Missing required columns"
  )
  all_transit <- data.frame(
    stop_id = "S1",
    latitude = 7.3,
    longitude = 80.6,
    current_status = "IN_TRANSIT_TO"
  )
  expect_error(g2g_stops_from_positions(all_transit), "No usable pings")
})

test_that("g2g_shapes_from_trips builds ordered per-trip traces", {
  inputs <- make_route_inputs()
  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = make_rt_trajectory(),
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150,
    trip_col = "trip_id",
    return_trajectory = TRUE
  ))

  expect_true("trajectory" %in% names(result))
  shapes <- g2g_shapes_from_trips(result$trajectory)

  expect_identical(
    names(shapes),
    c(
      "shape_id",
      "shape_pt_lat",
      "shape_pt_lon",
      "shape_pt_sequence",
      "shape_dist_traveled"
    )
  )
  expect_identical(sort(unique(shapes$shape_id)), c("SHP_1", "SHP_2"))
  for (sid in unique(shapes$shape_id)) {
    trace <- shapes[shape_id == sid]
    expect_identical(trace$shape_pt_sequence, seq_len(nrow(trace)))
    expect_true(all(diff(trace$shape_dist_traveled) >= 0))
    expect_identical(trace$shape_dist_traveled[1], 0)
  }
  # ~4.6 km route: cumulative distance must be in a plausible range
  expect_gt(max(shapes$shape_dist_traveled), 3000)
  expect_lt(max(shapes$shape_dist_traveled), 7000)
})

test_that("pipeline shapes follow GPS instants through Berlin's repeated hour", {
  tz <- "Europe/Berlin"
  instants <- as.POSIXct(c(
    "2026-10-24 23:50:00", "2026-10-25 00:10:00", "2026-10-25 00:50:00",
    "2026-10-25 01:10:00", "2026-10-25 01:50:00", "2026-10-25 02:10:00"
  ), tz = "UTC")
  gps <- data.frame(
    vehicle_id = "V1",
    trip_id = "T1",
    direction_id = 0L,
    latitude = 52.5 + seq_along(instants) / 1000,
    longitude = 13.4 + seq_along(instants) / 1000,
    timestamp = .POSIXct(as.numeric(instants), tz = tz),
    speed = c(0, 20, 20, 20, 20, 0)
  )
  stops <- data.frame(
    stop_id = c("A", "B"),
    direction = 0L,
    latitude = gps$latitude[c(1, 6)],
    longitude = gps$longitude[c(1, 6)]
  )

  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = gps[c(6, 2, 5, 4, 1, 3), ],
    stops_data = stops,
    stops_buffer_radius = 60,
    stops_extended_buffer_radius = 90,
    trip_col = "trip_id",
    direction_col = "direction_id",
    return_trajectory = TRUE
  ))
  expect_identical(length(unique(result$trajectory$trip_id)), 1L)
  expect_identical(sort(as.numeric(result$trajectory$timestamp)), as.numeric(instants))
  expect_identical(
    format(gps$timestamp, "%H:%M %z", tz = tz),
    c("01:50 +0200", "02:10 +0200", "02:50 +0200",
      "02:10 +0100", "02:50 +0100", "03:10 +0100")
  )

  shapes <- g2g_shapes_from_trips(result$trajectory)
  expect_identical(shapes$shape_pt_sequence, seq_along(instants))
  expect_identical(shapes$shape_pt_lat, gps$latitude)
  expect_identical(shapes$shape_pt_lon, gps$longitude)
})

test_that("g2g_shapes_from_trips handles empty trajectories", {
  empty <- data.table::data.table(
    trip_id = integer(),
    latitude = double(),
    longitude = double(),
    date = character(),
    time_str = character()
  )
  shapes <- g2g_shapes_from_trips(empty)
  expect_identical(nrow(shapes), 0L)
  expect_identical(
    names(shapes),
    c(
      "shape_id",
      "shape_pt_lat",
      "shape_pt_lon",
      "shape_pt_sequence",
      "shape_dist_traveled"
    )
  )
})

test_that("g2g_shapes_from_trips uses local clocks without a timestamp column", {
  trajectory <- data.table::data.table(
    trip_id = 1L,
    latitude = c(52.503, 52.501, 52.502),
    longitude = c(13.403, 13.401, 13.402),
    date = c("2026-01-02", "2026-01-01", "2026-01-01"),
    time_str = c("00:10:00", "23:00:00", "23:50:00"),
    timestamp_reported = as.POSIXct("2026-01-02 14:00:00", tz = "UTC")
  )
  shapes <- g2g_shapes_from_trips(trajectory)
  expect_identical(shapes$shape_pt_lat, trajectory$latitude[c(2, 3, 1)])
  expect_identical(shapes$shape_pt_lon, trajectory$longitude[c(2, 3, 1)])
})
