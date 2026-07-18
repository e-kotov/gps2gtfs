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
