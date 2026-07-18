make_rt_trajectory <- function() {
  # Two trips of one vehicle with GTFS-RT trip_id annotations:
  # trip CS_1 runs A -> B, trip CS_2 runs B -> A. One un-annotated ping
  # in between (layover) must not break segmentation.
  lat <- seq(7.290, 7.320, length.out = 4)
  lon <- seq(80.630, 80.660, length.out = 4)
  base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")

  data.frame(
    vehicle_id = "7482",
    latitude = c(lat, 7.321, rev(lat)),
    longitude = c(lon, 80.661, rev(lon)),
    timestamp = base + c(0, 300, 600, 900, 1200, 1800, 2100, 2400, 2700),
    speed = c(0, 20, 20, 0, 0, 0, 20, 20, 0),
    trip_id = c(rep("CS_1", 4), NA, rep("CS_2", 4)),
    stringsAsFactors = FALSE
  )
}

make_route_inputs <- function() {
  lat <- seq(7.290, 7.320, length.out = 4)
  lon <- seq(80.630, 80.660, length.out = 4)
  list(
    terminals = data.frame(
      terminal_id = c("A", "B"),
      latitude = lat[c(1, 4)],
      longitude = lon[c(1, 4)]
    ),
    stops = data.frame(
      stop_id = c("S1", "S2", "S1r", "S2r"),
      latitude = lat[c(2, 3, 3, 2)],
      longitude = lon[c(2, 3, 3, 2)],
      direction = c("A", "A", "B", "B")
    )
  )
}

test_that("trip_col fast path segments by supplied trip identities", {
  inputs <- make_route_inputs()

  expect_message(
    result <- g2g_extract_trips_and_stop_times(
      gps_data = make_rt_trajectory(),
      terminals_data = inputs$terminals,
      stops_data = inputs$stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 100,
      stops_extended_buffer_radius = 150,
      trip_col = "trip_id"
    ),
    "fast path"
  )

  expect_identical(nrow(result$trips), 2L)
  expect_identical(result$trips$start_terminal, c("A", "B"))
  expect_identical(result$trips$end_terminal, c("B", "A"))
  expect_identical(result$trips$direction, c(1L, 2L))
  expect_true(nrow(result$stop_times) > 0L)
})

test_that("fast path and spatial inference agree on this fixture", {
  inputs <- make_route_inputs()
  traj <- make_rt_trajectory()

  run <- function(trip_col) {
    suppressMessages(g2g_extract_trips_and_stop_times(
      gps_data = traj,
      terminals_data = inputs$terminals,
      stops_data = inputs$stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 100,
      stops_extended_buffer_radius = 150,
      trip_col = trip_col
    ))
  }
  fast <- run("trip_id")
  spatial <- run(NULL)

  cols <- c("start_terminal", "end_terminal", "direction")
  expect_equal(fast$trips[, ..cols], spatial$trips[, ..cols])
  expect_equal(
    fast$stop_times[, .(trip_id, direction, stop_id)],
    spatial$stop_times[, .(trip_id, direction, stop_id)]
  )
})

test_that("fast path handles missing/empty trip columns", {
  inputs <- make_route_inputs()

  expect_error(
    g2g_extract_trips_and_stop_times(
      gps_data = make_rt_trajectory(),
      terminals_data = inputs$terminals,
      stops_data = inputs$stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 100,
      stops_extended_buffer_radius = 150,
      trip_col = "nope"
    ),
    "not found in the GPS data"
  )

  all_na <- make_rt_trajectory()
  all_na$trip_id <- NA_character_
  expect_message(
    result <- g2g_extract_trips_and_stop_times(
      gps_data = all_na,
      terminals_data = inputs$terminals,
      stops_data = inputs$stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 100,
      stops_extended_buffer_radius = 150,
      trip_col = "trip_id"
    ),
    "no usable trip identities"
  )
  expect_identical(nrow(result$trips), 0L)
})

test_that("single-ping segments are skipped with a message", {
  inputs <- make_route_inputs()
  traj <- make_rt_trajectory()
  traj$trip_id[5] <- "CS_stub" # the layover ping becomes a 1-ping segment

  expect_message(
    result <- g2g_extract_trips_and_stop_times(
      gps_data = traj,
      terminals_data = inputs$terminals,
      stops_data = inputs$stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 100,
      stops_extended_buffer_radius = 150,
      trip_col = "trip_id"
    ),
    "single-ping trip segment"
  )
  expect_identical(nrow(result$trips), 2L)
})
