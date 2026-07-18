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
