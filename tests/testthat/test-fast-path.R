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

test_that("fast path preserves the official trip id as provided_trip_id", {
  traj <- make_rt_trajectory()
  inputs <- make_route_inputs()

  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = traj,
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150,
    trip_col = "trip_id"
  ))

  expect_true("provided_trip_id" %in% names(result$trips))
  expect_true("provided_trip_id" %in% names(result$stop_times))
  expect_setequal(result$trips$provided_trip_id, c("CS_1", "CS_2"))

  # stop_times carries the official id of its own (internal) trip
  st <- result$stop_times
  map <- unique(result$trips[, .(trip_id, provided_trip_id)])
  st_expected <- map[st, on = "trip_id"]$provided_trip_id
  expect_identical(st$provided_trip_id, st_expected)
  expect_false(anyNA(st$provided_trip_id))
})

test_that("spatial path leaves provided_trip_id as NA", {
  traj <- make_rt_trajectory()
  traj$trip_id <- NULL
  inputs <- make_route_inputs()

  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = traj,
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150
  ))

  expect_true("provided_trip_id" %in% names(result$trips))
  expect_true(all(is.na(result$trips$provided_trip_id)))
  expect_true(all(is.na(result$stop_times$provided_trip_id)))
})

test_that("trip_col accepts multiple columns (GTFS-RT TripDescriptor)", {
  traj <- make_rt_trajectory() # has trip_id CS_1 / CS_2 and NA layover
  inputs <- make_route_inputs()

  # Split the single trip identity into two descriptor-like columns; their
  # combination reproduces the trip identity. NA trip_id -> NA components.
  traj$rt_route <- ifelse(is.na(traj$trip_id), NA, "R1")
  traj$rt_run <- sub("^CS_", "run", traj$trip_id) # run1 / run2 / NA

  single <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = make_rt_trajectory(), terminals_data = inputs$terminals,
    stops_data = inputs$stops, terminals_buffer_radius = 100,
    stops_buffer_radius = 100, stops_extended_buffer_radius = 150,
    trip_col = "trip_id"
  ))
  multi <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = traj, terminals_data = inputs$terminals,
    stops_data = inputs$stops, terminals_buffer_radius = 100,
    stops_buffer_radius = 100, stops_extended_buffer_radius = 150,
    trip_col = c("rt_route", "rt_run")
  ))

  # Same number of trips and stop_times as the single-column identity
  expect_identical(nrow(multi$trips), nrow(single$trips))
  expect_identical(
    multi$stop_times[order(trip_id, stop_id), .(direction, stop_id)],
    single$stop_times[order(trip_id, stop_id), .(direction, stop_id)]
  )
  # composite identity is carried through as provided_trip_id
  expect_true(all(grepl("^R1_run", multi$trips$provided_trip_id)))
})

test_that("multi-column trip_col reports missing columns", {
  traj <- make_rt_trajectory()
  inputs <- make_route_inputs()
  expect_error(
    suppressMessages(g2g_extract_trips_and_stop_times(
      gps_data = traj, terminals_data = inputs$terminals,
      stops_data = inputs$stops, terminals_buffer_radius = 100,
      stops_buffer_radius = 100, stops_extended_buffer_radius = 150,
      trip_col = c("trip_id", "nope")
    )),
    "not found in the GPS data: nope"
  )
})

test_that("direction_col drives direction; terminals optional; any topology", {
  # A one-way (single-direction) service: one trip, no return leg, no terminals.
  lat <- seq(7.290, 7.320, length.out = 5)
  lon <- seq(80.630, 80.660, length.out = 5)
  base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
  gps <- data.frame(
    vehicle_id = "b1",
    latitude = lat,
    longitude = lon,
    timestamp = base + c(0, 120, 240, 360, 480),
    speed = c(0, 15, 0, 15, 0),
    trip_id = "T1",
    direction_id = 0L
  )
  # stops labelled by the same direction values as direction_col (here "0")
  stops <- data.frame(
    stop_id = c("S1", "S2", "S3"),
    latitude = lat[2:4],
    longitude = lon[2:4],
    direction = "0"
  )

  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = gps,
    terminals_data = NULL,          # <- optional now
    stops_data = stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 120,
    stops_extended_buffer_radius = 200,
    trip_col = "trip_id",
    direction_col = "direction_id"
  ))

  expect_equal(nrow(result$trips), 1L)
  expect_equal(result$trips$direction, 1L)          # single group -> 1
  expect_equal(result$trips$provided_trip_id, "T1")
  expect_true(nrow(result$stop_times) > 0L)
  expect_true(all(result$stop_times$stop_id %in% c("S1", "S2", "S3")))
})

test_that("direction_col requires trip_col", {
  gps <- data.frame(vehicle_id = "b1", latitude = 7.3, longitude = 80.6,
                    timestamp = as.POSIXct("2026-06-06 08:00:00", tz = "UTC"),
                    speed = 0, direction_id = 0L)
  expect_error(
    suppressMessages(g2g_extract_trips_and_stop_times(
      gps_data = gps, stops_data = data.frame(stop_id="S1", latitude=7.3,
      longitude=80.6, direction="0"), terminals_buffer_radius = 100,
      stops_buffer_radius = 100, stops_extended_buffer_radius = 150,
      direction_col = "direction_id"
    )),
    "only used with 'trip_col'"
  )
})

test_that("no terminals and no direction_col errors", {
  gps <- data.frame(vehicle_id = "b1", latitude = 7.3, longitude = 80.6,
                    timestamp = as.POSIXct("2026-06-06 08:00:00", tz = "UTC"),
                    speed = 0, trip_id = "T1")
  expect_error(
    suppressMessages(g2g_extract_trips_and_stop_times(
      gps_data = gps, stops_data = data.frame(stop_id="S1", latitude=7.3,
      longitude=80.6, direction="A"), terminals_buffer_radius = 100,
      stops_buffer_radius = 100, stops_extended_buffer_radius = 150,
      trip_col = "trip_id"
    )),
    "terminals_data' is required"
  )
})
