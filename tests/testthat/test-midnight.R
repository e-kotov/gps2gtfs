test_that("overnight trips keep absolute POSIXct times across midnight", {
  tz <- "Asia/Colombo"
  base <- as.POSIXct("2026-06-06 23:50:00", tz = tz)
  lat <- seq(7.290, 7.320, length.out = 4)
  lon <- seq(80.630, 80.660, length.out = 4)

  # Terminal A (23:50) -> S1 (23:55) -> S2 (00:05, next day) -> terminal B
  gps <- data.frame(
    vehicle_id = "7482",
    latitude = lat,
    longitude = lon,
    timestamp = base + c(0, 300, 900, 1500),
    speed = 0
  )
  inputs <- make_route_inputs()

  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = gps,
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150
  ))

  trips <- result$trips
  expect_equal(nrow(trips), 1L)
  expect_s3_class(trips$start_time, "POSIXct")
  expect_s3_class(trips$end_time, "POSIXct")
  expect_equal(as.numeric(trips$end_time - trips$start_time, units = "mins"), 25)

  st <- result$stop_times[order(arrival_time)]
  expect_equal(st$stop_id, c("S1", "S2"))
  expect_equal(length(unique(st$trip_id)), 1L)
  expect_s3_class(st$arrival_time, "POSIXct")
  expect_s3_class(st$departure_time, "POSIXct")
  expect_identical(attr(st$arrival_time, "tzone"), tz)

  # The S2 visit is 10 minutes after S1 even though its clock time (00:05)
  # is "earlier"; formatted strings would have wrapped at midnight.
  expect_equal(st$arrival_time, base + c(300, 900))
  expect_true(st$arrival_time[2] > st$arrival_time[1])
  expect_equal(st$hour_of_day, c(23L, 0L))
  expect_equal(st$date, c("2026-06-06", "2026-06-07"))
})

test_that("fast path keeps an annotated trip together across midnight", {
  tz <- "Asia/Colombo"
  base <- as.POSIXct("2026-06-06 23:50:00", tz = tz)
  lat <- seq(7.290, 7.320, length.out = 4)
  lon <- seq(80.630, 80.660, length.out = 4)

  gps <- data.frame(
    vehicle_id = "7482",
    latitude = lat,
    longitude = lon,
    timestamp = base + c(0, 300, 900, 1500),
    speed = 0,
    trip_id = "CS_1"
  )
  inputs <- make_route_inputs()

  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = gps,
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150,
    trip_col = "trip_id"
  ))

  expect_equal(nrow(result$trips), 1L)
  st <- result$stop_times[order(arrival_time)]
  expect_equal(st$stop_id, c("S1", "S2"))
  expect_equal(st$arrival_time, base + c(300, 900))
})

test_that("session_gap still separates operations across an overnight break", {
  tz <- "Asia/Colombo"
  base <- as.POSIXct("2026-06-06 22:00:00", tz = tz)
  lat <- seq(7.290, 7.320, length.out = 4)
  lon <- seq(80.630, 80.660, length.out = 4)

  # Evening run A -> B, vehicle parks overnight (8 h), morning run B -> A.
  # The A-exit and B-entry across the parking gap must not pair into a trip.
  gps <- data.frame(
    vehicle_id = "7482",
    latitude = c(lat, rev(lat)),
    longitude = c(lon, rev(lon)),
    timestamp = base +
      c(0, 300, 900, 1500, 8 * 3600 + c(0, 300, 900, 1500)),
    speed = 0
  )
  inputs <- make_route_inputs()

  result <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = gps,
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150
  ))

  trips <- result$trips[order(start_time)]
  expect_equal(nrow(trips), 2L)
  expect_equal(trips$start_terminal, c("A", "B"))
  expect_equal(trips$end_terminal, c("B", "A"))
  expect_equal(trips$direction, c(1L, 2L))
})
