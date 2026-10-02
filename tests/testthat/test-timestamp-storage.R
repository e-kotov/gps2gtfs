# Timestamp storage regressions. Integer epoch seconds (what fread reads) and
# POSIXct built from them have integer storage; the stop extractor's departure
# estimate is double on the `+ 15` branch and input-typed on the others, and
# data.table refuses a grouped result whose column storage differs between
# groups. Every time column must therefore be double regardless of input, and
# the epoch values must be unchanged.

# One trip, two stop visits on a north-south line (1 m of latitude is
# 1 / 111195 degree at the package's haversine radius). Both visits stand still
# (speed 0) for 10 s. The "short" visit leaves the 100 m stop buffer 10 s after
# its last stationary ping, so its departure is the last in-buffer ping, in the
# storage of the input; the "long" visit crawls inside the buffer for 50 s
# more, so its departure is the last stationary ping + 15 s, which is always
# double. The visit order decides which storage data.table sees first.
storage_fixture <- function(visit_order = c("short_first", "long_first")) {
  visit_order <- match.arg(visit_order)
  t0 <- as.integer(as.POSIXct("2026-06-06 08:00:00", tz = "UTC"))
  lat <- function(m) 52 + m / 111195

  short <- data.frame(
    sec = c(-20, -10, 0, 5, 10, 15, 20),
    x = c(-140, -70, 0, 0, 0, 35, 70),
    speed = c(7, 7, 0, 0, 0, 7, 7)
  )
  long <- data.frame(
    sec = c(-20, -10, 0, 5, 10, 20, 30, 40, 50, 60),
    x = c(-140, -70, 0, 0, 0, 10, 20, 30, 40, 50),
    speed = c(7, 7, 0, 0, 0, 1, 1, 1, 1, 1)
  )
  short_first <- visit_order == "short_first"
  first <- if (short_first) short else long
  second <- if (short_first) long else short

  # First visit at S1 (0 m) 100 s after t0, second at S2 (500 m) 400 s after.
  first$sec <- first$sec + 100
  second$sec <- second$sec + 400
  second$x <- second$x + 500
  pings <- rbind(first, second)

  list(
    epoch = t0 + as.integer(pings$sec),
    gps = data.frame(
      vehicle_id = "V1",
      trip_id = "T1",
      direction_id = 0L,
      latitude = lat(pings$x),
      longitude = 9,
      speed = pings$speed
    ),
    stops = data.frame(
      stop_id = c("S1", "S2"),
      direction = 0L,
      latitude = lat(c(0, 500)),
      longitude = 9
    ),
    arrival = t0 + c(100, 400),
    departure = t0 + c(100, 400) + if (short_first) c(20, 25) else c(25, 20)
  )
}

run_storage_fixture <- function(fx, timestamp) {
  gps <- fx$gps
  gps$timestamp <- timestamp
  suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = gps,
    stops_data = fx$stops,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 100,
    projected = FALSE,
    trip_col = "trip_id",
    direction_col = "direction_id",
    tz = "UTC",
    diagnostics_warn = FALSE
  ))
}

expect_double_stop_times <- function(result, fx) {
  st <- result$stop_times[order(arrival_time)]
  expect_identical(st$stop_id, c("S1", "S2"))
  expect_s3_class(st$arrival_time, "POSIXct")
  expect_s3_class(st$departure_time, "POSIXct")
  expect_type(st$arrival_time, "double")
  expect_type(st$departure_time, "double")
  expect_identical(attr(st$departure_time, "tzone"), "UTC")
  # The storage normalisation must not move a single second.
  expect_identical(as.numeric(st$arrival_time), as.numeric(fx$arrival))
  expect_identical(as.numeric(st$departure_time), as.numeric(fx$departure))
  expect_type(result$trips$start_time, "double")
  expect_type(result$trips$end_time, "double")
}

test_that("integer epoch timestamps survive mixed departure branches", {
  for (visit_order in c("short_first", "long_first")) {
    fx <- storage_fixture(visit_order)
    expect_type(fx$epoch, "integer")
    expect_no_error(result <- run_storage_fixture(fx, fx$epoch))
    expect_double_stop_times(result, fx)
  }
})

test_that("integer-storage POSIXct timestamps survive mixed departure branches", {
  for (visit_order in c("short_first", "long_first")) {
    fx <- storage_fixture(visit_order)
    ts <- as.POSIXct(fx$epoch, tz = "UTC")
    expect_type(ts, "integer")
    expect_no_error(result <- run_storage_fixture(fx, ts))
    expect_double_stop_times(result, fx)
  }
})

test_that("g2g_clean_gps stores timestamps as double without moving them", {
  epoch <- as.integer(as.POSIXct("2026-06-06 08:00:00", tz = "UTC")) +
    c(0L, 60L, 120L)
  pings <- function(timestamp) {
    data.frame(
      vehicle_id = "V1",
      latitude = 52,
      longitude = 9,
      speed = 0,
      timestamp = timestamp
    )
  }

  cleaned <- g2g_clean_gps(pings(epoch), tz = "Europe/Berlin")
  expect_type(cleaned$timestamp, "double")
  expect_identical(as.numeric(cleaned$timestamp), as.numeric(epoch))
  expect_identical(attr(cleaned$timestamp, "tzone"), "Europe/Berlin")

  ts_int <- as.POSIXct(epoch, tz = "Europe/Berlin")
  expect_type(ts_int, "integer")
  cleaned <- g2g_clean_gps(pings(ts_int))
  expect_type(cleaned$timestamp, "double")
  expect_identical(as.numeric(cleaned$timestamp), as.numeric(epoch))
  expect_identical(attr(cleaned$timestamp, "tzone"), "Europe/Berlin")

  # Double input is left exactly as it was.
  ts_dbl <- as.POSIXct(as.double(epoch), tz = "UTC")
  cleaned <- g2g_clean_gps(pings(ts_dbl))
  expect_identical(cleaned$timestamp, ts_dbl)
})
