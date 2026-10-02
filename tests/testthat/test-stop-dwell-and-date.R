# Stop-visit filtering by dwell (min_dwell) and the date of a visit.

# One trip on a north-south line (1 m of latitude is 1 / 111195 degree at the
# package's haversine radius) past three stops 500 m apart, each with a 100 m
# buffer. The vehicle stands still at S1 and S3 for 20 s, leaves the buffer
# 10 s after its last stationary ping (so dwell is 30 s), and crosses S2's
# buffer at 7 m/s without a zero-speed ping (a pass-through). `t0` is the
# first ping's time.
dwell_fixture <- function(t0 = as.POSIXct("2026-06-06 08:00:00", tz = "UTC")) {
  lat <- function(m) 52 + m / 111195
  stopped <- data.frame(
    sec = c(-20, -10, 0, 10, 20, 30, 40),
    x = c(-140, -70, 0, 0, 0, 70, 140),
    speed = c(7, 7, 0, 0, 0, 7, 7)
  )
  passing <- data.frame(
    sec = c(-20, -10, 0, 10, 20),
    x = c(-140, -70, 0, 70, 140),
    speed = 7
  )
  visit <- function(v, at_sec, at_m) {
    v$sec <- v$sec + at_sec
    v$x <- v$x + at_m
    v
  }
  pings <- rbind(
    visit(stopped, 20, 0),
    visit(passing, 300, 500),
    visit(stopped, 600, 1000)
  )
  list(
    gps = data.frame(
      vehicle_id = "V1",
      trip_id = "T1",
      direction_id = 0L,
      latitude = lat(pings$x),
      longitude = 9,
      speed = pings$speed,
      timestamp = t0 + pings$sec
    ),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3"),
      direction = 0L,
      latitude = lat(c(0, 500, 1000)),
      longitude = 9
    ),
    t0 = t0
  )
}

run_dwell_fixture <- function(fx, ...) {
  g2g_extract_trips_and_stop_times(
    gps_data = fx$gps,
    stops_data = fx$stops,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 100,
    projected = FALSE,
    trip_col = "trip_id",
    direction_col = "direction_id",
    tz = "UTC",
    diagnostics_warn = FALSE,
    ...
  )
}

test_that("a pass-through is kept with zero dwell by default", {
  fx <- dwell_fixture()
  st <- suppressMessages(run_dwell_fixture(fx))$stop_times
  expect_identical(st$stop_id, c("S1", "S2", "S3"))
  expect_identical(st$dwell_time_in_seconds, c(30, 0, 30))
})

test_that("min_dwell drops pass-throughs and short stops, and says so", {
  fx <- dwell_fixture()
  expect_message(
    res <- run_dwell_fixture(fx, min_dwell = 1),
    "Dropped 1 stop visit\\(s\\) with dwell below 'min_dwell' \\(1 s\\)"
  )
  st <- res$stop_times
  expect_identical(st$stop_id, c("S1", "S3"))
  expect_identical(st$dwell_time_in_seconds, c(30, 30))
  expect_identical(
    as.numeric(st$arrival_time - fx$t0, units = "secs"),
    c(20, 600)
  )
  # A dwell equal to min_dwell is kept.
  st <- suppressMessages(run_dwell_fixture(fx, min_dwell = 30))$stop_times
  expect_identical(st$stop_id, c("S1", "S3"))
  # Dropping every visit returns the empty stop_times shape.
  st <- suppressMessages(run_dwell_fixture(fx, min_dwell = 31))$stop_times
  expect_identical(nrow(st), 0L)
  expect_identical(names(st), names(empty_stop_times()))
  # Diagnostics count the kept rows.
  res <- suppressMessages(run_dwell_fixture(fx, min_dwell = 1))
  expect_identical(diagnostics_value(g2g_diagnostics(res), "stop_times_kept"), 2L)
})

test_that("min_dwell = 0 changes nothing", {
  fx <- dwell_fixture()
  a <- suppressMessages(run_dwell_fixture(fx))
  b <- suppressMessages(run_dwell_fixture(fx, min_dwell = 0))
  expect_identical(a$stop_times, b$stop_times)
})

test_that("min_dwell is validated", {
  fx <- dwell_fixture()
  for (bad in list(-1, NA_real_, Inf, c(1, 2), "1", NULL)) {
    expect_error(
      run_dwell_fixture(fx, min_dwell = bad),
      "'min_dwell' must be one non-negative finite number of seconds"
    )
  }
})

test_that("min_dwell warns when speed is entirely NA", {
  fx <- dwell_fixture()
  fx$gps$speed <- NA_real_
  # g2g_clean_gps() warns about the NA speed too; collect both warnings.
  warned <- character()
  res <- withCallingHandlers(
    suppressMessages(run_dwell_fixture(fx, min_dwell = 1)),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("'min_dwell' drops every stop visit", warned)))
  expect_identical(nrow(res$stop_times), 0L)
  # Without min_dwell only the cleaner's warning is raised.
  warned <- character()
  withCallingHandlers(
    suppressMessages(run_dwell_fixture(fx)),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("min_dwell", warned)))
})

test_that("a visit entered just before midnight is dated by its arrival", {
  # S1's buffer is entered at 23:59:50 (the -10 s ping) and the vehicle stops
  # at 00:00:00 on the next day; S2 and S3 are on that next day too.
  fx <- dwell_fixture(t0 = as.POSIXct("2026-06-06 23:59:40", tz = "UTC"))
  st <- suppressMessages(run_dwell_fixture(fx))$stop_times
  expect_identical(st$stop_id, c("S1", "S2", "S3"))
  expect_identical(format(st$arrival_time[1], "%F %T"), "2026-06-07 00:00:00")
  expect_identical(st$date, rep("2026-06-07", 3))
  expect_identical(as.character(st$day_of_week), rep("Sunday", 3))
  expect_identical(st$is_weekday, rep(FALSE, 3))
  expect_identical(st$hour_of_day, rep(0L, 3))
  # The column keeps its place in the table.
  expect_identical(names(st)[1:4], c("trip_id", "vehicle_id", "date", "direction"))
})

test_that("a visit's date follows the timestamps' timezone", {
  # 22:00 UTC is 00:00 the next day in Europe/Berlin (CEST, UTC+2).
  fx <- dwell_fixture(t0 = as.POSIXct("2026-06-06 21:59:40", tz = "UTC"))
  fx$gps$timestamp <- as.POSIXct(fx$gps$timestamp, tz = "Europe/Berlin")
  res <- suppressMessages(g2g_extract_trips_and_stop_times(
    gps_data = fx$gps,
    stops_data = fx$stops,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 100,
    projected = FALSE,
    trip_col = "trip_id",
    direction_col = "direction_id",
    diagnostics_warn = FALSE
  ))
  expect_identical(res$stop_times$date, rep("2026-06-07", 3))
  expect_identical(res$stop_times$hour_of_day, rep(0L, 3))
})
