test_that("weekday features are ISO compliant and Monday-first", {
  features <- make_weekday_features(c("2024-01-01", "2024-01-06"))

  expect_equal(as.character(features$day_of_week), c("Monday", "Saturday"))
  expect_equal(as.integer(features$day_of_week), c(1L, 6L))
  expect_equal(features$is_weekday, c(TRUE, FALSE))
  expect_equal(levels(features$day_of_week), weekday_levels)
  expect_s3_class(features$day_of_week, "ordered")
})

test_that("trip extraction uses weekday feature schema", {
  trips <- data.table::data.table(
    trip_id = c(1L, 1L, 2L, 2L),
    deviceid = "bus-a",
    date = rep(c("2024-01-01", "2024-01-06"), each = 2),
    bus_stop = rep(c("A", "B"), 2),
    devicetime = as.POSIXct(
      c(
        "2024-01-01 08:00:00",
        "2024-01-01 08:30:00",
        "2024-01-06 09:00:00",
        "2024-01-06 09:30:00"
      ),
      tz = "UTC"
    )
  )
  trips[, time_str := format(devicetime, "%H:%M:%S")]

  features <- extract_trip_features_r(trips, terminal_ids = c("A", "B"))

  expect_identical(names(features), names(empty_trip_features()))
  expect_equal(as.character(features$day_of_week), c("Monday", "Saturday"))
  expect_equal(as.integer(features$day_of_week), c(1L, 6L))
  expect_equal(features$is_weekday, c(TRUE, FALSE))
  expect_type(features$is_weekday, "logical")
  expect_s3_class(features$day_of_week, "ordered")
})

test_that("empty weekday schemas match non-empty output types", {
  expect_equal(levels(empty_trip_features()$day_of_week), weekday_levels)
  expect_equal(levels(empty_stop_times()$day_of_week), weekday_levels)
  expect_s3_class(empty_trip_features()$day_of_week, "ordered")
  expect_s3_class(empty_stop_times()$day_of_week, "ordered")
  expect_type(empty_trip_features()$is_weekday, "logical")
  expect_type(empty_stop_times()$is_weekday, "logical")
})
