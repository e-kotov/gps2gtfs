make_pings <- function(...) {
  base <- data.frame(
    id = c("1", "2"),
    vehicle_id = c("bus-a", "bus-a"),
    latitude = c(40.7128, 40.7129),
    longitude = c(-74.0060, -74.0061),
    timestamp = as.POSIXct(
      c("2026-07-15 12:00:00", "2026-07-15 12:01:00"),
      tz = "UTC"
    ),
    speed = c(10, 12),
    stringsAsFactors = FALSE
  )
  modifyList(base, list(...))
}

test_that("GTFS-RT-shaped input is accepted natively, extras pass through", {
  positions <- data.frame(
    id = c("MTA_1", "MTA_2"),
    latitude = c(40.7128, 40.7129),
    longitude = c(-74.0060, -74.0061),
    timestamp = as.POSIXct(
      c("2026-07-15 12:00:00", "2026-07-15 12:01:00"),
      tz = "UTC"
    ),
    speed = c(0, 6.4),
    vehicle_id = c("7482", "7482"),
    trip_id = c("CS_B62_0630", "CS_B62_0630"),
    route_id = c("B62", "B62"),
    current_status = c("STOPPED_AT", "IN_TRANSIT_TO"),
    stringsAsFactors = FALSE
  )

  cleaned <- g2g_clean_gps(positions)

  expect_s3_class(cleaned, "data.table")
  expect_identical(cleaned$vehicle_id, c("7482", "7482"))
  expect_identical(cleaned$trip_id, c("CS_B62_0630", "CS_B62_0630"))
  expect_identical(cleaned$route_id, c("B62", "B62"))
  expect_identical(cleaned$current_status, c("STOPPED_AT", "IN_TRANSIT_TO"))
  expect_true(all(c("date", "time_str") %in% names(cleaned)))
})

test_that("custom column names map onto the canonical schema", {
  avl <- data.frame(
    bus_no = c("12", "12"),
    latitude = c(6.9, 6.91),
    longitude = c(79.9, 79.91),
    gps_time = c("2026-06-06 08:00:00", "2026-06-06 08:00:30"),
    speed = c(0, 20),
    stringsAsFactors = FALSE
  )

  cleaned <- g2g_clean_gps(
    avl,
    vehicle_col = "bus_no",
    time_col = "gps_time",
    tz = "UTC"
  )

  expect_identical(cleaned$vehicle_id, c("12", "12"))
  expect_s3_class(cleaned$timestamp, "POSIXct")
  expect_false("bus_no" %in% names(cleaned))
  expect_false("gps_time" %in% names(cleaned))
})

test_that("missing mapped columns and name collisions error clearly", {
  pings <- make_pings()
  expect_error(
    g2g_clean_gps(pings, vehicle_col = "nope"),
    "Missing required columns"
  )
  pings_conflict <- cbind(make_pings(), bus_no = c("x", "y"))
  expect_error(
    g2g_clean_gps(pings_conflict, vehicle_col = "bus_no"),
    "both 'bus_no' and 'vehicle_id'"
  )
  expect_error(
    g2g_clean_gps(make_pings(), vehicle_col = c("a", "b")),
    "single column names"
  )
})

test_that("rows with missing vehicle ids are dropped with a warning", {
  pings <- make_pings(vehicle_id = c("bus-a", NA))

  expect_warning(cleaned <- g2g_clean_gps(pings), "missing/empty 'vehicle_id'")
  expect_identical(nrow(cleaned), 1L)
  expect_identical(cleaned$vehicle_id, "bus-a")

  expect_error(
    g2g_clean_gps(pings, drop_missing_vehicle = FALSE),
    "must not contain missing or empty"
  )
})

test_that("duplicated (vehicle_id, timestamp) rows are deduplicated", {
  pings <- make_pings(
    id = c("1", "1"),
    timestamp = as.POSIXct(rep("2026-07-15 12:00:00", 2), tz = "UTC")
  )

  expect_message(cleaned <- g2g_clean_gps(pings), "duplicated")
  expect_identical(nrow(cleaned), 1L)

  kept <- suppressMessages(g2g_clean_gps(pings, dedupe = FALSE))
  expect_identical(nrow(kept), 2L)
})

test_that("row ids are generated when absent or not unique", {
  no_id <- make_pings()
  no_id$id <- NULL
  cleaned <- g2g_clean_gps(no_id)
  expect_identical(cleaned$id, seq_len(2L))

  dup_id <- make_pings(id = c("x", "x"))
  expect_message(cleaned2 <- g2g_clean_gps(dup_id), "regenerating row ids")
  expect_identical(anyDuplicated(cleaned2$id), 0L)
})

test_that("missing or all-NA speed warns and degrades gracefully", {
  no_speed <- make_pings()
  no_speed$speed <- NULL
  expect_warning(cleaned <- g2g_clean_gps(no_speed), "No 'speed' column")
  expect_true(all(is.na(cleaned$speed)))

  na_speed <- make_pings(speed = c(NA_real_, NA_real_))
  expect_warning(g2g_clean_gps(na_speed), "entirely NA")
})

test_that("POSIXct timestamps keep their timezone through cleaning", {
  local <- make_pings(
    timestamp = as.POSIXct(
      c("2026-07-14 23:30:00", "2026-07-15 00:30:00"),
      tz = "America/New_York"
    )
  )
  cleaned <- g2g_clean_gps(local)

  expect_identical(attr(cleaned$timestamp, "tzone"), "America/New_York")
  # Service day splits at feed-local midnight, not UTC midnight
  expect_identical(sort(unique(cleaned$date)), c("2026-07-14", "2026-07-15"))
})

test_that("zero-row input passes through cleanly", {
  empty <- make_pings()[0, ]
  cleaned <- suppressWarnings(g2g_clean_gps(empty))
  expect_identical(nrow(cleaned), 0L)
  expect_true(all(
    c("id", "vehicle_id", "timestamp", "speed") %in% names(cleaned)
  ))
})

test_that("pipeline accepts column mapping end to end", {
  gps <- data.table::copy(g2g_data_gps)
  data.table::setnames(gps, c("vehicle_id", "timestamp"), c("bus", "seen_at"))

  result <- g2g_extract_trips(
    gps_data = gps,
    terminals_data = g2g_data_terminals,
    terminals_buffer_radius = 100,
    vehicle_col = "bus",
    time_col = "seen_at"
  )
  expect_true(nrow(result) > 0L)
  expect_true("vehicle_id" %in% names(result))
})

test_that("character timestamps: tz is honored and its absence warns", {
  pings <- data.frame(
    vehicle_id = "bus-a",
    latitude = 6.9,
    longitude = 79.9,
    timestamp = "2026-06-06 23:30:00",
    speed = 0
  )

  # No tz -> warns and parses as UTC
  expect_warning(
    cleaned_utc <- g2g_clean_gps(pings),
    "parsing as UTC"
  )
  expect_identical(attr(cleaned_utc$timestamp, "tzone"), "UTC")

  # Explicit tz -> silent, and that timezone is used (service day is local)
  expect_silent(
    cleaned_local <- g2g_clean_gps(pings, tz = "America/New_York")
  )
  expect_identical(attr(cleaned_local$timestamp, "tzone"), "America/New_York")
  expect_identical(cleaned_local$date, "2026-06-06")

  # tz = "UTC" explicitly silences the warning
  expect_silent(g2g_clean_gps(pings, tz = "UTC"))
})

test_that("tz is validated", {
  pings <- data.frame(
    vehicle_id = "bus-a", latitude = 6.9, longitude = 79.9,
    timestamp = "2026-06-06 08:00:00", speed = 0
  )
  expect_error(g2g_clean_gps(pings, tz = c("UTC", "GMT")), "single timezone")
  expect_error(g2g_clean_gps(pings, tz = 5), "single timezone")
})
