# BT01 is Kandy, BT02 is Digana, so each label maps to the terminal its trips
# start from. The map is required: the pairing is not derivable from the data.
fixture_direction_map <- c("Kandy-Digana" = "BT01", "Digana-Kandy" = "BT02")

run_fixture <- function(backend = "auto",
                        stop_direction_map = fixture_direction_map) {
  g2g_extract_trips_and_stop_times(
    gps_data = g2g_data_gps,
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 50,
    stops_extended_buffer_radius = 100,
    backend = backend,
    stop_direction_map = stop_direction_map
  )
}

test_that("auto selects the best available backend and explicit requests are strict", {
  expected <- if (is_rust_available()) {
    "rust"
  } else if (is_rcpp_available()) {
    "rcpp"
  } else {
    "pure_r"
  }

  result <- run_fixture()
  expect_identical(attr(result, "backend"), expected)
  expect_identical(attr(result$trips, "backend"), expected)

  if (!is_rust_available()) {
    expect_error(run_fixture("rust"), "not available")
  }
  if (!is_rcpp_available()) {
    expect_error(run_fixture("rcpp"), "not available")
  }
})

test_that("CI modes select the required backend", {
  mode <- Sys.getenv("GPS2GTFS_CI_MODE")
  if (!nzchar(mode)) {
    skip("Not running in a backend CI matrix.")
  }

  expected <- switch(
    mode,
    full = "rust",
    `no-rust` = "rcpp",
    `pure-r` = "pure_r"
  )
  expect_identical(resolve_backend(), expected)
})

test_that("available backends produce equivalent trip identities and stop sequences", {
  backends <- c(
    if (is_rust_available()) "rust",
    if (is_rcpp_available()) "rcpp",
    if (requireNamespace("sf", quietly = TRUE)) "pure_r"
  )
  results <- lapply(backends, run_fixture)

  expect_equal(length(results), length(backends))
  for (i in seq_along(results)) {
    expect_identical(attr(results[[i]], "backend"), backends[[i]])
  }

  reference_trips <- results[[1]]$trips[, .(
    trip_id,
    vehicle_id,
    start_terminal,
    end_terminal,
    direction
  )]
  reference_stops <- results[[1]]$stop_times[, .(trip_id, direction, stop_id)]
  for (result in results[-1]) {
    expect_equal(
      result$trips[, .(
        trip_id,
        vehicle_id,
        start_terminal,
        end_terminal,
        direction
      )],
      reference_trips,
      ignore_attr = TRUE
    )
    expect_equal(
      result$stop_times[, .(trip_id, direction, stop_id)],
      reference_stops,
      ignore_attr = TRUE
    )
  }
})

test_that("returned row order is a stable, backend-invariant contract", {
  backends <- c(
    if (is_rust_available()) "rust",
    if (is_rcpp_available()) "rcpp",
    if (requireNamespace("sf", quietly = TRUE)) "pure_r"
  )
  if (length(backends) < 2L) {
    skip("Need at least two backends to compare row order.")
  }
  results <- lapply(backends, run_fixture)

  # The documented order holds within each backend's own result.
  for (result in results) {
    expect_identical(
      result$trips$trip_id,
      result$trips[order(trip_id)]$trip_id
    )
    key <- result$stop_times[, .(
      trip_id,
      arrival_time,
      stop_id,
      departure_time
    )]
    expect_identical(
      key,
      key[order(trip_id, arrival_time, stop_id, departure_time)]
    )
  }

  # Every backend returns whole tables with identical values and row order
  # (the per-run `backend` attribute aside, which `ignore_attr` drops).
  for (result in results[-1]) {
    expect_equal(result$trips, results[[1]]$trips, ignore_attr = TRUE)
    expect_equal(
      result$stop_times,
      results[[1]]$stop_times,
      ignore_attr = TRUE
    )
  }
})

test_that("device pairing accepts character IDs and never crosses devices", {
  bus_stops <- c("A", "B", "A", "B")
  dates <- rep("2026-06-06", 4)
  devices <- c("bus-a", "bus-b", "bus-a", "bus-a")
  expected <- c(0L, 0L, 1L, 1L)

  expect_identical(assign_trip_ids_pure_r(bus_stops, dates, devices), expected)
  if (is_rcpp_available()) {
    expect_identical(assign_trip_ids_cpp(bus_stops, dates, devices), expected)
  }
  if (is_rust_available()) {
    expect_identical(assign_trip_ids_rust(bus_stops, dates, devices), expected)
    expect_identical(
      assign_trip_ids_rust(character(), character(), character()),
      integer()
    )
  }
})

test_that("numeric device IDs retain their output type", {
  result <- run_fixture()
  expect_type(result$trips$vehicle_id, "integer")
})

test_that("character device IDs remain distinct and retain their output type", {
  gps <- data.table::copy(g2g_data_gps)
  gps[, vehicle_id := paste0("bus-", vehicle_id)]

  backends <- c(
    if (is_rust_available()) "rust",
    if (is_rcpp_available()) "rcpp",
    if (requireNamespace("sf", quietly = TRUE)) "pure_r"
  )
  for (backend in backends) {
    result <- g2g_extract_trips(
      gps_data = gps,
      terminals_data = g2g_data_terminals,
      terminals_buffer_radius = 100,
      backend = backend
    )
    expect_type(result$vehicle_id, "character")
    expect_setequal(unique(result$vehicle_id), unique(gps$vehicle_id))
  }
})

test_that("coordinate validation is strict and projected integers work", {
  valid <- data.frame(
    id = 1L,
    vehicle_id = "bus-a",
    latitude = 6.9,
    longitude = 79.9,
    timestamp = "2026-06-06 08:00:00",
    speed = 0
  )
  invalid <- valid
  invalid$latitude <- 500000

  expect_error(g2g_clean_gps(invalid), "Set 'projected = TRUE'")
  expect_error(
    g2g_clean_gps(invalid, projected = FALSE),
    "outside valid WGS-84"
  )
  expect_error(
    g2g_clean_gps(transform(valid, latitude = Inf)),
    "must not contain NA, NaN, or infinite"
  )
  expect_identical(attr(g2g_clean_gps(valid, tz = "UTC"), "projected"), FALSE)

  projected_gps <- data.frame(
    id = 1:2,
    vehicle_id = c("bus-a", "bus-a"),
    latitude = c(1000010L, 1000210L),
    longitude = c(500000L, 500000L),
    timestamp = c("2026-06-06 08:00:00", "2026-06-06 08:05:00"),
    speed = c(0, 0)
  )
  terminals <- data.frame(
    terminal_id = c("A", "B"),
    latitude = c(1000000L, 1000200L),
    longitude = c(500000L, 500000L)
  )
  cleaned <- g2g_clean_gps(projected_gps, projected = TRUE, tz = "UTC")

  if (is_rcpp_available()) {
    expect_equal(
      nrow(extract_trips_r(
        cleaned,
        terminals,
        30,
        projected_crs = 32644,
        backend = "rcpp",
        projected = TRUE
      )),
      2L
    )
  }
  if (is_rust_available()) {
    expect_equal(
      nrow(extract_trips_r(
        cleaned,
        terminals,
        30,
        projected_crs = 32644,
        backend = "rust",
        projected = TRUE
      )),
      2L
    )
  }
})

test_that("public projected pipelines require a CRS", {
  expect_error(
    g2g_extract_trips(
      gps_data = g2g_data_gps,
      terminals_data = g2g_data_terminals,
      terminals_buffer_radius = 100,
      projected = TRUE
    ),
    "projected_crs"
  )
})

test_that("empty inputs and no matches return stable typed schemas", {
  empty_gps <- g2g_data_gps[0]

  result <- g2g_extract_trips_and_stop_times(
    gps_data = empty_gps,
    terminals_data = g2g_data_terminals,
    stops_data = g2g_data_stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 50,
    stops_extended_buffer_radius = 100,
    # Inputs are validated before the empty-data early return, so a missing
    # direction map is still an error on a day with no pings.
    stop_direction_map = fixture_direction_map
  )
  expect_identical(names(result$trips), names(empty_trip_features()))
  expect_identical(names(result$stop_times), names(empty_stop_times()))
  expect_equal(nrow(result$trips), 0L)
  expect_equal(nrow(result$stop_times), 0L)

  cleaned <- g2g_clean_gps(
    data.frame(
      id = 1L,
      vehicle_id = "bus-a",
      latitude = 6.9,
      longitude = 79.9,
      timestamp = "2026-06-06 08:00:00",
      speed = 0
    ),
    tz = "UTC"
  )
  far_terminals <- data.frame(
    terminal_id = c("A", "B"),
    latitude = c(7.5, 7.6),
    longitude = c(80.5, 80.6)
  )
  backends <- c(
    if (is_rust_available()) "rust",
    if (is_rcpp_available()) "rcpp",
    if (requireNamespace("sf", quietly = TRUE)) "pure_r"
  )
  for (backend in backends) {
    trips <- extract_trips_r(cleaned, far_terminals, 10, backend = backend)
    features <- extract_trip_features_r(trips, c("A", "B"))
    trajectory <- prepare_trajectory_r(cleaned, trips, features, backend)
    expect_equal(nrow(features), 0L)
    expect_equal(nrow(trajectory), 0L)
  }
})

test_that("no stop matches return the documented empty schema for every backend", {
  stops_far <- data.table::copy(g2g_data_stops)
  stops_far[, `:=`(latitude = latitude + 1, longitude = longitude + 1)]

  backends <- c(
    if (is_rust_available()) "rust",
    if (is_rcpp_available()) "rcpp",
    if (requireNamespace("sf", quietly = TRUE)) "pure_r"
  )
  for (backend in backends) {
    result <- g2g_extract_trips_and_stop_times(
      gps_data = g2g_data_gps,
      terminals_data = g2g_data_terminals,
      stops_data = stops_far,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 50,
      stops_extended_buffer_radius = 100,
      stop_direction_map = fixture_direction_map,
      backend = backend
    )
    expect_identical(names(result$stop_times), names(empty_stop_times()))
    expect_equal(nrow(result$stop_times), 0L)
  }
})

test_that("stop directions accept text and numeric labels", {
  terminals <- c("A", "B")
  make_stops <- function(direction) {
    data.table::data.table(
      stop_id = c("s1", "s2"),
      direction = direction,
      latitude = c(6.9, 7),
      longitude = c(79.9, 80)
    )
  }

  # Labels of any type work, but the map is what pairs them to terminals -
  # order of appearance no longer does.
  expect_identical(
    resolve_stop_directions(
      make_stops(c("out", "back")),
      terminals,
      c(out = "A", back = "B")
    ),
    1:2
  )
  expect_identical(
    resolve_stop_directions(
      make_stops(c(0, 1)),
      terminals,
      c("0" = "A", "1" = "B")
    ),
    1:2
  )
  expect_identical(
    resolve_stop_directions(
      make_stops(c(1, 2)),
      terminals,
      c("1" = "A", "2" = "B")
    ),
    1:2
  )
  expect_identical(
    resolve_stop_directions(
      make_stops(c("out", "back")),
      terminals,
      c(out = "B", back = "A")
    ),
    2:1
  )
})

test_that("invalid stop direction maps produce actionable errors", {
  stops <- data.table::data.table(
    stop_id = c("s1", "s2"),
    direction = c("out", "back"),
    latitude = c(6.9, 7),
    longitude = c(79.9, 80)
  )
  expect_error(
    resolve_stop_directions(stops, c("A", "B"), c(out = "A")),
    "does not cover"
  )
  expect_error(
    resolve_stop_directions(stops, c("A", "B"), c(out = "A", back = "C")),
    "unknown terminal"
  )
  expect_error(
    resolve_stop_directions(stops, c("A", "B"), c(out = "A", back = "A")),
    "exactly two"
  )
})

test_that("the direction map is load-bearing, not decorative", {
  # Swapping it must change which stops each direction's trips are matched
  # against. If this ever stops being true, the map has quietly become inert
  # and the argument is no longer protecting anything.
  correct <- run_fixture()
  swapped <- run_fixture(
    stop_direction_map = c(
      "Kandy-Digana" = "BT02",
      "Digana-Kandy" = "BT01"
    )
  )

  expect_true(nrow(correct$stop_times) > 0L)
  key <- function(x) paste(x$trip_id, x$stop_id)
  expect_false(setequal(key(correct$stop_times), key(swapped$stop_times)))
})

test_that("an underivable direction pairing is refused, not guessed", {
  # Which of "Kandy-Digana"/"Digana-Kandy" starts at BT01 is not in the data:
  # both groups span the same corridor. Pairing them by order of appearance
  # would be a coin flip that silently reverses every direction.
  err <- tryCatch(
    run_fixture(stop_direction_map = NULL),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "not recoverable from the data", fixed = TRUE)
  # Both candidate maps are offered, ready to paste.
  expect_match(
    err,
    'stop_direction_map = c("Kandy-Digana" = "BT01", "Digana-Kandy" = "BT02")',
    fixed = TRUE
  )
  expect_match(
    err,
    'stop_direction_map = c("Kandy-Digana" = "BT02", "Digana-Kandy" = "BT01")',
    fixed = TRUE
  )
})

test_that("labels that are terminal ids still need no map", {
  # g2g_stops_from_gtfs() labels directions with the starting terminal_id, so
  # the mapping is already unambiguous and must keep working unprompted.
  stops <- as.data.frame(g2g_data_stops)
  stops$direction <- ifelse(
    stops$direction == "Kandy-Digana",
    "BT01",
    "BT02"
  )
  expect_no_error(
    result <- suppressMessages(g2g_extract_trips_and_stop_times(
      gps_data = g2g_data_gps,
      terminals_data = g2g_data_terminals,
      stops_data = stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 50,
      stops_extended_buffer_radius = 100,
      stop_direction_map = NULL
    ))
  )
  expect_true(nrow(result$stop_times) > 0L)
})
