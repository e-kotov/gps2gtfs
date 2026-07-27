# g2g_clean_gps() passes unknown GPS columns through untouched, so a caller's
# own column can carry a name that the stop-matching stage uses internally.
# Before the fix, a column literally named `direction` collided with the
# trip-level direction merged in by prepare_trajectory_r(): the merge produced
# `direction.x`/`direction.y`, every direction group then matched zero pings,
# and extraction died with an error naming neither the column nor the cause.
# It fired on every segmentation mode, not just the supplied-identity path.

# make_rt_trajectory() plus one passthrough column per reserved name. Values
# are constant within each annotated trip, so nothing here is a direction
# conflict - the point is purely the column *name*.
collide_trajectory <- function(cols = "direction") {
  gps <- make_rt_trajectory()
  trip <- gps$trip_id
  for (nm in cols) {
    gps[[nm]] <- ifelse(is.na(trip) | trip == "CS_1", "0", "1")
  }
  gps
}

run_pipeline <- function(gps, ...) {
  inputs <- make_route_inputs()
  g2g_extract_trips_and_stop_times(
    gps_data = gps,
    terminals_data = inputs$terminals,
    stops_data = inputs$stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150,
    ...
  )
}

test_that("a passthrough column named 'direction' does not break extraction", {
  # Terminal-buffer segmentation: no trip_col, no direction_col. This path has
  # nothing to do with the supplied-identity features and still broke.
  gps <- make_rt_trajectory()
  gps$trip_id <- NULL
  gps$direction <- "inbound"

  expect_no_error(result <- run_pipeline(gps))
  expect_true(nrow(result$stop_times) > 0L)
})

test_that("collisions are dropped, not silently mangled", {
  gps <- make_rt_trajectory()
  gps$trip_id <- NULL
  gps$direction <- "inbound"

  msgs <- testthat::capture_messages(result <- run_pipeline(gps))

  expect_true(any(grepl("reserved by stop extraction", msgs, fixed = TRUE)))
  expect_true(any(grepl("direction", msgs, fixed = TRUE)))
  # The output carries the pipeline's own integer direction, never the
  # caller's string.
  expect_type(result$stop_times$direction, "integer")
  expect_false("direction.x" %in% names(result$stop_times))
  expect_false("direction.y" %in% names(result$stop_times))
})

test_that("a collision changes nothing but the dropped column", {
  clean <- make_rt_trajectory()
  clean$trip_id <- NULL

  colliding <- clean
  colliding$direction <- "inbound"
  colliding$bus_stop <- "ZZZ"
  colliding$grouped_ends <- 99L

  baseline <- run_pipeline(clean)
  collided <- suppressMessages(run_pipeline(colliding))

  expect_equal(collided$trips, baseline$trips, ignore_attr = TRUE)
  expect_equal(collided$stop_times, baseline$stop_times, ignore_attr = TRUE)
})

test_that("a passthrough 'stop_id' does not shadow the matched stop", {
  # The pure_r backend joins stops with sf::st_join(), which would produce
  # stop_id.x/stop_id.y and then fail on a missing stop_id.
  skip_if_not_installed("sf")
  clean <- make_rt_trajectory()
  clean$trip_id <- NULL

  colliding <- clean
  colliding$stop_id <- "not-a-real-stop"

  baseline <- run_pipeline(clean, backend = "pure_r")
  collided <- suppressMessages(run_pipeline(colliding, backend = "pure_r"))

  expect_equal(collided$stop_times, baseline$stop_times, ignore_attr = TRUE)
  expect_false("not-a-real-stop" %in% collided$stop_times$stop_id)
})

test_that("the supplied-identity path tolerates the same collision", {
  # Reported from a real feed: trip_col + direction_col with a source
  # column literally named `direction`.
  gps <- collide_trajectory("direction")
  inputs <- make_route_inputs()
  stops <- inputs$stops
  stops$direction <- ifelse(stops$direction == "A", "0", "1")

  expect_no_error(
    result <- suppressMessages(g2g_extract_trips_and_stop_times(
      gps_data = gps,
      terminals_data = inputs$terminals,
      stops_data = stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 100,
      stops_extended_buffer_radius = 150,
      trip_col = "trip_id",
      direction_col = "direction"
    ))
  )
  expect_identical(nrow(result$trips), 2L)
  expect_true(nrow(result$stop_times) > 0L)
})

test_that("g2g_extract_trips() is unaffected by a 'direction' passthrough", {
  # The trips-only entry point never builds a trajectory, so this column was
  # always harmless there and must stay harmless.
  gps <- make_rt_trajectory()
  gps$direction <- "inbound"
  inputs <- make_route_inputs()

  expect_no_error(
    trips <- suppressMessages(g2g_extract_trips(
      gps_data = gps,
      terminals_data = inputs$terminals,
      terminals_buffer_radius = 100
    ))
  )
  expect_true(nrow(trips) > 0L)
})
