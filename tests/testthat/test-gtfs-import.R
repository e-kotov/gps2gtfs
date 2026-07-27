make_gtfs <- function() {
  list(
    routes = data.frame(route_id = "r1", route_type = 3),
    trips = data.frame(
      trip_id = c("t1", "t2", "t3", "t4"),
      route_id = "r1",
      service_id = "s1"
    ),
    stop_times = data.frame(
      trip_id = rep(c("t1", "t2", "t3", "t4"), each = 4),
      stop_id = c(
        "A", "S1", "S2", "B", # t1: A -> B
        "B", "S2", "S1", "A", # t2: B -> A
        "A", "S1", "S2", "B", # t3: A -> B
        "B", "S2", "S1", "A" # t4: B -> A
      ),
      stop_sequence = rep(1:4, 4)
    ),
    stops = data.frame(
      stop_id = c("A", "S1", "S2", "B"),
      stop_name = c("Term A", "Stop 1", "Stop 2", "Term B"),
      stop_lat = c(7.290, 7.300, 7.310, 7.320),
      stop_lon = c(80.630, 80.640, 80.650, 80.660)
    )
  )
}

test_that("g2g_terminals_from_gtfs derives the two route endpoints", {
  terminals <- g2g_terminals_from_gtfs(make_gtfs(), route_id = "r1")

  expect_s3_class(terminals, "data.table")
  expect_identical(nrow(terminals), 2L)
  expect_setequal(terminals$terminal_id, c("A", "B"))
  expect_identical(
    names(terminals),
    c("terminal_id", "latitude", "longitude")
  )
  expect_equal(
    terminals[terminal_id == "A", latitude],
    7.290
  )
})

test_that("g2g_terminals_from_gtfs errors usefully", {
  expect_error(
    g2g_terminals_from_gtfs(make_gtfs(), route_id = "nope"),
    "not found in trips.txt"
  )

  loop_gtfs <- make_gtfs()
  loop_gtfs$stop_times$stop_id <- c(
    "A", "S1", "S2", "A",
    "A", "S2", "S1", "A",
    "A", "S1", "S2", "A",
    "A", "S2", "S1", "A"
  )
  expect_error(
    g2g_terminals_from_gtfs(loop_gtfs, route_id = "r1"),
    "fewer than two distinct trip endpoints"
  )

  no_trips <- make_gtfs()
  no_trips$trips <- NULL
  expect_error(
    g2g_terminals_from_gtfs(no_trips, route_id = "r1"),
    "missing required file 'trips.txt'"
  )

  expect_error(
    g2g_terminals_from_gtfs(42, route_id = "r1"),
    "must be a GTFS feed object"
  )
})

test_that("g2g_stops_from_gtfs labels stops with starting terminal ids", {
  stops <- g2g_stops_from_gtfs(make_gtfs(), route_id = "r1")

  expect_identical(
    names(stops),
    c("stop_id", "latitude", "longitude", "direction")
  )
  # S1 and S2 are served in both directions -> one row per direction
  expect_identical(nrow(stops), 4L)
  expect_setequal(unique(stops$direction), c("A", "B"))
  expect_false(any(stops$stop_id %in% c("A", "B")))
})

test_that("derived tables run through the pipeline with the identity map", {
  gtfs <- make_gtfs()
  terminals <- g2g_terminals_from_gtfs(gtfs, route_id = "r1")
  stops <- g2g_stops_from_gtfs(gtfs, route_id = "r1")

  # Synthetic trajectory A -> B passing near S1 and S2
  path <- gtfs$stops
  gps <- data.frame(
    vehicle_id = "bus-1",
    latitude = c(
      path$stop_lat[1],
      path$stop_lat[2],
      path$stop_lat[3],
      path$stop_lat[4]
    ),
    longitude = c(
      path$stop_lon[1],
      path$stop_lon[2],
      path$stop_lon[3],
      path$stop_lon[4]
    ),
    timestamp = as.POSIXct("2026-06-06 08:00:00", tz = "UTC") +
      c(0, 300, 600, 900),
    speed = c(0, 0, 0, 0)
  )
  # Return trip B -> A
  gps_back <- gps
  gps_back$latitude <- rev(gps$latitude)
  gps_back$longitude <- rev(gps$longitude)
  gps_back$timestamp <- gps$timestamp + 1800
  gps_all <- rbind(gps, gps_back)

  result <- suppressWarnings(g2g_extract_trips_and_stop_times(
    gps_data = gps_all,
    terminals_data = terminals,
    stops_data = stops,
    terminals_buffer_radius = 100,
    stops_buffer_radius = 100,
    stops_extended_buffer_radius = 150
  ))

  expect_identical(nrow(result$trips), 2L)
  expect_setequal(result$trips$start_terminal, c("A", "B"))
  expect_true(nrow(result$stop_times) > 0L)
  expect_true(all(result$stop_times$stop_id %in% c("S1", "S2")))
})

test_that("numeric stop ids survive without scientific notation", {
  # as.character(1e5) is "1e+05", which silently breaks every downstream id
  # join and looks exactly like "the helper shares no stop ids with my data".
  gtfs <- list(
    routes = data.frame(route_id = 1, route_type = 3),
    trips = data.frame(trip_id = c("t1", "t2"), route_id = 1, service_id = "s1"),
    stop_times = data.frame(
      trip_id = rep(c("t1", "t2"), each = 3),
      stop_id = c(100000, 200000, 300000, 300000, 200000, 100000),
      stop_sequence = rep(1:3, 2)
    ),
    stops = data.frame(
      stop_id = c(100000, 200000, 300000),
      stop_lat = c(7.29, 7.30, 7.31),
      stop_lon = c(80.63, 80.64, 80.65)
    )
  )

  terminals <- g2g_terminals_from_gtfs(gtfs, route_id = 1)
  expect_setequal(terminals$terminal_id, c("100000", "300000"))
  expect_false(any(grepl("e+", terminals$terminal_id, fixed = TRUE)))

  stops <- g2g_stops_from_gtfs(gtfs, route_id = 1)
  expect_identical(unique(stops$stop_id), "200000")
})

test_that("g2g_stops_from_gtfs warns when trips are dropped", {
  gtfs <- make_gtfs()
  # A short turn that starts mid-route: it belongs to no direction group and
  # its stops silently vanish from the result.
  gtfs$trips <- rbind(
    gtfs$trips,
    data.frame(trip_id = "t5", route_id = "r1", service_id = "s1")
  )
  gtfs$stop_times <- rbind(
    gtfs$stop_times,
    data.frame(
      trip_id = "t5",
      stop_id = c("S1", "S2"),
      stop_sequence = 1:2
    )
  )

  # The added short turn also drops endpoint coverage below 90%, so more than
  # one warning fires; assert over all of them rather than just the first.
  warns <- testthat::capture_warnings(g2g_stops_from_gtfs(gtfs, route_id = "r1"))
  expect_true(any(grepl("do not start at either derived terminal", warns)))
})

test_that("two platforms of one place are flagged, not returned silently", {
  gtfs <- list(
    routes = data.frame(route_id = "r1", route_type = 3),
    trips = data.frame(trip_id = c("t1", "t2"), route_id = "r1", service_id = "s1"),
    stop_times = data.frame(
      trip_id = rep(c("t1", "t2"), each = 3),
      # A1 and A2 are two platforms ~25 m apart at the same terminal
      stop_id = c("A1", "S1", "A2", "A2", "S1", "A1"),
      stop_sequence = rep(1:3, 2)
    ),
    stops = data.frame(
      stop_id = c("A1", "A2", "S1"),
      stop_lat = c(7.2900, 7.29020, 7.3000),
      stop_lon = c(80.6300, 80.63010, 80.6400)
    )
  )

  warns <- testthat::capture_warnings(g2g_terminals_from_gtfs(gtfs, route_id = "r1"))
  expect_true(any(grepl("platforms of the same place", warns)))
})

test_that("the loop-route error points at layover segmentation", {
  gtfs <- list(
    routes = data.frame(route_id = "r1", route_type = 3),
    trips = data.frame(trip_id = c("t1", "t2"), route_id = "r1", service_id = "s1"),
    stop_times = data.frame(
      trip_id = rep(c("t1", "t2"), each = 3),
      stop_id = c("A", "S1", "A", "A", "S2", "A"),
      stop_sequence = rep(1:3, 2)
    ),
    stops = data.frame(
      stop_id = c("A", "S1", "S2"),
      stop_lat = c(7.29, 7.30, 7.31),
      stop_lon = c(80.63, 80.64, 80.65)
    )
  )

  expect_error(
    g2g_terminals_from_gtfs(gtfs, route_id = "r1"),
    'segmentation = "layover"',
    fixed = TRUE
  )
})
