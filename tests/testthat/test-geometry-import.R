# Distances asserted here are hand-worked in the comment above
# make_geometry_route() in helper-fixtures.R.

test_that("g2g_terminals_from_geometries takes the first vertex of each line", {
  fx <- make_geometry_route()
  terminals <- g2g_terminals_from_geometries(fx$geometries, route_id = "r1")

  expect_s3_class(terminals, "data.table")
  expect_identical(
    names(terminals),
    c("terminal_id", "latitude", "longitude", "direction")
  )
  expect_identical(nrow(terminals), 2L)
  # terminal_id IS the direction label: that is what removes the need for a
  # stop_direction_map downstream.
  expect_identical(terminals$terminal_id, terminals$direction)
  expect_setequal(terminals$terminal_id, c("A", "B"))

  # A starts at the west end of the corridor, which is the fixture origin.
  expect_equal(terminals[terminal_id == "A", latitude], 7.29)
  expect_equal(terminals[terminal_id == "A", longitude], 80.63)

  # B starts at the east end, 20 m north: sqrt(1160^2 + 20^2) = 1160.17 m away.
  expect_equal(
    haversine_m_r(
      terminals$latitude[1L],
      terminals$longitude[1L],
      terminals$latitude[2L],
      terminals$longitude[2L]
    ),
    1160.172,
    tolerance = 1e-4
  )

  # Each terminal is the FIRST vertex of its own line, not the westmost point.
  b <- fx$geometries[fx$geometries$direction == "B", ]
  b <- b[order(b$vertex_seq), ]
  expect_equal(terminals[terminal_id == "B", latitude], b$latitude[1L])
  expect_equal(terminals[terminal_id == "B", longitude], b$longitude[1L])
})

test_that("one- and three-direction routes error and point at layover", {
  one <- make_geometry_route("one")$geometries
  expect_error(
    g2g_terminals_from_geometries(one, route_id = "r1"),
    "1 direction group"
  )
  expect_error(
    g2g_terminals_from_geometries(one, route_id = "r1"),
    'segmentation = "layover"',
    fixed = TRUE
  )

  three <- make_geometry_route("three")$geometries
  err <- tryCatch(
    g2g_terminals_from_geometries(three, route_id = "r1"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "3 direction group")
  # The error names the directions found, so the caller can see the stray one.
  expect_match(err, "A, B, C")
  expect_match(err, 'segmentation = "layover"', fixed = TRUE)

  # The stops builder refuses the same input, so the pair cannot disagree.
  expect_error(
    g2g_stops_from_geometries(
      three,
      make_geometry_route()$stops,
      route_id = "r1"
    ),
    "3 direction group"
  )
})

test_that("two lines starting 40 m apart warn about platform separation", {
  same_start <- make_geometry_route("same_start")$geometries
  expect_warning(
    terminals <- g2g_terminals_from_geometries(same_start, route_id = "r1"),
    "only 40 m apart"
  )
  expect_warning(
    g2g_terminals_from_geometries(same_start, route_id = "r1"),
    "two platforms of the same place"
  )
  expect_identical(nrow(terminals), 2L)
})

test_that("distance to the polyline is measured to segments, not vertices", {
  fx <- make_geometry_route()
  g <- fx$geometries
  a <- g[g$direction == "A", ]
  b <- g[g$direction == "B", ]
  s <- fx$stops
  smid <- s[s$stop_id == "SMID", ]

  # 5 m from A's line, 15 m from B's line ...
  expect_equal(
    point_polyline_distance_m(
      smid$latitude,
      smid$longitude,
      a$latitude,
      a$longitude
    ),
    5,
    tolerance = 1e-3
  )
  expect_equal(
    point_polyline_distance_m(
      smid$latitude,
      smid$longitude,
      b$latitude,
      b$longitude
    ),
    15,
    tolerance = 1e-3
  )
  # ... but 180.07 m from A's nearest vertex, which is why a vertex rule
  # would drop it at any sane buffer.
  expect_equal(
    min(haversine_m_r(smid$latitude, smid$longitude, a$latitude, a$longitude)),
    180.069,
    tolerance = 1e-4
  )

  sa1 <- s[s$stop_id == "SA1", ]
  expect_equal(
    point_polyline_distance_m(
      sa1$latitude,
      sa1$longitude,
      a$latitude,
      a$longitude
    ),
    8,
    tolerance = 1e-3
  )
  expect_equal(
    point_polyline_distance_m(
      sa1$latitude,
      sa1$longitude,
      b$latitude,
      b$longitude
    ),
    28,
    tolerance = 1e-3
  )
})

test_that("g2g_stops_from_geometries assigns stops per direction", {
  fx <- make_geometry_route()

  # buffer_m = 10: each corridor stop is 8 m from its own line and 28 m from
  # the other, so it matches only its own direction. SMID is 5 m from A and
  # 15 m from B, so it is an A stop.
  warns <- testthat::capture_warnings(
    msgs <- testthat::capture_messages(
      tight <- g2g_stops_from_geometries(
        fx$geometries,
        fx$stops,
        route_id = "r1",
        buffer_m = 10
      )
    )
  )

  expect_identical(
    names(tight),
    c("stop_id", "latitude", "longitude", "direction")
  )
  expect_identical(
    tight[direction == "A", stop_id],
    c("SA1", "SA2", "SMID")
  )
  expect_identical(tight[direction == "B", stop_id], c("SB1", "SB2"))
  # Sorted by direction then stop_id.
  expect_identical(tight$direction, c("A", "A", "A", "B", "B"))
  # SMID is included only because distance is measured to the segment: it is
  # 180.07 m from the nearest vertex, so a vertex-only rule finds nothing
  # here and this expectation fails.
  expect_true("SMID" %in% tight$stop_id)
  # 500 m off the corridor, in neither direction.
  expect_false("SFAR" %in% tight$stop_id)

  # The NA-coordinate stop and the blank-id stop are dropped, counted once.
  expect_length(warns, 1L)
  expect_match(warns, "2 of 8 stop\\(s\\) dropped")
  expect_match(warns, "1 with a missing coordinate")
  expect_match(warns, "1 with a missing or blank stop_id")
  expect_false(any(is.na(tight$stop_id)))
  expect_true(all(nzchar(tight$stop_id)))
  # SFAR is the only survivor matched by no direction.
  expect_match(paste(msgs, collapse = " "), "1 of 6 stop\\(s\\) are farther")

  # buffer_m = 50: the 20 m cross-street offset puts every corridor stop
  # within reach of both lines, which is exactly why the labels are needed.
  wide <- suppressWarnings(suppressMessages(g2g_stops_from_geometries(
    fx$geometries,
    fx$stops,
    route_id = "r1",
    buffer_m = 50
  )))
  expect_identical(
    wide[direction == "A", stop_id],
    c("SA1", "SA2", "SB1", "SB2", "SMID")
  )
  expect_identical(
    wide[direction == "B", stop_id],
    c("SA1", "SA2", "SB1", "SB2", "SMID")
  )
  expect_false("SFAR" %in% wide$stop_id)

  # Columns other than stop_id/latitude/longitude are dropped.
  expect_false("stop_name" %in% names(wide))

  expect_error(
    g2g_stops_from_geometries(
      fx$geometries,
      fx$stops,
      route_id = "r1",
      buffer_m = 0
    ),
    "buffer_m must be one positive finite number"
  )
})

test_that("a route no stop reaches returns an empty table, not an error", {
  fx <- make_geometry_route()
  # SFAR alone: 500 m from A's line and 480 m from B's, so nothing matches at
  # 10 m. A caller looping over dozens of routes has to be able to take that
  # answer for one of them.
  only_far <- fx$stops[fx$stops$stop_id == "SFAR", ]

  expect_warning(
    empty <- g2g_stops_from_geometries(
      fx$geometries,
      only_far,
      route_id = "r1",
      buffer_m = 10
    ),
    "No stop is within 10 m of any direction of route_id 'r1'"
  )
  expect_warning(
    g2g_stops_from_geometries(fx$geometries, only_far, "r1", buffer_m = 10),
    "same coordinate reference system"
  )

  expect_s3_class(empty, "data.table")
  expect_identical(nrow(empty), 0L)
  expect_identical(
    names(empty),
    c("stop_id", "latitude", "longitude", "direction")
  )
  expect_type(empty$stop_id, "character")
  expect_type(empty$latitude, "double")
  expect_type(empty$longitude, "double")
  expect_type(empty$direction, "character")
  expect_identical(data.table::key(empty), c("direction", "stop_id"))

  # The unmatched-count message would be redundant with the warning.
  expect_identical(
    testthat::capture_messages(suppressWarnings(
      g2g_stops_from_geometries(fx$geometries, only_far, "r1", buffer_m = 10)
    )),
    character()
  )
})

test_that("vertex order comes from vertex_seq, which is required", {
  fx <- make_geometry_route()
  ordered <- suppressWarnings(suppressMessages(
    g2g_stops_from_geometries(fx$geometries, fx$stops, "r1", buffer_m = 10)
  ))
  ordered_terminals <- g2g_terminals_from_geometries(fx$geometries, "r1")

  set.seed(42)
  shuffled_geom <- fx$geometries[sample(nrow(fx$geometries)), ]
  shuffled <- suppressWarnings(suppressMessages(
    g2g_stops_from_geometries(shuffled_geom, fx$stops, "r1", buffer_m = 10)
  ))
  expect_equal(shuffled, ordered)
  expect_equal(
    g2g_terminals_from_geometries(shuffled_geom, "r1"),
    ordered_terminals
  )

  no_seq <- fx$geometries
  no_seq$vertex_seq <- NULL
  expect_error(
    g2g_terminals_from_geometries(no_seq, "r1"),
    "Missing required columns in geometries: vertex_seq"
  )

  chr_seq <- fx$geometries
  chr_seq$vertex_seq <- as.character(chr_seq$vertex_seq)
  expect_error(
    g2g_terminals_from_geometries(chr_seq, "r1"),
    "vertex_seq' must be numeric"
  )

  expect_error(
    g2g_terminals_from_geometries(fx$geometries, "nope"),
    "not found in 'geometries'"
  )
})

test_that("sf LINESTRING and MULTILINESTRING inputs match the long form", {
  skip_if_not_installed("sf")
  fx <- make_geometry_route()
  g <- fx$geometries

  coords <- function(direction, rows = NULL) {
    v <- g[g$direction == direction, ]
    v <- v[order(v$vertex_seq), c("longitude", "latitude")]
    if (!is.null(rows)) {
      v <- v[rows, ]
    }
    as.matrix(v)
  }

  linestrings <- sf::st_sf(
    route_id = c("r1", "r1"),
    direction = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_linestring(coords("A")),
      sf::st_linestring(coords("B")),
      crs = 4326
    )
  )
  # Same corridor, but each direction split into two parts. The parts are
  # concatenated in part order, so the joining segment (vertices 3 -> 4, the
  # 360 m one carrying SMID) is restored.
  multilinestrings <- sf::st_sf(
    route_id = c("r1", "r1"),
    direction = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_multilinestring(list(coords("A", 1:3), coords("A", 4:6))),
      sf::st_multilinestring(list(coords("B", 1:3), coords("B", 4:6))),
      crs = 4326
    )
  )

  long_terminals <- g2g_terminals_from_geometries(g, "r1")
  long_stops <- suppressWarnings(suppressMessages(
    g2g_stops_from_geometries(g, fx$stops, "r1", buffer_m = 10)
  ))

  for (obj in list(linestrings, multilinestrings)) {
    expect_equal(g2g_terminals_from_geometries(obj, "r1"), long_terminals)
    expect_equal(
      suppressWarnings(suppressMessages(
        g2g_stops_from_geometries(obj, fx$stops, "r1", buffer_m = 10)
      )),
      long_stops
    )
  }

  # A bare sfc has no route_id/direction to work with.
  expect_error(
    g2g_terminals_from_geometries(sf::st_geometry(linestrings), "r1"),
    "bare 'sfc' geometry column"
  )
})

test_that("geometry-derived tables drive the pipeline without a direction map", {
  fx <- make_geometry_route()
  terminals <- g2g_terminals_from_geometries(fx$geometries, "r1")
  stops <- suppressWarnings(suppressMessages(
    g2g_stops_from_geometries(fx$geometries, fx$stops, "r1", buffer_m = 10)
  ))

  # One vehicle running A west -> east, then B east -> west, sampled every
  # 40 m along each line.
  leg_a <- fx$path("A")
  leg_b <- fx$path("B")
  base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
  gps <- data.frame(
    vehicle_id = "geo-1",
    latitude = c(leg_a$latitude, leg_b$latitude),
    longitude = c(leg_a$longitude, leg_b$longitude),
    timestamp = c(
      base + 20 * seq_len(nrow(leg_a)),
      base + 3600 + 20 * seq_len(nrow(leg_b))
    ),
    speed = 0,
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    g2g_extract_trips_and_stop_times(
      gps_data = gps,
      terminals_data = terminals,
      # No stop_direction_map: the labels already are terminal ids.
      stops_data = stops,
      terminals_buffer_radius = 100,
      stops_buffer_radius = 30,
      stops_extended_buffer_radius = 60
    )
  ))

  expect_identical(nrow(result$trips), 2L)
  expect_identical(result$trips$start_terminal, c("A", "B"))
  expect_identical(result$trips$end_terminal, c("B", "A"))
  expect_identical(result$trips$direction, c(1L, 2L))

  st <- result$stop_times
  expect_true(nrow(st) > 0L)
  # Direction 1 is the trip that started at terminal A, so it may only visit
  # the stops labelled "A" - including SMID, which only the segment rule put
  # there.
  expect_setequal(st[direction == 1L, stop_id], c("SA1", "SA2", "SMID"))
  expect_setequal(st[direction == 2L, stop_id], c("SB1", "SB2"))
})
