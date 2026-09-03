make_rt_trajectory <- function() {
  # Two trips of one vehicle with GTFS-RT trip_id annotations:
  # trip CS_1 runs A -> B, trip CS_2 runs B -> A. One un-annotated ping
  # in between (layover) must not break segmentation.
  lat <- seq(7.290, 7.320, length.out = 4)
  lon <- seq(80.630, 80.660, length.out = 4)
  base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")

  data.frame(
    vehicle_id = "7482",
    latitude = c(lat, 7.321, rev(lat)),
    longitude = c(lon, 80.661, rev(lon)),
    timestamp = base + c(0, 300, 600, 900, 1200, 1800, 2100, 2400, 2700),
    speed = c(0, 20, 20, 0, 0, 0, 20, 20, 0),
    trip_id = c(rep("CS_1", 4), NA, rep("CS_2", 4)),
    stringsAsFactors = FALSE
  )
}

# --- Layover-segmentation fixtures ------------------------------------------
# Same coordinate frame as make_rt_trajectory(): A = (7.290, 80.630),
# B = (7.320, 80.660); stops S1/S2 sit exactly on the 4-point A-B diagonal.

lay_leg <- function(from, to, t0, n = 4L, interval = 60) {
  data.frame(
    latitude = seq(from[1], to[1], length.out = n),
    longitude = seq(from[2], to[2], length.out = n),
    t = t0 + seq(0, by = interval, length.out = n),
    speed = 20
  )
}

lay_dwell <- function(at, t0, secs, interval = 120, jitter_lat = 0) {
  offs <- seq(0, secs, by = interval)
  data.frame(
    latitude = at[1] + jitter_lat * rep_len(c(0, 1, -1), length(offs)),
    longitude = at[2],
    t = t0 + offs,
    speed = 0
  )
}

lay_wrap <- function(
  parts,
  base = as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
) {
  df <- do.call(rbind, parts)
  data.frame(
    vehicle_id = "L1",
    latitude = df$latitude,
    longitude = df$longitude,
    timestamp = base + df$t,
    speed = df$speed,
    stringsAsFactors = FALSE
  )
}

# Raw GPS for routes the two-terminal model cannot segment. Dwells between
# trips exceed 10 min (the layover_gap default); every leg ping moves ~1.1 km
# so no in-trip stationarity.
make_layover_route <- function(
  kind = c("short_turn", "loop", "branch"),
  base = as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
) {
  kind <- match.arg(kind)
  A <- c(7.290, 80.630)
  B <- c(7.320, 80.660)
  M <- c(7.300, 80.640) # mid-route short-turn point (= S1 position)
  C <- c(7.290, 80.660) # third branch terminal
  P1 <- c(7.310, 80.630) # loop ring corners
  P2 <- c(7.310, 80.650)
  P3 <- c(7.290, 80.650)

  parts <- switch(
    kind,
    short_turn = list(
      # trip 1: A -> B, then a 15-min stationary layover at B
      lay_leg(A, B, 0),
      lay_dwell(B, 240, 900),
      # trip 2: B -> M (short-turn, ends mid-route), then 15 min of silence
      lay_leg(B, M, 1260, n = 3),
      # trip 3: M -> A
      lay_leg(M, A, 2280, n = 3)
    ),
    loop = list(
      # trip 1: ring A -> P1 -> P2 -> P3 -> A (start == end)
      lay_leg(A, P1, 0, n = 3),
      lay_leg(P1, P2, 180, n = 3),
      lay_leg(P2, P3, 360, n = 3),
      lay_leg(P3, A, 540, n = 3),
      # 12-min stationary layover at A
      lay_dwell(A, 720, 720),
      # trip 2: second ring
      lay_leg(A, P1, 1560, n = 3),
      lay_leg(P1, P2, 1740, n = 3),
      lay_leg(P2, P3, 1920, n = 3),
      lay_leg(P3, A, 2100, n = 3)
    ),
    branch = list(
      # 3-terminal service: A -> B, B -> A, A -> C, C -> A
      lay_leg(A, B, 0),
      lay_dwell(B, 240, 900),
      lay_leg(B, A, 1260),
      lay_dwell(A, 1500, 900),
      lay_leg(A, C, 2520),
      lay_dwell(C, 2760, 900),
      lay_leg(C, A, 3780)
    )
  )
  lay_wrap(parts, base = base)
}

make_route_inputs <- function() {
  lat <- seq(7.290, 7.320, length.out = 4)
  lon <- seq(80.630, 80.660, length.out = 4)
  list(
    terminals = data.frame(
      terminal_id = c("A", "B"),
      latitude = lat[c(1, 4)],
      longitude = lon[c(1, 4)]
    ),
    stops = data.frame(
      stop_id = c("S1", "S2", "S1r", "S2r"),
      latitude = lat[c(2, 3, 3, 2)],
      longitude = lon[c(2, 3, 3, 2)],
      direction = c("A", "A", "B", "B")
    )
  )
}

# --- Geometry-input fixtures (FR-9) -----------------------------------------
# A straight two-direction corridor near the other fixtures' coordinates, laid
# out in metres and converted to degrees with the same earth radius the
# package's haversine uses (6371000 m), so every distance below is exact to
# well under a millimetre:
#
#   1 m of latitude  = 1 / (6371000 * pi/180)            = 8.99320e-06 deg
#   1 m of longitude = that / cos(7.29 deg)              = 9.06651e-06 deg
#     (cos(7.29 deg) = 0.9919166; 110296.09 m per degree of longitude)
#
# Direction "A" runs west -> east along north = 0 m; direction "B" is the
# reverse of it, 20 m north (the other side of the street). Vertices sit at
# 0, 200, 400, 760, 960, 1160 m east, so the segments are 200, 200, 360, 200,
# 200 m long and the third is the longest. Hand-worked distances:
#
#   terminal A = first vertex of A = (0 m E, 0 m N)
#   terminal B = first vertex of B = (1160 m E, 20 m N)
#   terminal separation  = sqrt(1160^2 + 20^2) = 1160.172 m   (> 150 m floor)
#
#   stop    east   north   to A line   to B line   nearest A vtx   nearest B vtx
#   SA1      200      -8         8.0        28.0         8.0            28.0
#   SA2      960      -8         8.0        28.0         8.0            28.0
#   SB1      400      28        28.0         8.0        28.0             8.0
#   SB2      760      28        28.0         8.0        28.0             8.0
#   SMID     580       5         5.0        15.0       180.069         180.624
#   SFAR     580     500       500.0       480.0       531.4           512.6
#   SNA      200      NA          -           -          -               -
#   ""       200      -8         8.0        28.0         8.0            28.0
#
# SMID is the segment-versus-vertex case: it sits at the midpoint of A's
# 360 m segment, so sqrt(180^2 + 5^2) = 180.069 m from either end vertex but
# 5 m from the line. sqrt(180^2 + 15^2) = 180.624 m from B's nearest vertex,
# 15 m from B's line. At buffer_m = 50 a vertex rule finds it in neither
# direction; the segment rule finds it in both.
#
# The 8 m / 28 m pairs are the cross-street case: at buffer_m = 50 every
# corridor stop matches both directions, at buffer_m = 10 each matches only
# its own side.
#
# Variants: "one" keeps only direction A; "three" adds a C branch 200 m north;
# "same_start" replaces B with a west -> east line 40 m north of A, whose
# first vertex is therefore 40 m from A's (the platform-separation warning).
make_geometry_route <- function(
  variant = c("two", "one", "three", "same_start")
) {
  variant <- match.arg(variant)

  lat0 <- 7.29
  lon0 <- 80.63
  m_lat <- 1 / (6371000 * pi / 180)
  m_lon <- m_lat / cos(lat0 * pi / 180)
  east <- c(0, 200, 400, 760, 960, 1160)

  line <- function(direction, east_m, north_m) {
    data.frame(
      route_id = "r1",
      direction = direction,
      vertex_seq = seq_along(east_m),
      latitude = lat0 + north_m * m_lat,
      longitude = lon0 + east_m * m_lon,
      stringsAsFactors = FALSE
    )
  }

  a <- line("A", east, rep(0, length(east)))
  b <- line("B", rev(east), rep(20, length(east)))

  geometries <- switch(
    variant,
    two = rbind(a, b),
    one = a,
    three = rbind(a, b, line("C", east, rep(200, length(east)))),
    same_start = rbind(a, line("B", east, rep(40, length(east))))
  )

  stops <- data.frame(
    stop_id = c("SA1", "SA2", "SB1", "SB2", "SMID", "SFAR", "SNA", ""),
    stop_name = paste("stop", 1:8),
    east_m = c(200, 960, 400, 760, 580, 580, 200, 200),
    north_m = c(-8, -8, 28, 28, 5, 500, NA, -8),
    stringsAsFactors = FALSE
  )
  stops$latitude <- lat0 + stops$north_m * m_lat
  stops$longitude <- lon0 + stops$east_m * m_lon
  stops$east_m <- NULL
  stops$north_m <- NULL

  list(
    geometries = geometries,
    stops = stops,
    # Interpolated pings along a direction's line, `spacing` metres apart, for
    # the end-to-end pipeline test. Returned as a helper so the test can chain
    # A then B into one vehicle's day.
    path = function(direction, spacing = 40) {
      v <- geometries[geometries$direction == direction, ]
      v <- v[order(v$vertex_seq), ]
      out_lat <- numeric(0)
      out_lon <- numeric(0)
      for (i in seq_len(nrow(v) - 1L)) {
        d_m <- sqrt(
          ((v$latitude[i + 1L] - v$latitude[i]) / m_lat)^2 +
            ((v$longitude[i + 1L] - v$longitude[i]) / m_lon)^2
        )
        k <- max(1L, as.integer(round(d_m / spacing)))
        f <- seq(0, 1, length.out = k + 1L)[-(k + 1L)]
        out_lat <- c(out_lat, v$latitude[i] + f * diff(v$latitude[i:(i + 1L)]))
        out_lon <- c(out_lon, v$longitude[i] + f * diff(v$longitude[i:(i + 1L)]))
      }
      data.frame(
        latitude = c(out_lat, v$latitude[nrow(v)]),
        longitude = c(out_lon, v$longitude[nrow(v)])
      )
    }
  )
}
