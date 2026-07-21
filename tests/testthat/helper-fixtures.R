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
