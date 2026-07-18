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
