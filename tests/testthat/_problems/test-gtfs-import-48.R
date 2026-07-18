# Extracted from test-gtfs-import.R:48

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gps2gtfs", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
expect_error(
    g2g_terminals_from_gtfs(make_gtfs(), route_id = "nope"),
    "not found in trips.txt"
  )
