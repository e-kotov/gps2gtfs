# Debounced direction-change trip cutting (B3).
#
# A raw direction field flips spuriously: a vehicle reports the opposite
# direction for a handful of pings and then reverts. Cutting a trip on every
# such flip shatters real trips into fragments. The debounce accepts a
# direction change only when the contrary run is BOTH long enough in pings and
# long enough in seconds.
#
# The conjunction is the whole point, and it is what the mixed cases below
# pin down: an `OR` rule would accept a burst that is dense but instantaneous
# (many pings, no elapsed time) and one that is sparse but long (two pings
# either side of a GPS dropout), absorbing neither. Tests 3, 4, 5 and 6a fail
# against `OR` semantics; they are the reason they exist.

# One vehicle on one route, direction supplied per ping. Offsets are given
# explicitly in seconds so each test controls run spans exactly.
make_dir_series <- function(dir, offsets) {
  stopifnot(length(dir) == length(offsets))
  n <- length(dir)
  base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")
  data.table::data.table(
    vehicle_id = "V1",
    timestamp = base + offsets,
    latitude = seq(7.290, 7.320, length.out = n),
    longitude = seq(80.630, 80.660, length.out = n),
    route_id = "R1",
    # Present only to keep the cleaning step from warning about its absence;
    # the fast path segments on identity, not on speed.
    speed = 20,
    dir_flag = as.character(dir)
  )
}

# Number of trips the extractor emits with the debounce switched on.
debounced_trips <- function(gps, min_pings, min_seconds) {
  res <- extract_trips_from_ids_r(
    gps,
    trip_terminals_df = NULL,
    trip_col = "route_id",
    direction_col = "dir_flag",
    cut_on_direction_change = TRUE,
    direction_debounce_min_pings = min_pings,
    direction_debounce_min_seconds = min_seconds
  )
  data.table::uniqueN(res$trip_id)
}

# A steady 60 s run of "0" long enough to qualify at any threshold used here.
steady <- function(n = 10L) rep("0", n)


# --- C2 (1): short in pings AND short in seconds -> absorbed ----------------

test_that("a burst short in both pings and seconds is absorbed", {
  # 2 pings spanning 60 s, against min 3 pings / 300 s.
  dir <- c(steady(), rep("1", 2), steady())
  gps <- make_dir_series(dir, seq(0, by = 60, length.out = length(dir)))

  expect_identical(debounced_trips(gps, 3L, 300), 1L)
})


# --- C2 (2): long in pings AND long in seconds -> a real direction change ---

test_that("a run long in both pings and seconds cuts the trip", {
  # 6 pings spanning 300 s, against min 3 pings / 300 s: qualifies, and is
  # sandwiched between two qualifying runs, so it yields three trips.
  dir <- c(steady(), rep("1", 6), steady())
  gps <- make_dir_series(dir, seq(0, by = 60, length.out = length(dir)))

  expect_identical(debounced_trips(gps, 3L, 300), 3L)
})

test_that("a qualifying run at the end of the series yields two trips", {
  dir <- c(steady(), rep("1", 6))
  gps <- make_dir_series(dir, seq(0, by = 60, length.out = length(dir)))

  expect_identical(debounced_trips(gps, 3L, 300), 2L)
})


# --- C2 (3): long pings, short seconds -> absorbed (R1) ---------------------

test_that("a dense but instantaneous burst is absorbed", {
  # 5 contrary pings at 5 s cadence span 20 s: the ping count clears min 3 but
  # the span does not clear min 300 s. An `OR` rule would cut here.
  dir <- c(steady(), rep("1", 5), steady())
  offsets <- c(
    seq(0, by = 60, length.out = 10L),
    600 + seq(0, by = 5, length.out = 5L),
    700 + seq(0, by = 60, length.out = 10L)
  )
  gps <- make_dir_series(dir, offsets)

  expect_identical(debounced_trips(gps, 3L, 300), 1L)
})


# --- C2 (4): short pings, long seconds -> absorbed (R1) ---------------------

test_that("a sparse burst across a GPS dropout is absorbed", {
  # 2 contrary pings 350 s apart: the span clears min 300 s but the ping count
  # does not clear min 3. An `OR` rule would cut here too.
  dir <- c(steady(), rep("1", 2), steady())
  offsets <- c(
    seq(0, by = 60, length.out = 10L),
    c(600, 950),
    1010 + seq(0, by = 60, length.out = 10L)
  )
  gps <- make_dir_series(dir, offsets)

  expect_identical(debounced_trips(gps, 3L, 300), 1L)
})


# --- C2 (5): a single-ping run spans zero seconds -> absorbed (R2) ----------

test_that("a single contrary ping is absorbed even at min_pings = 1", {
  # min_pings = 1 admits the run on count alone; its span is 0 s, so the
  # conjunction still rejects it. This is the assertion an `OR` rule fails
  # most directly.
  dir <- c(steady(), "1", steady())
  gps <- make_dir_series(dir, seq(0, by = 60, length.out = length(dir)))

  expect_identical(debounced_trips(gps, 1L, 300), 1L)
})


# --- C2 (6): the thresholds are inclusive, and both must be met -------------

test_that("a run one second short of min_seconds is absorbed", {
  # Exactly min_pings (3), span 299 s against min 300 s.
  dir <- c(steady(), rep("1", 3), steady())
  offsets <- c(
    seq(0, by = 60, length.out = 10L),
    c(600, 749, 899),
    959 + seq(0, by = 60, length.out = 10L)
  )
  gps <- make_dir_series(dir, offsets)

  expect_identical(debounced_trips(gps, 3L, 300), 1L)
})

test_that("a run exactly at both thresholds qualifies", {
  # Exactly min_pings (3), span exactly 300 s. Terminal run, so two trips.
  dir <- c(steady(), rep("1", 3))
  offsets <- c(
    seq(0, by = 60, length.out = 10L),
    c(600, 750, 900)
  )
  gps <- make_dir_series(dir, offsets)

  expect_identical(debounced_trips(gps, 3L, 300), 2L)
})

test_that("a run one ping short of min_pings is absorbed at exactly min_seconds", {
  # 2 pings against min 3, span exactly 300 s.
  dir <- c(steady(), rep("1", 2), steady())
  offsets <- c(
    seq(0, by = 60, length.out = 10L),
    c(600, 900),
    960 + seq(0, by = 60, length.out = 10L)
  )
  gps <- make_dir_series(dir, offsets)

  expect_identical(debounced_trips(gps, 3L, 300), 1L)
})


# --- C2 (7): when nothing qualifies, fall back to the modal direction -------

test_that("a series where no run qualifies collapses to the modal direction", {
  # Alternating pairs at 60 s cadence: every run is 2 pings / 60 s, so none
  # clears min 3 pings / 300 s. Without the fallback this vehicle would
  # produce a fragment per run; with it, one trip.
  dir <- rep(c("0", "0", "1", "1"), times = 4L)
  dir <- c(dir, "0", "0")
  gps <- make_dir_series(dir, seq(0, by = 60, length.out = length(dir)))

  expect_no_error(
    res <- extract_trips_from_ids_r(
      gps,
      trip_terminals_df = NULL,
      trip_col = "route_id",
      direction_col = "dir_flag",
      cut_on_direction_change = TRUE,
      direction_debounce_min_pings = 3L,
      direction_debounce_min_seconds = 300
    )
  )
  expect_identical(data.table::uniqueN(res$trip_id), 1L)
  # "0" is modal (10 pings vs 8), so the whole series takes it.
  expect_identical(unique(res$rt_direction), "0")
})


# --- The debounce rule in isolation ----------------------------------------

test_that("debounce_direction_runlen back-fills the prefix from the first qualifying run", {
  # The leading "1" run is too short to qualify, so it inherits the direction
  # of the first run that does - not its own value, and not a placeholder.
  dir <- c(rep("1", 2), rep("0", 10))
  ts <- seq(0, by = 60, length.out = length(dir))

  out <- debounce_direction_runlen(dir, ts, min_pings = 3L, min_seconds = 300)

  expect_identical(out, rep("0", 12))
})

test_that("debounce_direction_runlen absorbs forward from the preceding qualifying run", {
  dir <- c(rep("0", 10), rep("1", 6), rep("0", 2))
  ts <- seq(0, by = 60, length.out = length(dir))

  out <- debounce_direction_runlen(dir, ts, min_pings = 3L, min_seconds = 300)

  # The trailing 2-ping "0" run does not qualify, so it is absorbed into the
  # qualifying "1" run before it rather than reverting to "0".
  expect_identical(out, c(rep("0", 10), rep("1", 8)))
})

test_that("debounce_direction_runlen leaves a series it cannot flip alone", {
  expect_identical(
    debounce_direction_runlen(c("0"), 0, min_pings = 3L, min_seconds = 300),
    "0"
  )
  expect_identical(
    debounce_direction_runlen(rep("0", 5), seq(0, by = 60, length.out = 5),
      min_pings = 3L, min_seconds = 300
    ),
    rep("0", 5)
  )
})


# --- C1: the switch is opt-in and default-off ------------------------------

test_that("the default leaves the mid-trip direction conflict an error", {
  dir <- c(steady(), rep("1", 6), steady())
  gps <- make_dir_series(dir, seq(0, by = 60, length.out = length(dir)))

  expect_error(
    extract_trips_from_ids_r(
      gps,
      trip_terminals_df = NULL,
      trip_col = "route_id",
      direction_col = "dir_flag"
    ),
    "takes more than one value"
  )
})

test_that("cutting on direction change requires a direction column", {
  gps <- make_dir_series(steady(), seq(0, by = 60, length.out = 10L))

  expect_error(
    extract_trips_from_ids_r(
      gps,
      trip_terminals_df = NULL,
      trip_col = "route_id",
      cut_on_direction_change = TRUE
    ),
    "requires 'direction_col'"
  )
})

test_that("the debounce thresholds are validated only when the switch is on", {
  gps <- make_dir_series(steady(), seq(0, by = 60, length.out = 10L))

  expect_error(
    extract_trips_from_ids_r(
      gps,
      trip_terminals_df = NULL,
      trip_col = "route_id",
      direction_col = "dir_flag",
      cut_on_direction_change = TRUE,
      direction_debounce_min_pings = 0L
    ),
    "whole number of pings"
  )
  expect_error(
    extract_trips_from_ids_r(
      gps,
      trip_terminals_df = NULL,
      trip_col = "route_id",
      direction_col = "dir_flag",
      cut_on_direction_change = TRUE,
      direction_debounce_min_seconds = -1
    ),
    "non-negative finite"
  )
  # Off by default, so a nonsense threshold is inert rather than fatal.
  expect_no_error(
    extract_trips_from_ids_r(
      gps,
      trip_terminals_df = NULL,
      trip_col = "route_id",
      direction_col = "dir_flag",
      direction_debounce_min_pings = 0L
    )
  )
})

test_that("g2g_extract_trips threads the switch through to the extractor", {
  dir <- c(steady(), rep("1", 6), steady())
  gps <- as.data.frame(make_dir_series(
    dir,
    seq(0, by = 60, length.out = length(dir))
  ))

  trips <- suppressMessages(g2g_extract_trips(
    gps_data = gps,
    trip_col = "route_id",
    direction_col = "dir_flag",
    cut_on_direction_change = TRUE,
    direction_debounce_min_pings = 3L,
    direction_debounce_min_seconds = 300,
    diagnostics_warn = FALSE
  ))

  expect_identical(nrow(trips), 3L)
})

test_that("g2g_extract_trips rejects the switch without a direction column", {
  gps <- as.data.frame(make_dir_series(
    steady(),
    seq(0, by = 60, length.out = 10L)
  ))

  expect_error(
    g2g_extract_trips(
      gps_data = gps,
      trip_col = "route_id",
      cut_on_direction_change = TRUE,
      diagnostics_warn = FALSE
    ),
    "requires 'direction_col'"
  )
})
