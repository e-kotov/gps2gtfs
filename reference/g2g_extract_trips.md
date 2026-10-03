# Extract trips from GPS trajectories

Cuts a stream of vehicle GPS pings into individual trips and returns one
row of features per trip: its vehicle, direction, start and end times,
duration, and day of week. Takes a data.frame or a path to a CSV, and
optionally writes the result to `output_path`.

## Usage

``` r
g2g_extract_trips(
  gps_data,
  terminals_data = NULL,
  terminals_buffer_radius = NULL,
  output_path = NULL,
  projected_crs = NULL,
  backend = "auto",
  projected = NULL,
  vehicle_col = "vehicle_id",
  time_col = "timestamp",
  tz = NULL,
  trip_col = NULL,
  direction_col = NULL,
  cut_on_direction_change = FALSE,
  direction_debounce_min_pings = 2L,
  direction_debounce_min_seconds = 600,
  session_gap = 4 * 3600,
  segmentation = c("auto", "terminals", "layover"),
  layover_gap = 10 * 60,
  layover_radius = 50,
  diagnostics_warn = getOption("gps2gtfs.diagnostics_warn", TRUE)
)
```

## Arguments

- gps_data:

  A data.frame or path to the raw GPS CSV.

- terminals_data:

  A data.frame or path to the terminal coordinates CSV. Optional
  (`NULL`) with `segmentation = "layover"` or with `direction_col`;
  required for terminal-buffer segmentation.

- terminals_buffer_radius:

  Numeric. Buffer radius for terminals (in meters). Only consumed by
  terminal-buffer segmentation; may be omitted otherwise.

- output_path:

  Character. Optional path to write output trip features as CSV. Default
  is `NULL` (no file written).

- projected_crs:

  Numeric. The EPSG code of a projected coordinate system used for
  metric distance calculations. Required when `projected = TRUE`;
  otherwise a suitable UTM projection is selected when omitted.

- backend:

  Character. The backend to use: `"auto"`, `"rust"`, `"rcpp"`, or
  `"pure_r"`. Automatic selection prefers Rust, then Rcpp, then pure R.
  Explicit unavailable backends produce an error.

- projected:

  Logical. Whether plain-table coordinates are already projected.
  Out-of-bounds coordinates require explicit `TRUE`.

- vehicle_col:

  Character. Name of the vehicle identifier column in `gps_data`.
  Default `"vehicle_id"` (GTFS-Realtime convention).

- time_col:

  Character. Name of the timestamp column in `gps_data`. Default
  `"timestamp"` (GTFS-Realtime convention).

- tz:

  Character. Timezone in which non-`POSIXct` timestamps are interpreted;
  also the timezone whose midnight bounds service days and thus every
  downstream GTFS clock string. Pass the feed's operating timezone, or
  supply localized `POSIXct` timestamps. `NULL` (default) parses
  character input as UTC with a warning. Passed to
  [`g2g_clean_gps`](https://e-kotov.github.io/gps2gtfs/reference/g2g_clean_gps.md).

- trip_col:

  Character. Optional name(s) of column(s) holding supplied trip
  identities (e.g. `"trip_id"` from GTFS-Realtime Vehicle Positions).
  When given, trips are segmented by those identities (fast path)
  instead of inferred from terminal-buffer crossings. Without
  `direction_col`, the first/last ping of each segment is matched to the
  nearest terminal for direction assignment. May be several columns that
  jointly identify a trip - e.g.
  `c("route_id", "direction_id", "start_date", "start_time")`, the
  GTFS-Realtime TripDescriptor, for feeds whose positions carry no
  `trip_id`; they are combined into one identity. Default `NULL`
  (spatial inference).

- direction_col:

  Character. Optional column giving each ping's travel direction (e.g.
  GTFS-Realtime `"direction_id"`). Only used with `trip_col`. When
  supplied, trip direction is taken from the data rather than inferred
  from which of two terminals a trip started at, so `terminals_data`
  becomes optional and routes that are not simple two-terminal lines
  (short-turns, variants, one-way services) are handled. For stop-time
  extraction, stops are grouped for matching by their `direction` label,
  which must use the same values as `direction_col`. By default this
  column labels a trip's direction; it does not cut trips. A trip has
  exactly one direction, so if `direction_col` takes more than one value
  inside a segment that `trip_col` declared to be one trip, the two
  inputs contradict each other and extraction stops with an error:
  either add the direction column to `trip_col` to segment by it, set
  `cut_on_direction_change = TRUE`, or collapse it to one value per
  supplied trip identity. Default `NULL`.

- cut_on_direction_change:

  Logical. When `TRUE`, a change in `direction_col` cuts a trip rather
  than raising the mid-trip direction conflict error above. Raw
  direction fields flip spuriously, so the change is debounced first:
  only a run of consecutive identical values that is both long enough in
  pings and long enough in seconds counts as a real direction change,
  and shorter contrary bursts are absorbed into the direction around
  them. Requires `direction_col`. Default `FALSE`, which leaves
  behaviour unchanged.

- direction_debounce_min_pings:

  Integer. Minimum consecutive pings for a direction run to be treated
  as a real direction change. Inert unless
  `cut_on_direction_change = TRUE`. Note this floor only binds on
  high-cadence feeds or at small `direction_debounce_min_seconds`: at a
  15 s ping cadence a run needs about `min_seconds / 15 + 1` pings to
  span the seconds threshold at all, so from roughly 300 s upwards the
  seconds threshold binds first. Default `2L`.

- direction_debounce_min_seconds:

  Numeric. Minimum intra-run span, in seconds, for a direction run to be
  treated as a real direction change. A single-ping run spans 0 seconds
  and is therefore always absorbed. Inert unless
  `cut_on_direction_change = TRUE`. Default 600.

- session_gap:

  Numeric. Successive observations of a vehicle further apart than this
  many seconds start a new driving session; trips never span sessions.
  This replaces the former calendar-date boundary, so overnight trips
  crossing midnight stay intact while overnight parking still separates
  one day's operations from the next. Default 4 hours (`4 * 3600`).

- segmentation:

  Character. How the raw-GPS spatial path cuts trips when no `trip_col`
  is supplied: `"terminals"` (classic two-terminal buffer model),
  `"layover"` (cut wherever the vehicle dwells longer than
  `layover_gap`, anywhere on the route - handles short-turn, loop, and
  multi-branch services; all trips share a single direction group), or
  `"auto"` (default: `"terminals"` when `terminals_data` is given,
  `"layover"` otherwise). Not used with `trip_col`.

- layover_gap:

  Numeric. Layover segmentation only: dwells longer than this many
  seconds bound trips - whether a silent gap between successive pings or
  a stationary spell (pings present but staying within
  `layover_radius`). Mode-dependent: must exceed the feed's ping
  interval and normal in-service stop dwell, and stay below the shortest
  real layover. Default 10 minutes (`10 * 60`).

- layover_radius:

  Numeric. Layover segmentation only: a vehicle counts as stationary
  while successive pings stay within this many meters of the dwell's
  first ping (anchoring absorbs GPS jitter while parked). Default 50.

- diagnostics_warn:

  Logical. Emit a one-line
  [`warning()`](https://rdrr.io/r/base/warning.html) when the run drops
  coverage worth surfacing (pings with no usable trip identity,
  unmatched segments, stops out of range) so unattended or agent-driven
  pipelines notice silent loss. The full breakdown is always available
  via
  [`g2g_diagnostics`](https://e-kotov.github.io/gps2gtfs/reference/g2g_diagnostics.md)
  regardless of this flag. Default
  `getOption("gps2gtfs.diagnostics_warn", TRUE)`.

## Value

A data.table containing extracted trip features. `start_time` and
`end_time` are absolute `POSIXct` times in the timezone of the input
timestamps. Rows are returned in a stable, backend-invariant order:
sorted by the internal integer `trip_id`. The result carries an
`attr(., "diagnostics")` coverage table (see
[`g2g_diagnostics`](https://e-kotov.github.io/gps2gtfs/reference/g2g_diagnostics.md)).
See the "Inference tables, not GTFS files" section of
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md).

## Details

Where the cuts fall is the whole problem, and there are three regimes.
If the data already carries trip identities (`trip_col`, e.g. a
GTFS-Realtime `trip_id`), segmentation follows them and nothing is
inferred. Otherwise the trips are inferred from the trajectory:
`segmentation = "terminals"` cuts on crossings of two known terminal
buffers, and `segmentation = "layover"` cuts wherever the vehicle dwells
longer than `layover_gap`, anywhere on the route, which is what handles
short-turns, loops and multi-branch service. `session_gap` bounds all
three: a vehicle unseen for that long starts a new driving session and
no trip spans one, so overnight trips stay intact while overnight
parking still separates one day's operations from the next.

Inferred boundaries are estimates, and `layover_gap` in particular is
mode- and feed-dependent: it must exceed the ping interval and the
normal in-service dwell, and stay below the shortest real layover. The
result carries a coverage table
([`g2g_diagnostics`](https://e-kotov.github.io/gps2gtfs/reference/g2g_diagnostics.md))
reporting what the run dropped, which is the way to tell a clean
extraction from a plausible-looking one.

To get stop times as well, use
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md),
which runs this segmentation and then matches the pings of each trip to
stop buffers.

## Examples

``` r
# \donttest{
data(g2g_data_gps)
data(g2g_data_terminals)
trips <- g2g_extract_trips(
  gps_data = g2g_data_gps,
  terminals_data = g2g_data_terminals,
  terminals_buffer_radius = 50
)
#> Starting Pipeline for extracting Trip Data using backend: rust
#> [INFO] Removed 56 duplicated (vehicle_id, timestamp) row(s).
#> [INFO] Auto-detected local UTM projection EPSG:32644 (Zone 44N)
#> Pipeline finished successfully!
#> Warning: gps2gtfs extraction lost coverage: kept 6 trips from 2,167 input pings. Dropped pings_dropped_zero_coord (28). Inspect the full coverage table with g2g_diagnostics(result) (also attr(result, "diagnostics")). Silence with diagnostics_warn = FALSE or options(gps2gtfs.diagnostics_warn = FALSE).
# }
```
