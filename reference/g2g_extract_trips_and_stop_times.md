# Extract trips and stop times from GPS trajectories

The package's main entry point. Cuts a stream of vehicle GPS pings into
individual trips, then matches each trip's pings to stop buffers,
returning both a trip-feature table and a stop-time table with arrival,
departure and dwell per stop. Each of `gps_data`, `terminals_data` and
`stops_data` may be a data.frame or a path to a CSV, and either table
can optionally be written out.

## Usage

``` r
g2g_extract_trips_and_stop_times(
  gps_data,
  terminals_data = NULL,
  stops_data,
  terminals_buffer_radius = NULL,
  stops_buffer_radius,
  stops_extended_buffer_radius,
  output_trips_path = NULL,
  output_stops_path = NULL,
  projected_crs = NULL,
  backend = "auto",
  projected = NULL,
  stop_direction_map = NULL,
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
  return_trajectory = FALSE,
  diagnostics_warn = getOption("gps2gtfs.diagnostics_warn", TRUE),
  min_dwell = 0
)
```

## Arguments

- gps_data:

  A data.frame or path to the raw GPS CSV.

- terminals_data:

  A data.frame or path to the terminal coordinates CSV. Optional
  (`NULL`) with `segmentation = "layover"` or with `direction_col`;
  required for terminal-buffer segmentation.

- stops_data:

  A data.frame or path to the bus stops coordinates CSV. Its `direction`
  column groups stops for matching; with layover segmentation the column
  is optional and any labels are ignored (all stops match all trips).

- terminals_buffer_radius:

  Numeric. Buffer radius for terminals (in meters). Only consumed by
  terminal-buffer segmentation; may be omitted otherwise.

- stops_buffer_radius:

  Numeric. Buffer radius for bus stops (in meters).

- stops_extended_buffer_radius:

  Numeric. Extended buffer radius for bus stops (in meters).

- output_trips_path:

  Character. Optional path to write output trip features as CSV. Default
  is `NULL` (no file written).

- output_stops_path:

  Character. Optional path to write output stop times as CSV. Default is
  `NULL` (no file written).

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

- stop_direction_map:

  Named character vector mapping each raw stop-direction label to the
  terminal ID that trips in that direction *start from*. Required
  whenever `stops_data` labels its directions with anything other than
  the terminal IDs themselves: which label belongs to which terminal is
  not recoverable from the data (both direction groups span the same
  corridor), and getting it backwards silently reverses every direction
  in the output while still producing plausible stop times. Omitting it
  in that case is an error listing both candidate maps. Not needed when
  the labels are already terminal IDs, as with
  [`g2g_stops_from_gtfs`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_gtfs.md),
  nor in data-driven direction mode (`direction_col`) or layover
  segmentation.

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

- return_trajectory:

  Logical. Also return the ping-level trajectory (cleaned GPS records
  with assigned `trip_id` and `direction`) as a `trajectory` element —
  the input for
  [`g2g_shapes_from_trips`](https://e-kotov.github.io/gps2gtfs/reference/g2g_shapes_from_trips.md).
  Default `FALSE`.

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

- min_dwell:

  Numeric. Minimum dwell, in seconds, for a stop visit to be kept;
  larger values also drop short stops. Default `0` keeps every visit.
  Dwell is measured from zero-speed pings, so three kinds of visit have
  zero dwell: a vehicle that crossed the stop's buffer without stopping;
  one that stopped between two pings; and one seen stationary only on
  its last ping inside the buffer. `min_dwell = 1` removes all three, so
  it is not a pure pass-through filter. With pings a few seconds apart
  the last two are rare; at 15-30 s intervals they are common, and many
  visits to stops that were served are removed. Check the number
  dropped, reported in a message, before relying on it. Without a
  `speed` on any ping inside a stop buffer every dwell is 0, so any
  positive value drops every visit (with a warning).

## Value

A list containing two data.tables: `trips` and `stop_times`, plus
`trajectory` when `return_trajectory = TRUE`. All times (`start_time`,
`end_time`, `arrival_time`, `departure_time`) are absolute `POSIXct`
values in the timezone of the input timestamps — never clock strings, so
trips running past midnight stay unambiguous. In `stop_times`, `date`,
`day_of_week`, `is_weekday` and `hour_of_day` describe the visit's
`arrival_time` in that timezone.

Row order is a stable, backend-invariant contract: `trips` are ordered
by the internal integer `trip_id`, and `stop_times` by
`(trip_id, arrival_time, stop_id, departure_time)` with every remaining
column appended as a final tie-breaker. The sort is therefore total, so
the three backends (Rust, Rcpp, pure R) return identical row order for
identical input and summaries built on the result are reproducible
regardless of backend or parallelism.

The result also carries an `attr(., "diagnostics")` coverage table (see
[`g2g_diagnostics`](https://e-kotov.github.io/gps2gtfs/reference/g2g_diagnostics.md)).

## Details

Segmentation works exactly as in
[`g2g_extract_trips`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips.md) -
supplied `trip_col` identities, terminal buffers, or layover dwells, all
bounded by `session_gap` - and this function adds the stop-matching
stage on top. A ping is attributed to a stop when it falls inside
`stops_buffer_radius`, with `stops_extended_buffer_radius` as the wider
fallback; the stops of a trip are restricted to those labeled with its
direction, so `stops_data$direction` and the trips' direction labels
must use the same values (see `stop_direction_map`).

Both stages estimate. Trip boundaries can be misplaced without the trip
count looking wrong, and a stop the vehicle passed outside every buffer
simply produces no row. The returned tables therefore carry a coverage
table
([`g2g_diagnostics`](https://e-kotov.github.io/gps2gtfs/reference/g2g_diagnostics.md))
reporting pings with no usable trip identity, unmatched segments and
out-of-range stops, and that is what to read before treating the output
as complete.

The result is inference tables, not GTFS files; see the section below
for what still has to happen before this is a publishable feed.

## Inference tables, not GTFS files

The returned `trips` and `stop_times` use GTFS-style column names but
are \*inference tables\*, not valid `trips.txt`/ `stop_times.txt`: they
carry no `route_id`, `service_id`, `stop_sequence`, or GTFS clock
strings (`"HH:MM:SS"`, with `>24:00:00` for post-midnight stops). This
is deliberate: `route_id`, `service_id`, and canonical stop identities
cannot be inferred from coordinates alone, so `gps2gtfs` stops at
inference rather than synthesizing them.

Turning these tables into a standard-compliant feed — deriving
`stop_sequence`, attributing each trip to a service day, encoding
`>24:00:00` clock strings, and linking or synthesizing IDs — is the job
of the companion package `gtfsrt2static` (a separate, optional install),
which yields a feed the MobilityData `gtfs-validator` accepts:


    res    <- g2g_extract_trips_and_stop_times(...)
    events <- gtfsrt2static::snapshot_from_stop_times(
      res$stop_times, trip_id_col = "provided_trip_id"  # keep official IDs
    )
    feed   <- gtfsrt2static::snapshot_assemble(events)  # baseline: real IDs
    # or, with no planned feed to lean on:
    # feed <- gtfsrt2static::snapshot_scaffold(events, strict = TRUE)
    gtfsio::export_gtfs(feed, "realized_gtfs.zip")      # gtfstools/tidytransit-ready

Both tables carry a `provided_trip_id` column: the caller's official
trip identity (the `trip_col` value, e.g. a GTFS-Realtime `trip_id`)
when the fast path is used, `NA` otherwise. It lets a downstream
assembler preserve official trip IDs instead of synthesizing them. The
internal integer `trip_id` remains the join key between the two tables.

## Orientation and pattern labels (C5 contract)

Both tables also carry an additive, versioned set of inference-label
columns. They are the contract slots for a future baseline-free
orientation/turnaround-detection stage and are currently always in the
“no detector applied” state (no such detector is enabled yet), so the
legacy `direction` column - retained unchanged - is what downstream
consumers read today:

- `orientation_id`:

  Integer `0`/`1`, geometric travel direction only; `NA` when
  unavailable/abstained (always `NA` today).

- `orientation_status`:

  Factor, never `NA`. `"none"` (no orientation method applied) today;
  reserved values `"ok"` / `"single_group"` / `"abstain_<reason>"` make
  a future detector's abstentions observable rather than silent.

- `orientation_confidence`:

  Double in `[0,1]`, `NA` when `orientation_id` is `NA` (always `NA`
  today).

- `pattern_ref`:

  Character K-way branch/short-turn variant identity; reserved nullable
  (always `NA` today). The handoff to `gtfsrt2static`'s cross-trip
  stop-order stage.

- `start_anchor_ref`, `end_anchor_ref`:

  Character discovered turnaround-cluster ids; **`trips` only**
  (trip-level metadata, not per-stop), `NA` today.

Downstream, `gtfsrt2static` maps `orientation_id` into the GTFS
`direction_id` (falling back to `direction` while `orientation_id` is
`NA`); `pattern_ref` rides through to C6; anchors stay in C5.
`orientation_*`/`pattern_ref` propagate from `trips` onto `stop_times`
on the internal `trip_id`, the same mechanism as `provided_trip_id`.

## Examples

``` r
# \donttest{
data(g2g_data_gps)
data(g2g_data_terminals)
data(g2g_data_stops)
result <- g2g_extract_trips_and_stop_times(
  gps_data = g2g_data_gps,
  terminals_data = g2g_data_terminals,
  stops_data = g2g_data_stops,
  terminals_buffer_radius = 50,
  stops_buffer_radius = 30,
  stops_extended_buffer_radius = 50,
  # Each stop-direction label maps to the terminal its trips start from.
  # BT01 is Kandy, BT02 is Digana.
  stop_direction_map = c(
    "Kandy-Digana" = "BT01",
    "Digana-Kandy" = "BT02"
  )
)
#> Starting Pipeline for extracting Trip and Bus Stop Data using backend: rust
#> [INFO] Removed 56 duplicated (vehicle_id, timestamp) row(s).
#> [INFO] Auto-detected local UTM projection EPSG:32644 (Zone 44N)
#> Pipeline finished successfully!
#> Warning: gps2gtfs extraction lost coverage: kept 6 trips from 2,167 input pings. Dropped pings_dropped_not_in_trip (678), pings_dropped_zero_coord (28). Inspect the full coverage table with g2g_diagnostics(result) (also attr(result, "diagnostics")). Silence with diagnostics_warn = FALSE or options(gps2gtfs.diagnostics_warn = FALSE).
head(result$trips)
#>    trip_id vehicle_id       date start_terminal end_terminal direction
#>      <int>      <int>     <char>         <char>       <char>     <int>
#> 1:       1        116 2022-07-01           BT02         BT01         2
#> 2:       2        116 2022-07-01           BT01         BT02         1
#> 3:       3        116 2022-07-01           BT02         BT01         2
#> 4:       4        116 2022-07-01           BT01         BT02         1
#> 5:       5        116 2022-07-01           BT02         BT01         2
#> 6:       6        116 2022-07-01           BT01         BT02         1
#>             start_time            end_time provided_trip_id duration_in_mins
#>                 <POSc>              <POSc>           <char>            <num>
#> 1: 2022-07-01 06:52:06 2022-07-01 07:38:17             <NA>         46.18333
#> 2: 2022-07-01 07:56:29 2022-07-01 08:50:15             <NA>         53.76667
#> 3: 2022-07-01 10:46:59 2022-07-01 11:42:31             <NA>         55.53333
#> 4: 2022-07-01 12:49:00 2022-07-01 13:44:14             <NA>         55.23333
#> 5: 2022-07-01 15:19:18 2022-07-01 16:13:18             <NA>         54.00000
#> 6: 2022-07-01 16:51:39 2022-07-01 18:02:08             <NA>         70.48333
#>    day_of_week hour_of_day is_weekday orientation_id orientation_status
#>          <ord>       <int>     <lgcl>          <int>             <fctr>
#> 1:      Friday           6       TRUE             NA               none
#> 2:      Friday           7       TRUE             NA               none
#> 3:      Friday          10       TRUE             NA               none
#> 4:      Friday          12       TRUE             NA               none
#> 5:      Friday          15       TRUE             NA               none
#> 6:      Friday          16       TRUE             NA               none
#>    orientation_confidence pattern_ref start_anchor_ref end_anchor_ref
#>                     <num>      <char>           <char>         <char>
#> 1:                     NA        <NA>             <NA>           <NA>
#> 2:                     NA        <NA>             <NA>           <NA>
#> 3:                     NA        <NA>             <NA>           <NA>
#> 4:                     NA        <NA>             <NA>           <NA>
#> 5:                     NA        <NA>             <NA>           <NA>
#> 6:                     NA        <NA>             <NA>           <NA>
head(result$stop_times)
#>    trip_id vehicle_id       date direction stop_id        arrival_time
#>      <int>      <int>     <char>     <int>  <char>              <POSc>
#> 1:       1        116 2022-07-01         2     201 2022-07-01 06:57:49
#> 2:       1        116 2022-07-01         2     202 2022-07-01 07:00:12
#> 3:       1        116 2022-07-01         2     203 2022-07-01 07:04:34
#> 4:       1        116 2022-07-01         2     204 2022-07-01 07:07:18
#> 5:       1        116 2022-07-01         2     205 2022-07-01 07:10:57
#> 6:       1        116 2022-07-01         2     206 2022-07-01 07:12:48
#>         departure_time dwell_time_in_seconds day_of_week hour_of_day is_weekday
#>                 <POSc>                 <num>       <ord>       <int>     <lgcl>
#> 1: 2022-07-01 06:58:19                    30      Friday           6       TRUE
#> 2: 2022-07-01 07:00:57                    45      Friday           7       TRUE
#> 3: 2022-07-01 07:04:34                     0      Friday           7       TRUE
#> 4: 2022-07-01 07:07:33                    15      Friday           7       TRUE
#> 5: 2022-07-01 07:11:12                    15      Friday           7       TRUE
#> 6: 2022-07-01 07:13:48                    60      Friday           7       TRUE
#>    provided_trip_id orientation_id orientation_status orientation_confidence
#>              <char>          <int>             <fctr>                  <num>
#> 1:             <NA>             NA               none                     NA
#> 2:             <NA>             NA               none                     NA
#> 3:             <NA>             NA               none                     NA
#> 4:             <NA>             NA               none                     NA
#> 5:             <NA>             NA               none                     NA
#> 6:             <NA>             NA               none                     NA
#>    pattern_ref
#>         <char>
#> 1:        <NA>
#> 2:        <NA>
#> 3:        <NA>
#> 4:        <NA>
#> 5:        <NA>
#> 6:        <NA>
# }
```
