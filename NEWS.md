# gps2gtfs 0.2.0

## New features (baseline static GTFS + GTFS-RT trip identities)

* New `g2g_terminals_from_gtfs()` and `g2g_stops_from_gtfs()` derive the
  pipeline's `terminals_data` and `stops_data` inputs from a planned (baseline)
  static GTFS feed — a gtfsio/gtfstools-style object or a zip path (via the
  optional 'gtfsio' package) — instead of hand-built tables. Stop direction
  labels are the starting terminal ids, which the pipeline now maps onto
  terminals directly (identity mapping) without a manual `stop_direction_map`.
* New `trip_col` argument in `g2g_extract_trips()` and
  `g2g_extract_trips_and_stop_times()`: when positions already carry trip
  identities (e.g. GTFS-Realtime `trip_id`), trips are segmented by those
  identities (fast path) instead of inferred from terminal-buffer crossings.
  `trip_col` may name several columns that jointly identify a trip — e.g.
  `c("route_id", "direction_id", "start_date", "start_time")`, the GTFS-RT
  TripDescriptor — for feeds whose positions carry no `trip_id`.
* `direction` in trip features is now consistently integer.

## New features (baseline-free mode)

* New `g2g_stops_from_positions()` estimates stop coordinates from
  `stop_id`-annotated Vehicle Positions (median position of `STOPPED_AT`
  pings) — fills the spec-required `stop_lat`/`stop_lon` when no static feed
  exists.
* New `g2g_shapes_from_trips()` converts the ping-level trajectory of
  extracted trips into a `shapes.txt`-shaped table (one shape per trip,
  cumulative `shape_dist_traveled`) recording the geometry actually driven.
  Obtain the trajectory with the new
  `g2g_extract_trips_and_stop_times(..., return_trajectory = TRUE)`.

## Breaking changes

* Canonical input and output column names now follow the GTFS-Realtime
  convention: `deviceid` is now `vehicle_id` and `devicetime` is now
  `timestamp`. The output of `gtfsrealtime::read_gtfsrt_positions()` is
  therefore accepted by the pipeline directly, with no adapter. Data in any
  other naming scheme can be mapped via the new `vehicle_col` and `time_col`
  arguments of `g2g_clean_gps()`, `g2g_extract_trips()`, and
  `g2g_extract_trips_and_stop_times()`.
* The example dataset `g2g_data_gps` uses the new column names.
* All times in the returned tables are now absolute `POSIXct` values in the
  timezone of the input timestamps: `arrival_time`/`departure_time` in
  `$stop_times` and `start_time`/`end_time` in `$trips` were previously
  `"HH:MM:SS"` strings, which wrapped at midnight and silently corrupted
  post-midnight stops of overnight trips. Format with
  `format(x, "%H:%M:%S")` if clock strings are needed; GTFS clock encoding
  (including `>24:00:00`) belongs to downstream feed assembly.
* The returned `trips` and `stop_times` are documented as *inference
  tables*, not valid GTFS `trips.txt`/`stop_times.txt` (no `route_id`,
  `service_id`, `stop_sequence`, or GTFS clock strings); see
  `?g2g_extract_trips_and_stop_times`.
* New `direction_col` argument (used with `trip_col`): trip direction is
  taken from the data (e.g. GTFS-Realtime `direction_id`) instead of inferred
  from which of two terminals a trip started at. This makes `terminals_data`
  optional and handles routes that are not simple two-terminal lines —
  short-turns, variants, one-way services — which empirically are the
  majority of real routes (only ~18-27% of routes in the HSL and OVapi feeds
  are strict two-terminal). Stop matching now works for any number of
  direction groups. The classic two-terminal behavior is unchanged when
  `direction_col` is `NULL`.
* When `trip_col` is supplied and the data has no usable `vehicle_id` (some
  feeds, e.g. OVapi, leave it empty), a placeholder is filled instead of
  dropping every row, since trip identities drive segmentation.
* Trips are now segmented within *driving sessions* instead of calendar
  days: previously an overnight trip crossing midnight was silently dropped
  (spatial path) or split in two (`trip_col` fast path). A new session
  starts when a vehicle goes unseen for longer than the new `session_gap`
  argument (default 4 hours), so midnight-crossing trips survive while
  overnight parking still separates one day's operations from the next.

## New features

* `g2g_clean_gps()` gains hygiene options for archived GTFS-Realtime feeds
  (useful for any GPS source): `dedupe = TRUE` drops repeated
  `(vehicle_id, timestamp)` observations, and `drop_missing_vehicle = TRUE`
  removes rows with missing vehicle identifiers with a warning instead of
  failing.
* `g2g_clean_gps()`, `g2g_extract_trips()`, and
  `g2g_extract_trips_and_stop_times()` gain a `tz` argument controlling the
  timezone in which non-`POSIXct` timestamps are parsed — and therefore the
  timezone whose midnight bounds service days and every downstream GTFS
  clock string. Character timestamps parsed without an explicit `tz` now
  warn (and fall back to UTC) instead of silently reinterpreting local times
  as UTC, which could shift service dates around midnight.
* `g2g_clean_gps()` now generates row `id`s when the column is absent or not
  unique, treats `speed` as optional (with a warning, since dwell-time
  estimation relies on `speed == 0`), passes non-canonical columns through
  untouched, and preserves the timezone of `POSIXct` timestamps so service
  days split at feed-local midnight.

# gps2gtfs 0.1.1

* `day_of_week` is now an ordered factor from Monday through Sunday instead
  of a raw integer; use `as.integer(day_of_week)` for ISO weekday numbers
  (`1` = Monday, ..., `7` = Sunday).
* `g2g_extract_trips()` and `g2g_extract_trips_and_stop_times()` now return
  logical `is_weekday` values for weekday/weekend filtering.

# gps2gtfs 0.1.0
* Initial CRAN release.
* Added high-performance Rust and Rcpp backends for GPS to GTFS extraction.
* Exported example datasets (`g2g_data_gps`, `g2g_data_terminals`, `g2g_data_stops`).
