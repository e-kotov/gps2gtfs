# gps2gtfs 0.3.1

* The pure R backend now ignores a passthrough `bus_stop` column while matching
  terminal visits, consistent with the compiled backends.

# gps2gtfs 0.3.0

## Breaking changes

* `stop_direction_map` is now required when `stops_data` labels its direction
  groups with anything other than the terminal IDs themselves. It was
  previously derived by pairing the two labels with the two terminals in order
  of appearance — a coin flip, since the label order follows however the caller
  happened to build `stops_data`. Losing that flip reverses every direction in
  the output while still producing plausible stop times, because each ping then
  matches the opposite-direction stop across the street. Which label belongs to
  which terminal is not recoverable from the data — both groups span the same
  corridor, so they sit at the same distance from both terminals — so the case
  is now an error listing both candidate maps rather than a guess. Callers
  whose labels are already terminal IDs (everyone using
  `g2g_stops_from_gtfs()`) and users of `direction_col` or layover
  segmentation are unaffected.

## Bug fixes

* Stop extraction no longer breaks when the GPS data carries a column of its
  own named `direction`. `g2g_clean_gps()` passes unknown columns through, and
  such a column collided with the trip-level direction merged into the
  trajectory: every direction group then matched zero pings and extraction
  failed with an error naming neither the column nor the cause. It affected
  every segmentation mode, not only the supplied-identity path. Columns whose
  names stop extraction reserves (`direction`, `bus_stop`, `stop_id`,
  `grouped_ends`, and the already-handled `trip_id`) are now dropped from the
  trajectory with a message. `g2g_extract_trips()` was never affected.

* `direction_col` now errors instead of silently discarding data when it takes
  more than one value inside a segment that `trip_col` declared to be one trip.
  A trip has exactly one direction, so the two inputs contradict each other;
  keeping the first value merged an out-and-back into a single multi-hour
  "trip" that directional routing rejects. The error names both resolutions
  (add the column to `trip_col`, or collapse it per trip identity). Direction
  that is constant within each supplied trip identity — the well-formed
  GTFS-Realtime case — is unaffected.

* `g2g_terminals_from_gtfs()` and `g2g_stops_from_gtfs()` no longer render
  numeric `stop_id`/`route_id` values in scientific notation (`1e+05`), which
  silently broke every downstream id join.

* `g2g_stops_from_gtfs()` warns when trips are dropped for not starting at
  either derived terminal, instead of silently omitting the stops they serve.

* `g2g_terminals_from_gtfs()` warns when the two derived terminals are close
  enough together to be two platforms of one place rather than the two ends of
  the route, and its loop-route error now points at `segmentation = "layover"`.


## New features

* `g2g_diagnostics()` gains `max_trip_duration_mins`, the longest extracted
  trip. Reported, never judged: a mis-segmented run merges a whole shift into
  one "trip" while the trip count stays plausible, and no other metric shows
  it.

* `trips` and `stop_times` gain an additive, versioned set of C5 inference-label
  columns for a future baseline-free orientation/turnaround-detection stage:
  `orientation_id`, `orientation_status`, `orientation_confidence`, and a
  reserved-nullable `pattern_ref` (both tables), plus trips-only
  `start_anchor_ref`/`end_anchor_ref`. No orientation detector is enabled yet,
  so every new column is in its empty state (`orientation_id`/`_confidence`/
  `pattern_ref`/anchors `NA`, `orientation_status = "none"`) and the legacy
  `direction` column is retained unchanged. The columns are appended after all
  pre-existing columns, so existing output is byte-for-byte compatible.
  `orientation_*`/`pattern_ref` propagate from `trips` onto `stop_times` on the
  internal `trip_id`, the same mechanism as `provided_trip_id`. See
  `?g2g_extract_trips_and_stop_times` ("Orientation and pattern labels").

# gps2gtfs 0.2.0

## New features

* New `segmentation = "layover"` mode in `g2g_extract_trips()` and
  `g2g_extract_trips_and_stop_times()` segments raw GPS without terminals or
  trip ids: trips are cut at dwells longer than `layover_gap` (default 10 min),
  detected as a silent inter-ping gap or a stationary spell within
  `layover_radius` (default 50 m). Handles short-turn, loop, and multi-branch
  routes; emits a single direction group. `segmentation = "auto"` (default)
  uses terminals when `terminals_data` is supplied, layover otherwise.
* New `trip_col` segments by supplied trip identities (fast path) instead of
  terminal-buffer inference; may name several columns that jointly identify a
  trip (e.g. the GTFS-Realtime TripDescriptor).
* New `direction_col` takes trip direction from the data instead of terminal
  inference, so `terminals_data` becomes optional and non-two-terminal routes
  (short-turns, variants, one-way) work.
* New `g2g_terminals_from_gtfs()` and `g2g_stops_from_gtfs()` derive
  `terminals_data`/`stops_data` from a baseline static GTFS feed.
* New `g2g_stops_from_positions()` estimates stop coordinates from `STOPPED_AT`
  vehicle positions; `g2g_shapes_from_trips()` (with `return_trajectory = TRUE`)
  builds a `shapes.txt`-shaped table of the geometry driven.
* `g2g_clean_gps()` gains `dedupe` and `drop_missing_vehicle` hygiene options,
  a `tz` argument (also on both pipeline entry points), row-`id` generation,
  and optional `speed`.
* `g2g_extract_trips()` and `g2g_extract_trips_and_stop_times()` now return
  rows in a documented, backend-invariant order — `trips` by `trip_id`,
  `stop_times` by `(trip_id, arrival_time, stop_id, departure_time)` — so
  summaries built on the output are reproducible regardless of backend or
  parallelism.
* `g2g_extract_trips()` and `g2g_extract_trips_and_stop_times()` now attach an
  extraction coverage diagnostics table (per-stage ping, segment, and trip
  counts) to the result, retrievable with the new `g2g_diagnostics()`, and
  emit a one-line `warning()` when a run drops coverage worth surfacing so
  unattended pipelines notice silent loss. Silence it with `diagnostics_warn =
  FALSE` or `options(gps2gtfs.diagnostics_warn = FALSE)`.

## Breaking changes

* Input/output columns follow the GTFS-Realtime convention (`vehicle_id`,
  `timestamp`); map other schemes via `vehicle_col`/`time_col`. `g2g_data_gps`
  uses the new names.
* Times in the returned tables are absolute `POSIXct` (input timezone), not
  `"HH:MM:SS"` strings — format yourself if clock strings are needed.
* Returned `trips`/`stop_times` are documented as inference tables, not valid
  GTFS `trips.txt`/`stop_times.txt` (no `route_id`, `service_id`,
  `stop_sequence`, or clock strings).
* Trips are segmented within driving sessions (`session_gap`, default 4 h)
  instead of calendar days, so midnight-crossing trips stay intact.

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
