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
  `stop_times` by `(trip_id, arrival_time, stop_id)` — so summaries built on
  the output are reproducible regardless of backend or parallelism.
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
