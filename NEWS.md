# gps2gtfs 0.2.0

## Breaking changes

* Canonical input and output column names now follow the GTFS-Realtime
  convention: `deviceid` is now `vehicle_id` and `devicetime` is now
  `timestamp`. The output of `gtfsrealtime::read_gtfsrt_positions()` is
  therefore accepted by the pipeline directly, with no adapter. Data in any
  other naming scheme can be mapped via the new `vehicle_col` and `time_col`
  arguments of `g2g_clean_gps()`, `g2g_extract_trips()`, and
  `g2g_extract_trips_and_stop_times()`.
* The example dataset `g2g_data_gps` uses the new column names.

## New features

* `g2g_clean_gps()` gains hygiene options for archived GTFS-Realtime feeds
  (useful for any GPS source): `dedupe = TRUE` drops repeated
  `(vehicle_id, timestamp)` observations, and `drop_missing_vehicle = TRUE`
  removes rows with missing vehicle identifiers with a warning instead of
  failing.
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
