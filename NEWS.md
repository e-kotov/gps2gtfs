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
