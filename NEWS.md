# gps2gtfs 0.5.0

Two new exported functions. **Nothing else changed: every existing function behaves
exactly as in 0.4.0.**

* **`g2g_terminals_from_geometries()` and `g2g_stops_from_geometries()`** build the
  `terminals_data` and `stops_data` inputs of
  `g2g_extract_trips_and_stop_times()` from the route's own linestrings and a plain
  table of stop coordinates. They are the GTFS-free analogues of
  `g2g_terminals_from_gtfs()` and `g2g_stops_from_gtfs()`, and return the same
  shapes, for operators who publish route geometry and a stop list but no static
  feed.

* **Input shape.** `geometries` is a data.frame of vertices in long form with columns
  `route_id`, `direction`, `vertex_seq` (the numeric order of the vertices within a
  direction), `latitude` and `longitude` — which is what three lines of any
  JSON/GeoJSON reader produce, and needs no spatial dependency. An `sf` object of
  `LINESTRING` or `MULTILINESTRING` geometries with `route_id` and `direction`
  columns is also accepted when the (already suggested) 'sf' package is installed;
  `MULTILINESTRING` parts are concatenated in part order.

* **Each direction's terminal is the first vertex of its line**, and the
  `terminal_id` *is* the direction label. Because `g2g_stops_from_geometries()`
  labels stops with those same values, the stop labels already are terminal IDs and
  **no `stop_direction_map` is needed**. Exactly two directions are required; a loop
  or a third branch errors and points at `segmentation = "layover"`. Two lines whose
  first vertices are closer than 150 m warn, because that usually means one direction
  was drawn twice.

* **A stop belongs to a direction when it is within `buffer_m` (default 50 m) of that
  direction's polyline — measured to the line segments, not to the vertices.** A stop
  sitting halfway along a 300 m segment is on the route; a nearest-vertex rule would
  measure 150 m and drop it. Distances are computed on a local equirectangular
  projection in metres, vectorised over stops by segments. A stop within `buffer_m`
  of both lines (the two sides of one street) is returned once per direction, which
  is what makes the direction labels do any work. Stops matched by no direction are
  absent and their count is reported in a message; if that is every stop, the result
  is a zero-row table with the same four columns and a warning, not an error, so a
  caller looping over many routes can take that answer for one of them. Rows with a
  missing coordinate or a missing or blank `stop_id` are dropped with a warning
  giving the counts.

# gps2gtfs 0.4.0

New feature, opt-in and default-off. **Callers who do not set the new arguments get
byte-identical behaviour to 0.3.2.**

* **`g2g_extract_trips()` and `g2g_extract_trips_and_stop_times()` can now cut a trip
  when the supplied direction changes.** Three new arguments, all with
  backward-compatible defaults:

    * `cut_on_direction_change` (default `FALSE`) — when `TRUE`, a change in the
      `direction_col` value ends the current trip and starts a new one. When `FALSE`,
      nothing changes: direction is still only labelled, exactly as in 0.3.2.
    * `direction_debounce_min_pings` (default `2L`) — minimum consecutive pings a
      direction run needs before it is allowed to cut.
    * `direction_debounce_min_seconds` (default `600`) — minimum span, in seconds, a
      direction run needs before it is allowed to cut.

  Both thresholds must be met for a run to qualify (`counts >= N` **and**
  `spans >= M`), so a single contrary ping, or a brief flicker at a stop, no longer
  splits a trip. `cut_on_direction_change = TRUE` without `direction_col` is an error
  rather than a silent no-op. The two thresholds are validated: `min_pings` must be one
  whole number `>= 1`, `min_seconds` one non-negative finite number.

* **Why it exists.** The `netmob26-transport-justice` evaluation reported that the
  directional fast path labelled segments without splitting them, producing 12-14 hour
  trips. 0.3.x fixed the package's *response* (it now errors on conflicting direction
  inside one supplied trip identity); this release adds the cutting behaviour itself,
  which was deferred at the time because a naive cut is not good enough — cutting on
  every raw direction change inflates trip counts by about 17% and pushes fragmentation
  from 0.40 to 0.57. The debounce is what makes the cut usable.

* **What the evaluation showed, on one dataset.** Against a pre-registered, frozen
  acceptance gate on held-out Niterói vehicles (`nitD11`, `nitD12`), the default
  `N = 2` / `M = 600 s` setting improved fragmentation, downstream frequency MAPE and
  median trip-duration error relative to a naive route+direction cut, and met every
  registered bar. It is **worse** than that naive baseline on recovery and merge rate,
  which the gate did not require it to beat. This is one deterministic vehicle split of
  two service days on one operator: it is evidence that the feature works, not a claim
  about other feeds. Evaluate on your own data before relying on it.

# gps2gtfs 0.3.2

Documentation only. No user-visible behaviour changed, and no function, argument
or return value was renamed or added.

* **`g2g_extract_trips()` and `g2g_extract_trips_and_stop_times()` no longer
  claim to read CSV files.** Both descriptions opened "Reads raw GPS and terminal
  CSV files", which was never true: each of `gps_data`, `terminals_data` and
  `stops_data` accepts a data.frame or a path, exactly as their own `@param`
  entries already said. The two front doors also had one sentence each while
  smaller helpers had a reasoned paragraph, so both are now written to that
  depth - which segmentation regime applies when, what `session_gap` bounds, what
  the stop-matching stage can silently drop, and why `g2g_diagnostics()` is the
  thing to read before trusting the output.

* **`g2g_data_stops` and `g2g_data_terminals` now state their provenance.** Both
  are documented as working with `g2g_data_gps`, which says it comes from Kandy,
  Sri Lanka; they did not, so the trio read as three unrelated sample tables.
  Their titles are now parallel as well.

* **Titles are sentence case throughout**, per the tidyverse convention the
  package mostly already followed.

* Regenerating the documentation with the current roxygen2 also rewrote
  `NAMESPACE`'s `importFrom(data.table, ...)` into its multi-argument form and
  replaced `RoxygenNote` with `Config/roxygen2/version`. Both are equivalent to
  what they replaced.

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
