# Estimate stop coordinates from vehicle positions

Baseline-free helper: when no static GTFS feed exists, stop locations
can be estimated from GTFS-Realtime Vehicle Positions that carry a
`stop_id` annotation. Each stop's coordinates are the median position of
the pings observed at it — by default only pings labeled `STOPPED_AT`,
the most precise signal.

## Usage

``` r
g2g_stops_from_positions(positions, statuses = "STOPPED_AT", min_obs = 3L)
```

## Arguments

- positions:

  A data.frame of vehicle positions (e.g. from
  [`gtfsrealtime::read_gtfsrt_positions()`](https://projects.indicatrix.org/gtfsrealtime-r/reference/read_gtfsrt_positions.html)
  or
  [`g2g_clean_gps()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_clean_gps.md))
  with columns `stop_id`, `latitude`, `longitude`, and optionally
  `current_status`.

- statuses:

  Character vector of `current_status` values to use. Default
  `"STOPPED_AT"`. Ignored (all `stop_id`-annotated pings used, with a
  message) when the `current_status` column is absent.

- min_obs:

  Integer. Minimum number of pings required per stop; stops with fewer
  observations are dropped with a warning. Default `3`.

## Value

A data.table with columns `stop_id`, `latitude`, `longitude`, `n_obs`,
sorted by `stop_id`.

## Details

The result covers the `stop_id`/`latitude`/`longitude` columns of the
`stops_data` input to
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md);
the `direction` label (starting terminal of trips serving the stop) must
still be supplied, e.g. from the RT `trip_id`/`direction_id` annotations
or a baseline feed. It also fills the spec-required
`stop_lat`/`stop_lon` of a scaffolded `stops.txt`.
