# Derive a stops table from a static GTFS feed

Builds the `stops_data` input for
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md)
from a planned (baseline) GTFS feed: all non-terminal stops served by a
route, labeled with the direction group they belong to. Direction labels
are the `terminal_id` a trip starts from, so they map onto the terminals
derived by
[`g2g_terminals_from_gtfs`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_gtfs.md)
without a manual `stop_direction_map`. Note this means the labels are
terminal `stop_id`s, not GTFS `direction_id` values; they are not
interchangeable with a `direction_col` taken from the positions.

## Usage

``` r
g2g_stops_from_gtfs(gtfs, route_id)
```

## Arguments

- gtfs:

  A GTFS feed object (named list of data.frames, as returned by
  [`gtfsio::import_gtfs()`](https://r-transit.github.io/gtfsio/reference/import_gtfs.html)
  or `gtfstools::read_gtfs()`) or a path to a GTFS zip file (requires
  the 'gtfsio' package).

- route_id:

  A single route identifier present in `trips.txt`. The gps2gtfs
  pipeline models one route (two terminals) at a time.

## Value

A data.table with columns `stop_id`, `latitude`, `longitude`,
`direction` (the starting terminal_id of trips serving the stop in that
direction). Stops served in both directions appear once per direction.

## Details

Trips that start at neither derived terminal have no direction group and
are dropped with a warning, so stops served only by short turns or
branch variants do not appear in the result.

## Examples

``` r
gtfs <- list(
  trips = data.frame(trip_id = c("t1", "t2"), route_id = "r1"),
  stop_times = data.frame(
    trip_id = c("t1", "t1", "t1", "t2", "t2", "t2"),
    stop_id = c("A", "S1", "B", "B", "S1", "A"),
    stop_sequence = c(1, 2, 3, 1, 2, 3)
  ),
  stops = data.frame(
    stop_id = c("A", "S1", "B"),
    stop_lat = c(7.29, 7.30, 7.31),
    stop_lon = c(80.63, 80.64, 80.65)
  )
)
g2g_stops_from_gtfs(gtfs, route_id = "r1")
#> Key: <direction, stop_id>
#>    stop_id latitude longitude direction
#>     <char>    <num>     <num>    <char>
#> 1:      S1      7.3     80.64         A
#> 2:      S1      7.3     80.64         B
```
