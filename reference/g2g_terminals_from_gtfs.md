# Derive trip terminals from a static GTFS feed

Determines the two terminals of a route from a planned (baseline) GTFS
feed: the two most frequent trip endpoints (first/last stop per trip in
`stop_times.txt`). The result feeds directly into `terminals_data` of
[`g2g_extract_trips`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips.md)
and
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md).

## Usage

``` r
g2g_terminals_from_gtfs(gtfs, route_id)
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

A data.table with columns `terminal_id`, `latitude`, `longitude`.

## Details

Frequency ranking cannot tell two ends of a line from two platforms of
one place, so the function warns when fewer than 90% of trip endpoints
match the two it derived (variants or branches) and when the two are
close enough together to be the same physical terminal under
platform-level `stop_id`s. Loop routes have no two terminals at all and
error; use `segmentation = "layover"` for those.

## Examples

``` r
gtfs <- list(
  trips = data.frame(trip_id = c("t1", "t2"), route_id = "r1"),
  stop_times = data.frame(
    trip_id = c("t1", "t1", "t2", "t2"),
    stop_id = c("A", "B", "B", "A"),
    stop_sequence = c(1, 2, 1, 2)
  ),
  stops = data.frame(
    stop_id = c("A", "B"),
    stop_lat = c(7.29, 7.31),
    stop_lon = c(80.63, 80.65)
  )
)
g2g_terminals_from_gtfs(gtfs, route_id = "r1")
#>    terminal_id latitude longitude
#>         <char>    <num>     <num>
#> 1:           A     7.29     80.63
#> 2:           B     7.31     80.65
```
