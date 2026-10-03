# Derive trip terminals from supplied route geometries

Determines the two terminals of a route from the route's own
linestrings, with no GTFS feed involved: each direction's terminal is
the *first* vertex of that direction's line, which is where a vehicle
running that direction starts. The result feeds directly into
`terminals_data` of
[`g2g_extract_trips`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips.md)
and
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md).

## Usage

``` r
g2g_terminals_from_geometries(geometries, route_id)
```

## Arguments

- geometries:

  The route geometries, in either of two forms:

  - a data.frame of vertices in long form, with columns `route_id`,
    `direction`, `vertex_seq` (the numeric order of the vertices within
    a direction), `latitude` and `longitude`; or

  - an `sf` object of `LINESTRING` or `MULTILINESTRING` geometries with
    `route_id` and `direction` columns (requires the 'sf' package).
    `MULTILINESTRING` parts are concatenated in part order.

- route_id:

  A single route identifier present in `geometries`. The gps2gtfs
  pipeline models one route (two terminals) at a time.

## Value

A data.table with columns `terminal_id`, `latitude`, `longitude`,
`direction`, one row per direction. `terminal_id` equals `direction`.

## Details

The `terminal_id` is the direction label itself. That is deliberate:
[`g2g_stops_from_geometries`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_geometries.md)
labels stops with the same values, so the stop labels already are
terminal IDs and no `stop_direction_map` is needed (the convention
[`g2g_stops_from_gtfs`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_gtfs.md)
documents).

Exactly two directions are required, because the terminal model is one
route with two ends. A route drawn as one loop, or with a third branch
variant, errors; use `segmentation = "layover"` for those. Two first
vertices a few meters apart are not two ends of a line either, and warn.

## See also

[`g2g_stops_from_geometries`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_geometries.md)
for the matching `stops_data`, and
[`g2g_terminals_from_gtfs`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_gtfs.md)
for the same table derived from a planned GTFS feed.

## Examples

``` r
geometries <- data.frame(
  route_id = "r1",
  direction = rep(c("east", "west"), each = 3),
  vertex_seq = rep(1:3, times = 2),
  latitude = c(7.29, 7.30, 7.31, 7.31, 7.30, 7.29),
  longitude = c(80.63, 80.64, 80.65, 80.65, 80.64, 80.63)
)
g2g_terminals_from_geometries(geometries, route_id = "r1")
#>    terminal_id latitude longitude direction
#>         <char>    <num>     <num>    <char>
#> 1:        east     7.29     80.63      east
#> 2:        west     7.31     80.65      west
```
