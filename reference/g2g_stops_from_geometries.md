# Derive a stops table from supplied route geometries

Builds the `stops_data` input for
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md)
from the route's own linestrings and a plain table of stop coordinates,
with no GTFS feed involved: a stop belongs to a direction when it lies
within `buffer_m` of that direction's line.

## Usage

``` r
g2g_stops_from_geometries(geometries, stops, route_id, buffer_m = 50)
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

- stops:

  A data.frame of stop coordinates with columns `stop_id`, `latitude`,
  `longitude`. Any other columns are dropped. Rows with a missing
  coordinate, or a missing or blank `stop_id`, are dropped with a
  warning giving the counts.

- route_id:

  A single route identifier present in `geometries`. The gps2gtfs
  pipeline models one route (two terminals) at a time.

- buffer_m:

  Numeric. Maximum distance in meters from a direction's line for a stop
  to be assigned to that direction. Default `50`.

## Value

A data.table with columns `stop_id`, `latitude`, `longitude`,
`direction` (the `terminal_id` of the direction the stop was matched
to), sorted by `direction` then `stop_id`. Stops matched in both
directions appear once per direction. Zero rows, with the same four
columns and types, when no stop is within `buffer_m` of either
direction.

## Details

Distance is measured to the *polyline*, not to its vertices. A stop
sitting halfway along a 300 m segment is on the route; a nearest-vertex
rule would measure 150 m and drop it.

Direction labels are the `terminal_id`s that
[`g2g_terminals_from_geometries`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_geometries.md)
emits for the same input, so the two tables drop into `terminals_data`
and `stops_data` together and no `stop_direction_map` is needed.

A stop within `buffer_m` of both lines - the usual case for the two
sides of one street - is returned once per direction, which is what
makes the direction labels do any work. Stops farther than `buffer_m`
from every direction are simply absent, and their count is reported in a
message. If that is every stop, the result is a zero-row table and a
warning, not an error, so a caller looping over many routes can take
that answer for one of them. Terminals are not treated specially and are
not removed.

## See also

[`g2g_terminals_from_geometries`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_geometries.md)
for the matching `terminals_data`, and
[`g2g_stops_from_gtfs`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_gtfs.md)
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
stops <- data.frame(
  stop_id = c("S1", "S2"),
  latitude = c(7.295, 7.305),
  longitude = c(80.635, 80.645)
)
g2g_stops_from_geometries(geometries, stops, route_id = "r1")
#> Key: <direction, stop_id>
#>    stop_id latitude longitude direction
#>     <char>    <num>     <num>    <char>
#> 1:      S1    7.295    80.635      east
#> 2:      S2    7.305    80.645      east
#> 3:      S1    7.295    80.635      west
#> 4:      S2    7.305    80.645      west
```
