# Get started with gps2gtfs

`gps2gtfs` turns raw GPS pings from transit vehicles into GTFS-shaped
`trips` and `stop_times` tables. This article runs the whole pipeline on
the data that ships with the package: one day of pings from a bus on
route 654 between Kandy and Digana, Sri Lanka.

``` r

library(gps2gtfs)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
```

## The inputs

The extractor needs three tables:

- **GPS pings**: one row per position report, with a vehicle id,
  coordinates, a timestamp and, ideally, a speed.
- **Terminals**: the points where trips start and end.
- **Stops**: the stops of each direction of the route.

``` r

head(g2g_data_gps)
#>            id vehicle_id           timestamp latitude longitude   speed
#>         <int>      <int>              <POSc>    <num>     <num>   <num>
#> 1: 1227693436        116 2022-07-01 05:39:42 0.000000   0.00000  0.0000
#> 2: 1227693437        116 2022-07-01 05:39:57 0.000000   0.00000  0.0000
#> 3: 1227693438        116 2022-07-01 05:40:12 0.000000   0.00000  0.0000
#> 4: 1227693439        116 2022-07-01 05:40:20 7.294970  80.73560 17.8186
#> 5: 1227698167        116 2022-07-01 05:40:21 7.295032  80.73564 15.1188
#> 6: 1227698168        116 2022-07-01 05:40:36 7.294980  80.73559  0.0000
g2g_data_terminals
#>    terminal_id terminal_name latitude longitude
#>         <char>        <char>    <num>     <num>
#> 1:        BT01         Kandy 7.292462  80.63498
#> 2:        BT02        Digana 7.298960  80.73472
head(g2g_data_stops)
#>    stop_id route_id    direction                   address latitude longitude
#>      <int>    <int>       <char>                    <char>    <num>     <num>
#> 1:     101      654 Kandy-Digana                Wales Park 7.291186  80.63766
#> 2:     102      654 Kandy-Digana                  Mahamaya 7.287840  80.64584
#> 3:     103      654 Kandy-Digana          Lewella junction 7.294430  80.65003
#> 4:     104      654 Kandy-Digana                  Talwatta 7.286701  80.66034
#> 5:     105      654 Kandy-Digana       Tennekumbura Bridge 7.281866  80.66603
#> 6:     106      654 Kandy-Digana Kalapura Junction Busstop 7.279830  80.67621
```

The stops are labelled by direction (`Kandy-Digana`, `Digana-Kandy`).
Which label belongs to which terminal cannot be recovered from the data,
because both directions run along the same road. You state it once, as a
map from each label to the terminal its trips *start* from:

``` r

direction_map <- c("Kandy-Digana" = "BT01", "Digana-Kandy" = "BT02")
```

## Step 1: clean the pings

[`g2g_clean_gps()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_clean_gps.md)
normalises the column names, drops pings with a zero latitude or
longitude (the usual sentinel for a missing fix) and pings without a
vehicle id, removes repeated `(vehicle_id, timestamp)` reports, and
sorts each vehicle’s pings in time. The extractor calls it internally,
so you only call it yourself to inspect the result:

``` r

gps <- g2g_clean_gps(g2g_data_gps)
#> [INFO] Removed 56 duplicated (vehicle_id, timestamp) row(s).
nrow(g2g_data_gps)
#> [1] 2167
nrow(gps)
#> [1] 2083
head(gps)
#> Key: <vehicle_id, date, time_str>
#>            id vehicle_id           timestamp latitude longitude   speed
#>         <int>      <int>              <POSc>    <num>     <num>   <num>
#> 1: 1227693439        116 2022-07-01 05:40:20 7.294970  80.73560 17.8186
#> 2: 1227698167        116 2022-07-01 05:40:21 7.295032  80.73564 15.1188
#> 3: 1227698168        116 2022-07-01 05:40:36 7.294980  80.73559  0.0000
#> 4: 1227698169        116 2022-07-01 05:40:51 7.294977  80.73559  0.0000
#> 5: 1227698170        116 2022-07-01 05:41:06 7.294978  80.73559  0.0000
#> 6: 1227698172        116 2022-07-01 05:41:21 7.294978  80.73559  0.0000
#>          date time_str
#>        <char>   <char>
#> 1: 2022-07-01 05:40:20
#> 2: 2022-07-01 05:40:21
#> 3: 2022-07-01 05:40:36
#> 4: 2022-07-01 05:40:51
#> 5: 2022-07-01 05:41:06
#> 6: 2022-07-01 05:41:21
```

The first pings of the day sit at `(0, 0)` and are dropped.

## Step 2: extract trips and stop times

[`g2g_extract_trips_and_stop_times()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md)
cuts each vehicle’s trajectory into trips between the terminal buffers,
then finds the visits to each stop of the trip’s direction. The buffer
radii are in metres.

``` r

result <- g2g_extract_trips_and_stop_times(
  gps_data = g2g_data_gps,
  terminals_data = g2g_data_terminals,
  stops_data = g2g_data_stops,
  terminals_buffer_radius = 50,
  stops_buffer_radius = 30,
  stops_extended_buffer_radius = 50,
  stop_direction_map = direction_map,
  return_trajectory = TRUE
)
#> Starting Pipeline for extracting Trip and Bus Stop Data using backend: rust
#> [INFO] Removed 56 duplicated (vehicle_id, timestamp) row(s).
#> [INFO] Auto-detected local UTM projection EPSG:32644 (Zone 44N)
#> Pipeline finished successfully!
#> Warning: gps2gtfs extraction lost coverage: kept 6 trips from 2,167 input
#> pings. Dropped pings_dropped_not_in_trip (678), pings_dropped_zero_coord (28).
#> Inspect the full coverage table with g2g_diagnostics(result) (also attr(result,
#> "diagnostics")). Silence with diagnostics_warn = FALSE or
#> options(gps2gtfs.diagnostics_warn = FALSE).
```

The bus made six trips, alternating between the two terminals:

``` r

result$trips[, .(trip_id, start_terminal, end_terminal, direction,
                 start_time, end_time, duration_in_mins)]
#>    trip_id start_terminal end_terminal direction          start_time
#>      <int>         <char>       <char>     <int>              <POSc>
#> 1:       1           BT02         BT01         2 2022-07-01 06:52:06
#> 2:       2           BT01         BT02         1 2022-07-01 07:56:29
#> 3:       3           BT02         BT01         2 2022-07-01 10:46:59
#> 4:       4           BT01         BT02         1 2022-07-01 12:49:00
#> 5:       5           BT02         BT01         2 2022-07-01 15:19:18
#> 6:       6           BT01         BT02         1 2022-07-01 16:51:39
#>               end_time duration_in_mins
#>                 <POSc>            <num>
#> 1: 2022-07-01 07:38:17         46.18333
#> 2: 2022-07-01 08:50:15         53.76667
#> 3: 2022-07-01 11:42:31         55.53333
#> 4: 2022-07-01 13:44:14         55.23333
#> 5: 2022-07-01 16:13:18         54.00000
#> 6: 2022-07-01 18:02:08         70.48333
```

Each stop visit has an arrival, a departure and a dwell time:

``` r

head(result$stop_times[, .(trip_id, stop_id, arrival_time, departure_time,
                           dwell_time_in_seconds)], 10)
#>     trip_id stop_id        arrival_time      departure_time
#>       <int>  <char>              <POSc>              <POSc>
#>  1:       1     201 2022-07-01 06:57:49 2022-07-01 06:58:19
#>  2:       1     202 2022-07-01 07:00:12 2022-07-01 07:00:57
#>  3:       1     203 2022-07-01 07:04:34 2022-07-01 07:04:34
#>  4:       1     204 2022-07-01 07:07:18 2022-07-01 07:07:33
#>  5:       1     205 2022-07-01 07:10:57 2022-07-01 07:11:12
#>  6:       1     206 2022-07-01 07:12:48 2022-07-01 07:13:48
#>  7:       1     207 2022-07-01 07:14:39 2022-07-01 07:14:39
#>  8:       1     208 2022-07-01 07:17:17 2022-07-01 07:17:32
#>  9:       1     209 2022-07-01 07:21:09 2022-07-01 07:21:24
#> 10:       1     210 2022-07-01 07:28:21 2022-07-01 07:28:36
#>     dwell_time_in_seconds
#>                     <num>
#>  1:                    30
#>  2:                    45
#>  3:                     0
#>  4:                    15
#>  5:                    15
#>  6:                    60
#>  7:                     0
#>  8:                    15
#>  9:                    15
#> 10:                    15
```

All times are absolute `POSIXct` values in the timezone of the input,
not GTFS clock strings, so a trip that runs past midnight stays
unambiguous. These are inference tables, not GTFS files yet: they carry
no `route_id`, `service_id` or `stop_sequence`. Turning them into a
valid feed is the job of
[`gtfsrt2static`](https://e-kotov.github.io/gtfsrt2static/); see [From
raw GPS to a realized GTFS
feed](https://e-kotov.github.io/gtfsrt2static/articles/pipeline.html).

## Step 3: check what was kept

The extraction above warned that it lost coverage. Every stage records
how many pings, segments and trips it kept or dropped, so a loss is
visible rather than silent:

``` r

g2g_diagnostics(result)
#> <gps2gtfs extraction diagnostics>
#> 2,167 pings in -> 6 trips, 80 stop_times
#>   dropped:
#>     pings_dropped_not_in_trip: 678
#>     pings_dropped_duplicate: 56
#>     pings_dropped_zero_coord: 28
#> 
#>            stage                        metric     n
#>           <char>                        <char> <int>
#>  1:        input                      pings_in  2167
#>  2:     cleaning      pings_dropped_zero_coord    28
#>  3:     cleaning pings_dropped_missing_vehicle     0
#>  4:     cleaning       pings_dropped_duplicate    56
#>  5:     cleaning          pings_after_cleaning  2083
#>  6: segmentation rows_dropped_no_trip_identity    NA
#>  7: segmentation  segments_dropped_single_ping    NA
#>  8: segmentation   segments_dropped_stationary    NA
#>  9:        trips       pings_assigned_to_trips  1405
#> 10:        trips     pings_dropped_not_in_trip   678
#> 11:        trips                    trips_kept     6
#> 12:        trips        max_trip_duration_mins    70
#> 13:        stops               stop_times_kept    80
```

Here the pings dropped at the trip stage fall outside every trip: the
bus standing at a terminal between runs, before its first run and after
its last. Cleaning removed the pings with a zero coordinate and the
repeated reports. A large drop at another stage, such as rows without a
trip identity or stops out of buffer range, is the signal to revisit the
inputs or the radii. Silence the warning with `diagnostics_warn = FALSE`
once you have looked.

## Step 4: the shapes actually driven

With `return_trajectory = TRUE` the result also carries the ping-level
trajectory of each trip.
[`g2g_shapes_from_trips()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_shapes_from_trips.md)
turns it into a table shaped like GTFS `shapes.txt`, with one shape per
trip. Unlike planned shapes, these record the road the vehicle took,
detours included:

``` r

shapes <- g2g_shapes_from_trips(result$trajectory)
head(shapes)
#>    shape_id shape_pt_lat shape_pt_lon shape_pt_sequence shape_dist_traveled
#>      <char>        <num>        <num>             <int>               <num>
#> 1:    SHP_1     7.299112     80.73459                 1             0.00000
#> 2:    SHP_1     7.298897     80.73403                 2            66.57977
#> 3:    SHP_1     7.298092     80.73311                 3           201.88920
#> 4:    SHP_1     7.297458     80.73226                 4           318.99126
#> 5:    SHP_1     7.297447     80.73189                 5           359.82087
#> 6:    SHP_1     7.297448     80.73190                 6           360.58363
shapes[, .(points = .N, km = round(max(shape_dist_traveled) / 1000, 1)),
       by = shape_id]
#>    shape_id points    km
#>      <char>  <int> <num>
#> 1:    SHP_1    194  15.8
#> 2:    SHP_2    221  16.1
#> 3:    SHP_3    233  16.0
#> 4:    SHP_4    236  16.1
#> 5:    SHP_5    229  16.0
#> 6:    SHP_6    292  16.1
```

## Backends

The heavy steps run in one of three backends: compiled Rust (when a Rust
toolchain was available at install time), C++ through Rcpp, or pure R
with `data.table` and `sf`. `backend = "auto"` takes the first
available, in that order. The startup message of each run names the
backend used. The output does not depend on it: same rows, same values,
same row order.

``` r

rcpp <- g2g_extract_trips_and_stop_times(
  gps_data = g2g_data_gps,
  terminals_data = g2g_data_terminals,
  stops_data = g2g_data_stops,
  terminals_buffer_radius = 50,
  stops_buffer_radius = 30,
  stops_extended_buffer_radius = 50,
  stop_direction_map = direction_map,
  backend = "rcpp",
  diagnostics_warn = FALSE
)
#> Starting Pipeline for extracting Trip and Bus Stop Data using backend: rcpp
#> [INFO] Removed 56 duplicated (vehicle_id, timestamp) row(s).
#> [INFO] Auto-detected local UTM projection EPSG:32644 (Zone 44N)
#> Pipeline finished successfully!
identical(rcpp$stop_times, result$stop_times)
#> [1] TRUE
```

## Where next

- [GTFS-Realtime Vehicle Positions to
  GTFS](https://e-kotov.github.io/gps2gtfs/articles/gtfs-realtime.md):
  reading archived feeds with `gtfsrealtime`, deriving stops and
  terminals from a planned feed, and skipping trip inference when the
  pings carry a `trip_id`.
- [From raw GPS to a realized GTFS
  feed](https://e-kotov.github.io/gtfsrt2static/articles/pipeline.html):
  assembling these tables into a standard-compliant GTFS feed with
  `gtfsrt2static` and reading it back with `gtfstools`.
