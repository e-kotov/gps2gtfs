# GTFS-Realtime Vehicle Positions to GTFS, natively

GTFS-Realtime Vehicle Positions are raw GPS pings with transit
annotations — exactly the data `gps2gtfs` converts into GTFS-shaped
`trips` and `stop_times` tables. Since version 0.2.0, the package’s
canonical column names follow the GTFS-Realtime convention
(`vehicle_id`, `latitude`, `longitude`, `timestamp`, `speed`), so the
output of
[`gtfsrealtime::read_gtfsrt_positions()`](https://projects.indicatrix.org/gtfsrealtime-r/reference/read_gtfsrt_positions.html)
feeds the pipeline **directly, with no adapter**.

``` r

library(gps2gtfs)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
```

## Reading an archived feed

The [gtfsrealtime](https://cran.r-project.org/package=gtfsrealtime)
package parses GTFS-RT protobuf files — single snapshots, gzip/bzip2
files, or a daily ZIP of archived polls — into a plain data frame:

``` r

positions <- gtfsrealtime::read_gtfsrt_positions(
  system.file("nyc-vehicle-positions.pb.bz2", package = "gtfsrealtime"),
  timezone = "America/New_York"
)
names(positions)
#>  [1] "id"                            "latitude"                     
#>  [3] "longitude"                     "bearing"                      
#>  [5] "odometer"                      "speed"                        
#>  [7] "trip_id"                       "route_id"                     
#>  [9] "direction_id"                  "start_time"                   
#> [11] "start_date"                    "schedule_relationship"        
#> [13] "stop_id"                       "current_stop_sequence"        
#> [15] "current_status"                "timestamp"                    
#> [17] "congestion_level"              "occupancy_status"             
#> [19] "occupancy_percentage"          "vehicle_id"                   
#> [21] "vehicle_label"                 "vehicle_license_plate"        
#> [23] "vehicle_wheelchair_accessible" "file_timestamp"               
#> [25] "file_index"
```

Every column of that data frame is either consumed by `gps2gtfs`
(`vehicle_id`, `latitude`, `longitude`, `timestamp`, `speed`) or passed
through untouched (`trip_id`, `route_id`, `stop_id`, `current_status`,
`bearing`, …). Pass the timezone the agency operates in:
[`g2g_clean_gps()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_clean_gps.md)
preserves it, so service days split at local midnight.

## A worked example

A single feed snapshot contains one ping per vehicle — a trajectory
needs an *archive* of polls. For a reproducible example we simulate what
a day-long archive of one bus on a two-terminal route looks like after
parsing: two runs (A → B, then B → A) with GTFS-RT `trip_id`
annotations, plus one unannotated layover ping.

``` r

lat <- seq(7.290, 7.320, length.out = 4)
lon <- seq(80.630, 80.660, length.out = 4)
base <- as.POSIXct("2026-06-06 08:00:00", tz = "UTC")

positions <- data.frame(
  vehicle_id = "7482",
  latitude = c(lat, 7.321, rev(lat)),
  longitude = c(lon, 80.661, rev(lon)),
  timestamp = base + c(0, 300, 600, 900, 1200, 1800, 2100, 2400, 2700),
  speed = c(0, 20, 20, 0, 0, 0, 20, 20, 0),
  trip_id = c(rep("CS_1", 4), NA, rep("CS_2", 4))
)
```

## Terminals and stops from a baseline feed

The pipeline needs terminal and stop locations. With a planned
(baseline) static GTFS feed for the same system — a
gtfsio/gtfstools-style object or a path to a GTFS zip — both are derived
automatically:

``` r

baseline <- list(
  trips = data.frame(
    trip_id = c("CS_1", "CS_2"),
    route_id = "R1",
    service_id = "wk"
  ),
  stop_times = data.frame(
    trip_id = rep(c("CS_1", "CS_2"), each = 4),
    stop_id = c("A", "S1", "S2", "B", "B", "S2", "S1", "A"),
    stop_sequence = rep(1:4, 2)
  ),
  stops = data.frame(
    stop_id = c("A", "S1", "S2", "B"),
    stop_name = c("Terminal A", "Stop 1", "Stop 2", "Terminal B"),
    stop_lat = lat,
    stop_lon = lon
  )
)

terminals <- g2g_terminals_from_gtfs(baseline, route_id = "R1")
stops <- g2g_stops_from_gtfs(baseline, route_id = "R1")
terminals
#>    terminal_id latitude longitude
#>         <char>    <num>     <num>
#> 1:           A     7.29     80.63
#> 2:           B     7.32     80.66
stops
#> Key: <direction, stop_id>
#>    stop_id latitude longitude direction
#>     <char>    <num>     <num>    <char>
#> 1:      S1     7.30     80.64         A
#> 2:      S2     7.31     80.65         A
#> 3:      S1     7.30     80.64         B
#> 4:      S2     7.31     80.65         B
```

### Inputs without a GTFS

A baseline feed is one of three ways to get the two tables. The two
builder pairs — from a GTFS feed and from route geometry — each return
both tables and label stop directions with terminal IDs, so neither
needs a `stop_direction_map`.
[`g2g_stops_from_positions()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_positions.md)
is narrower: it estimates stop coordinates only, and the `direction`
labels have to come from elsewhere.

- **From a GTFS feed** —
  [`g2g_terminals_from_gtfs()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_gtfs.md)
  and
  [`g2g_stops_from_gtfs()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_gtfs.md),
  above.
- **From the positions themselves** — when pings carry `stop_id`
  annotations,
  [`g2g_stops_from_positions()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_positions.md)
  estimates each stop’s coordinates from the pings observed at it. It
  fills the coordinate columns only; the `direction` label still has to
  come from somewhere else.
- **From supplied route geometry** —
  [`g2g_terminals_from_geometries()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_geometries.md)
  and
  [`g2g_stops_from_geometries()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_geometries.md)
  take the route’s own linestrings (a long
  `route_id`/`direction`/`vertex_seq`/`latitude`/`longitude` table, or
  an `sf` object) plus a plain stop list. Each direction’s terminal is
  the first vertex of its line, and a stop joins a direction when it
  lies within `buffer_m` of that direction’s polyline — measured to the
  segments, so a stop midway along a long segment is not missed.

Failing all three, supply the two tables by hand.

## Extracting trips and stop times

Because the positions carry `trip_id` annotations, we can skip spatial
trip inference entirely (`trip_col = "trip_id"`, the *fast path*).
Without the annotation, drop that argument and trips are inferred from
terminal-buffer crossings instead — same output either way.

``` r

result <- g2g_extract_trips_and_stop_times(
  gps_data = positions,
  terminals_data = terminals,
  stops_data = stops,
  terminals_buffer_radius = 100,
  stops_buffer_radius = 100,
  stops_extended_buffer_radius = 150,
  trip_col = "trip_id",
  return_trajectory = TRUE
)
#> Starting Pipeline for extracting Trip and Bus Stop Data using backend: rust
#> [INFO] Auto-detected local UTM projection EPSG:32644 (Zone 44N)
#> [INFO] Using supplied trip identities from column(s) 'trip_id' (fast path).
#> Pipeline finished successfully!
#> Warning: gps2gtfs extraction lost coverage: kept 2 trips from 9 input pings.
#> Dropped rows_dropped_no_trip_identity (1), pings_dropped_not_in_trip (1).
#> Inspect the full coverage table with g2g_diagnostics(result) (also attr(result,
#> "diagnostics")). Silence with diagnostics_warn = FALSE or
#> options(gps2gtfs.diagnostics_warn = FALSE).

result$trips
#>    trip_id vehicle_id       date start_terminal end_terminal direction
#>      <int>     <char>     <char>         <char>       <char>     <int>
#> 1:       1       7482 2026-06-06              A            B         1
#> 2:       2       7482 2026-06-06              B            A         2
#>             start_time            end_time provided_trip_id duration_in_mins
#>                 <POSc>              <POSc>           <char>            <num>
#> 1: 2026-06-06 08:00:00 2026-06-06 08:15:00             CS_1               15
#> 2: 2026-06-06 08:30:00 2026-06-06 08:45:00             CS_2               15
#>    day_of_week hour_of_day is_weekday orientation_id orientation_status
#>          <ord>       <int>     <lgcl>          <int>             <fctr>
#> 1:    Saturday           8      FALSE             NA               none
#> 2:    Saturday           8      FALSE             NA               none
#>    orientation_confidence pattern_ref start_anchor_ref end_anchor_ref
#>                     <num>      <char>           <char>         <char>
#> 1:                     NA        <NA>             <NA>           <NA>
#> 2:                     NA        <NA>             <NA>           <NA>
result$stop_times
#>    trip_id vehicle_id       date direction stop_id        arrival_time
#>      <int>     <char>     <char>     <int>  <char>              <POSc>
#> 1:       1       7482 2026-06-06         1      S1 2026-06-06 08:05:00
#> 2:       1       7482 2026-06-06         1      S2 2026-06-06 08:10:00
#> 3:       2       7482 2026-06-06         2      S2 2026-06-06 08:35:00
#> 4:       2       7482 2026-06-06         2      S1 2026-06-06 08:40:00
#>         departure_time dwell_time_in_seconds day_of_week hour_of_day is_weekday
#>                 <POSc>                 <num>       <ord>       <int>     <lgcl>
#> 1: 2026-06-06 08:05:00                     0    Saturday           8      FALSE
#> 2: 2026-06-06 08:10:00                     0    Saturday           8      FALSE
#> 3: 2026-06-06 08:35:00                     0    Saturday           8      FALSE
#> 4: 2026-06-06 08:40:00                     0    Saturday           8      FALSE
#>    provided_trip_id orientation_id orientation_status orientation_confidence
#>              <char>          <int>             <fctr>                  <num>
#> 1:             CS_1             NA               none                     NA
#> 2:             CS_1             NA               none                     NA
#> 3:             CS_2             NA               none                     NA
#> 4:             CS_2             NA               none                     NA
#>    pattern_ref
#>         <char>
#> 1:        <NA>
#> 2:        <NA>
#> 3:        <NA>
#> 4:        <NA>
```

These are *inference tables*, not finished GTFS files: GTFS-style names,
but no `route_id`, `service_id`, or `stop_sequence`, and times are
absolute `POSIXct` values (input timezone) rather than GTFS clock
strings — so trips running past midnight stay unambiguous.

## From inference tables to a valid GTFS feed

`gps2gtfs` stops at coordinate inference on purpose: `route_id`,
`service_id`, and canonical stop identities cannot be inferred from GPS
alone, so the package does not invent them. Turning the inference tables
into a standard-compliant feed (deriving `stop_sequence`, attributing
each trip to a service day, encoding `>24:00:00` clock strings, and
linking or synthesizing IDs) is the job of the companion package
[`gtfsrt2static`](https://e-kotov.github.io/gtfsrt2static/), a separate,
optional install. The handoff is three calls:

``` r

# install.packages("pak"); pak::pak("e-kotov/gtfsrt2static")
library(gtfsrt2static)

# The fast path keeps the GTFS-RT trip_id as `provided_trip_id`, which
# becomes the event's trip identity, so official trip IDs survive.
events <- rt2s_events_from_stop_times(
  result$stop_times,
  trip_id_col = "provided_trip_id"
)

# Baseline mode: inherit official route, service and stop IDs from the
# complete planned feed (a gtfsio object or a zip path), with the observed
# times in stop_times.txt.
feed <- rt2s_assemble(events, baseline = "planned_gtfs.zip")

# ...or, with no planned feed, scaffold a compliant feed from scratch
# (strict = TRUE turns missing agency or stop coordinates into errors):
# feed <- rt2s_scaffold(events, agency = list(...), stops = ..., strict = TRUE)

gtfsio::export_gtfs(feed, "realized_gtfs.zip")
```

The chunk is not evaluated here because `gtfsrt2static` is a separate
package. [From raw GPS to a realized GTFS
feed](https://e-kotov.github.io/gtfsrt2static/articles/pipeline.html)
runs this handoff end to end, in both modes, and reads the result back
with `gtfstools`.

## Shapes: the geometry actually driven

`return_trajectory = TRUE` exposes the ping-level trajectory, which
converts into a `shapes.txt`-shaped table — unlike planned shapes, these
traces record what the vehicle actually drove, including detours:

``` r

shapes <- g2g_shapes_from_trips(result$trajectory)
head(shapes)
#>    shape_id shape_pt_lat shape_pt_lon shape_pt_sequence shape_dist_traveled
#>      <char>        <num>        <num>             <int>               <num>
#> 1:    SHP_1         7.29        80.63                 1               0.000
#> 2:    SHP_1         7.30        80.64                 2            1566.182
#> 3:    SHP_1         7.31        80.65                 3            3132.347
#> 4:    SHP_1         7.32        80.66                 4            4698.495
#> 5:    SHP_2         7.32        80.66                 1               0.000
#> 6:    SHP_2         7.31        80.65                 2            1566.147
```

## Notes for real archives

- [`g2g_clean_gps()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_clean_gps.md)
  (called internally by the pipeline) deduplicates repeated
  `(vehicle_id, timestamp)` observations — archived feeds re-report
  unchanged positions every poll — and drops pings with missing
  `vehicle_id` with a warning.
- GTFS-RT reports `speed` in meters per second; only `speed == 0` is
  used (dwell-time detection). A missing speed column degrades dwell
  estimates and triggers a warning.
- Data in any other column naming (AVL exports, logger CSVs) is mapped
  with `vehicle_col =` / `time_col =` — no pre-processing step needed.
