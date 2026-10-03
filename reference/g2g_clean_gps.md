# Clean raw GPS data

Cleans raw GPS or GTFS-Realtime vehicle position data: maps input
columns to the canonical schema, removes records with zero coordinates
or missing vehicle identifiers, deduplicates repeated pings, parses
timestamps, and sorts by vehicle, date, and time.

## Usage

``` r
g2g_clean_gps(
  raw_gps_df,
  projected = NULL,
  vehicle_col = "vehicle_id",
  time_col = "timestamp",
  tz = NULL,
  dedupe = TRUE,
  drop_missing_vehicle = TRUE
)
```

## Arguments

- raw_gps_df:

  A data.frame containing raw GPS data. Must include `latitude` and
  `longitude` (WGS-84 degrees unless `projected = TRUE`), a vehicle
  identifier column, and a timestamp column. A `speed` column is
  recommended (used for dwell-time estimation); GTFS-Realtime reports it
  in meters per second, but only `speed == 0` is interpreted. An `id`
  row identifier is generated when absent or not unique.

- projected:

  Logical. Is the coordinates data already projected? Default is NULL
  (auto-detect).

- vehicle_col:

  Character. Name of the column holding the vehicle identifier. Default
  `"vehicle_id"` (GTFS-Realtime convention).

- time_col:

  Character. Name of the column holding the observation time. Default
  `"timestamp"` (GTFS-Realtime convention). `POSIXct` input keeps its
  own timezone (service days split at feed-local midnight) and `tz` is
  ignored for it. Character/numeric input is parsed in `tz`.

- tz:

  Character. Timezone in which non-`POSIXct` timestamps are interpreted,
  and therefore the timezone whose midnight bounds service days. Because
  service-day attribution (and every downstream GTFS clock string)
  depends on it, there is no safe default for character input: pass the
  feed's operating timezone (e.g. `"America/New_York"`), or pass
  already-localized `POSIXct` timestamps. `NULL` (default) parses
  character input as UTC *with a warning*; set `tz = "UTC"` explicitly
  to silence it when UTC is truly intended.

- dedupe:

  Logical. Drop repeated `(vehicle_id, timestamp)` rows, as produced by
  archived GTFS-Realtime feeds that re-report unchanged positions.
  Default `TRUE`.

- drop_missing_vehicle:

  Logical. Drop rows with missing/empty vehicle identifiers with a
  warning (`TRUE`, default) instead of failing.

## Value

A sorted `data.table` with canonical columns `id`, `vehicle_id`,
`latitude`, `longitude`, `timestamp`, `speed`, additional `date` and
`time_str` columns, and all other input columns passed through.

## Details

The canonical column names follow the GTFS-Realtime convention, so the
output of
[`gtfsrealtime::read_gtfsrt_positions()`](https://projects.indicatrix.org/gtfsrealtime-r/reference/read_gtfsrt_positions.html)
is accepted directly. Any other source (AVL exports, logger CSVs) can be
mapped via `vehicle_col` and `time_col`. Columns beyond the canonical
set (e.g. `trip_id`, `route_id`, `stop_id`) are passed through
untouched.

## Examples

``` r
# \donttest{
data(g2g_data_gps)
cleaned_gps <- g2g_clean_gps(g2g_data_gps)
#> [INFO] Removed 56 duplicated (vehicle_id, timestamp) row(s).
head(cleaned_gps)
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
# }
```
