# Extraction coverage diagnostics

Retrieve the coverage diagnostics attached to the return value of
[`g2g_extract_trips`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips.md)
or
[`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md).
The diagnostics record how many GPS pings, trip segments, and trips
survived each stage of extraction, so a non-random loss of coverage (a
feed whose vehicle positions carry no usable trip identity, routes the
two-terminal model cannot segment, stops out of buffer range) is visible
as data instead of vanishing silently.

## Usage

``` r
g2g_diagnostics(x)
```

## Arguments

- x:

  The value returned by
  [`g2g_extract_trips_and_stop_times`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md)
  (a list) or by
  [`g2g_extract_trips`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips.md)
  (a data.table). Diagnostics are attached to that top-level object, not
  to the individual `trips`/`stop_times` tables inside the list.

## Value

A `g2g_diagnostics` data.table with columns `stage`, `metric`, and `n`
(integer count; `NA` where a metric does not apply to the run). It has a
compact `print` method; treat it as a normal data.table for programmatic
use.

## Details

The extractors also emit a one-line
[`warning()`](https://rdrr.io/r/base/warning.html) whenever a run loses
coverage worth surfacing, pointing back here — so unattended or
agent-driven pipelines notice the loss without having to inspect
attributes. Duplicate removal is treated as routine and never triggers
that warning (it still appears in the table). Suppress the warning with
`diagnostics_warn = FALSE` or
`options(gps2gtfs.diagnostics_warn = FALSE)`.

## Examples

``` r
# \donttest{
data(g2g_data_gps)
data(g2g_data_terminals)
data(g2g_data_stops)
res <- g2g_extract_trips_and_stop_times(
  gps_data = g2g_data_gps,
  terminals_data = g2g_data_terminals,
  stops_data = g2g_data_stops,
  terminals_buffer_radius = 50,
  stops_buffer_radius = 30,
  stops_extended_buffer_radius = 50
)
#> Starting Pipeline for extracting Trip and Bus Stop Data using backend: rust
#> [INFO] Removed 56 duplicated (vehicle_id, timestamp) row(s).
#> Error: 'stops_df' labels its direction groups with values that are not terminal IDs ("Kandy-Digana", "Digana-Kandy"), and 'stop_direction_map' was not supplied. Which label belongs to which terminal is not recoverable from the data - both groups span the same corridor - and getting it backwards silently reverses every direction in the output. Supply one of:
#>   stop_direction_map = c("Kandy-Digana" = "BT01", "Digana-Kandy" = "BT02")
#>   stop_direction_map = c("Kandy-Digana" = "BT02", "Digana-Kandy" = "BT01")
#> Each label maps to the terminal that trips in that direction start from.
g2g_diagnostics(res)
#> Error: object 'res' not found
# }
```
