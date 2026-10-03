# gps2gtfs

Website: <https://e-kotov.github.io/gps2gtfs/>

`gps2gtfs` turns raw GPS pings from public transit vehicles into
GTFS-shaped `trips` and `stop_times` tables. It cleans the pings, cuts
each vehicle’s trajectory into trips between terminals (or at layovers,
or by a trip annotation that the pings already carry), detects stop
visits with arrival, departure and dwell times, and traces the geometry
each trip actually drove. It is a high-performance R port of the Python
[gps2gtfs](https://github.com/aaivu/gps2gtfs) package.

## Where it fits

    GTFS-RT Vehicle Positions ──(gtfsrealtime)──┐
                                                ├─► gps2gtfs ─► trips, stop_times, shapes
    raw AVL / logger GPS ───────────────────────┘                     │
                                                                      ▼
                                       gtfsrt2static ─► realized GTFS zip ─► gtfsio / gtfstools / tidytransit

- [`gtfsrealtime`](https://cran.r-project.org/package=gtfsrealtime)
  parses GTFS-Realtime protobuf. Its Vehicle Positions output uses the
  column names `gps2gtfs` expects (`vehicle_id`, `latitude`,
  `longitude`, `timestamp`, `speed`), so it feeds the pipeline with no
  adapter.
- **This package** does the spatial inference: trips, stop visits and
  driven shapes from coordinates.
- [`gtfsrt2static`](https://github.com/e-kotov/gtfsrt2static) assembles
  the result into a standard-compliant static GTFS feed, either
  inheriting the identifiers of a planned feed or scaffolding one from
  scratch.

## Installation

``` r

# install.packages("pak")
pak::pak("e-kotov/gps2gtfs")
```

A Rust toolchain is optional. When `cargo` and `rustc` are found at
install time, the heavy steps are compiled in Rust; otherwise a C++
(Rcpp) backend is built. `backend = "auto"` prefers Rust, then Rcpp,
then a pure R implementation that uses `data.table` and `sf`. All three
return identical tables.

## Example

The package ships one day of GPS pings from a bus on route 654 between
Kandy and Digana, Sri Lanka, with the route’s stops and its two
terminals:

``` r

library(gps2gtfs)

result <- g2g_extract_trips_and_stop_times(
  gps_data = g2g_data_gps,
  terminals_data = g2g_data_terminals,
  stops_data = g2g_data_stops,
  terminals_buffer_radius = 50,
  stops_buffer_radius = 30,
  stops_extended_buffer_radius = 50,
  # each stop-direction label maps to the terminal its trips start from
  stop_direction_map = c("Kandy-Digana" = "BT01", "Digana-Kandy" = "BT02"),
  return_trajectory = TRUE
)

result$trips          # one row per trip: terminals, direction, start/end time
result$stop_times     # one row per stop visit: arrival, departure, dwell
g2g_diagnostics(result)                       # what was kept and dropped, by stage
shapes <- g2g_shapes_from_trips(result$trajectory)  # shapes.txt-shaped table
```

All times are absolute `POSIXct` values, never clock strings, so a trip
that runs past midnight stays unambiguous.

## Learn more

- [Get
  started](https://e-kotov.github.io/gps2gtfs/articles/gps2gtfs.html):
  the whole pipeline on the bundled data, step by step.
- [GTFS-Realtime Vehicle Positions to
  GTFS](https://e-kotov.github.io/gps2gtfs/articles/gtfs-realtime.html):
  reading archived feeds with `gtfsrealtime`, deriving stops and
  terminals from a planned feed, and the trip-annotation fast path.
- [From raw GPS to a realized GTFS
  feed](https://e-kotov.github.io/gtfsrt2static/articles/pipeline.html):
  the end-to-end walkthrough across `gtfsrealtime`, `gps2gtfs`,
  `gtfsrt2static` and `gtfstools`, on the `gtfsrt2static` website.
- [Function
  reference](https://e-kotov.github.io/gps2gtfs/reference/index.html).

## Credits

`gps2gtfs` ports the method and the example data of the Python
[gps2gtfs](https://github.com/aaivu/gps2gtfs) package by Aaivu. The
method is described in Ratneswaran et al. (2023,
[doi:10.1109/ICCT56969.2023.10075789](https://doi.org/10.1109/ICCT56969.2023.10075789))
and the software in Ratneswaran et al. (2025,
[doi:10.1016/j.simpa.2025.100780](https://doi.org/10.1016/j.simpa.2025.100780)).

## License

MIT © Egor Kotov, Aaivu
