# Package index

## Clean GPS

Normalise raw pings: canonical column names, timezone, deduplication.

- [`g2g_clean_gps()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_clean_gps.md)
  : Clean raw GPS data

## Stops and terminals

Build the stop and terminal tables the extractor needs, from a planned
GTFS feed, from route geometry, or from annotated Vehicle Positions.

- [`g2g_stops_from_geometries()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_geometries.md)
  : Derive a stops table from supplied route geometries
- [`g2g_stops_from_gtfs()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_gtfs.md)
  : Derive a stops table from a static GTFS feed
- [`g2g_stops_from_positions()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_stops_from_positions.md)
  : Estimate stop coordinates from vehicle positions
- [`g2g_terminals_from_geometries()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_geometries.md)
  : Derive trip terminals from supplied route geometries
- [`g2g_terminals_from_gtfs()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_terminals_from_gtfs.md)
  : Derive trip terminals from a static GTFS feed

## Trips and stop times

Cut trajectories into trips, detect stop visits, and inspect what was
kept and dropped at each stage.

- [`g2g_extract_trips()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips.md)
  : Extract trips from GPS trajectories
- [`g2g_extract_trips_and_stop_times()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_extract_trips_and_stop_times.md)
  : Extract trips and stop times from GPS trajectories
- [`g2g_diagnostics()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_diagnostics.md)
  : Extraction coverage diagnostics

## Shapes

- [`g2g_shapes_from_trips()`](https://e-kotov.github.io/gps2gtfs/reference/g2g_shapes_from_trips.md)
  : Build shapes.txt-shaped traces from a pipeline trajectory

## Example data

One bus on route 654 (Kandy to Digana, Sri Lanka) for one day, with the
route’s stops and terminals; from the original Python gps2gtfs.

- [`g2g_data_gps`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_gps.md)
  : Sample bus GPS trajectories
- [`g2g_data_stops`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_stops.md)
  : Sample bus stops
- [`g2g_data_terminals`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_terminals.md)
  : Sample bus terminals
