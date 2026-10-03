# Build shapes.txt-shaped traces from a pipeline trajectory

Converts the ping-level trajectory of extracted trips into a table
shaped like the GTFS `shapes.txt` file: one shape per trip, points in
travel order, with cumulative distance. Unlike planned shapes, these
traces record the geometry *actually driven*, including detours.

## Usage

``` r
g2g_shapes_from_trips(trajectory, shape_id_prefix = "SHP_")
```

## Arguments

- trajectory:

  A data.table of trajectory GPS records with columns `trip_id`,
  `latitude`, `longitude`, `date`, `time_str` (as returned in the
  pipeline's `$trajectory`).

- shape_id_prefix:

  Character prefix for generated shape ids. Default `"SHP_"` (shape ids
  become `SHP_<trip_id>`).

## Value

A data.table with GTFS-standard columns `shape_id`, `shape_pt_lat`,
`shape_pt_lon`, `shape_pt_sequence`, `shape_dist_traveled` (meters).

## Details

Obtain the trajectory by running
`g2g_extract_trips_and_stop_times(..., return_trajectory = TRUE)` and
using the `$trajectory` element of the result.
