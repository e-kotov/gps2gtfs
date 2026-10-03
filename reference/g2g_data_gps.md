# Sample bus GPS trajectories

A lightweight subset of raw bus GPS pings from Kandy, Sri Lanka,
intended for examples and testing of the `gps2gtfs` pipeline. It is the
`gps_data` input, and works together with
[`g2g_data_stops`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_stops.md)
and
[`g2g_data_terminals`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_terminals.md),
which describe the same service.

## Usage

``` r
g2g_data_gps
```

## Format

A data frame with 2167 rows and 6 variables:

- id:

  Unique identifier for the GPS record.

- vehicle_id:

  Unique identifier for the tracking vehicle (bus). Column naming
  follows the GTFS-Realtime convention.

- timestamp:

  Timestamp of the GPS ping (UTC).

- latitude:

  Latitude in WGS-84 degrees.

- longitude:

  Longitude in WGS-84 degrees.

- speed:

  Recorded speed of the vehicle.

## Source

Original data from the Python `gps2gtfs` package.

## References

Ratneswaran, S., & Thayasivam, U. (2023). Extracting potential Travel
time information from raw GPS data and Evaluating the Performance of
Public transit - a case study in Kandy, Sri Lanka. *2023 3rd
International Conference on Intelligent Communication and Computational
Techniques (ICCT)*, 1-7.
[doi:10.1109/ICCT56969.2023.10075789](https://doi.org/10.1109/ICCT56969.2023.10075789)
