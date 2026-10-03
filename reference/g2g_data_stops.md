# Sample bus stops

Coordinates and metadata for the bus stops of the same Kandy, Sri Lanka
service as
[`g2g_data_gps`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_gps.md).
It is the `stops_data` input, and works together with that dataset and
[`g2g_data_terminals`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_terminals.md)
to extract stop times.

## Usage

``` r
g2g_data_stops
```

## Format

A data frame with 23 rows and 6 variables:

- stop_id:

  Unique identifier for the bus stop.

- route_id:

  Route identifier the stop belongs to.

- direction:

  Direction of the route the stop serves.

- latitude:

  Latitude in WGS-84 degrees.

- longitude:

  Longitude in WGS-84 degrees.

- address:

  Address or name of the stop location.

## Source

Original data from the Python `gps2gtfs` package.

## References

Ratneswaran, S., & Thayasivam, U. (2023). Extracting potential Travel
time information from raw GPS data and Evaluating the Performance of
Public transit - a case study in Kandy, Sri Lanka. *2023 3rd
International Conference on Intelligent Communication and Computational
Techniques (ICCT)*, 1-7.
[doi:10.1109/ICCT56969.2023.10075789](https://doi.org/10.1109/ICCT56969.2023.10075789)
