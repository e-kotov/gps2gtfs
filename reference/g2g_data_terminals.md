# Sample bus terminals

Coordinates for the two route terminals of the same Kandy, Sri Lanka
service as
[`g2g_data_gps`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_gps.md).
It is the `terminals_data` input, and works together with that dataset
and
[`g2g_data_stops`](https://e-kotov.github.io/gps2gtfs/reference/g2g_data_stops.md):
the terminal buffers are what bound the start and end of each trip under
`segmentation = "terminals"`.

## Usage

``` r
g2g_data_terminals
```

## Format

A data frame with 2 rows and 4 variables:

- terminal_id:

  Unique identifier for the bus terminal.

- terminal_name:

  Name of the terminal.

- latitude:

  Latitude in WGS-84 degrees.

- longitude:

  Longitude in WGS-84 degrees.

## Source

Original data from the Python `gps2gtfs` package.

## References

Ratneswaran, S., & Thayasivam, U. (2023). Extracting potential Travel
time information from raw GPS data and Evaluating the Performance of
Public transit - a case study in Kandy, Sri Lanka. *2023 3rd
International Conference on Intelligent Communication and Computational
Techniques (ICCT)*, 1-7.
[doi:10.1109/ICCT56969.2023.10075789](https://doi.org/10.1109/ICCT56969.2023.10075789)
