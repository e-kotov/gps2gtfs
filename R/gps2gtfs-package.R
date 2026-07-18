#' @useDynLib gps2gtfs, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".",
    ".I",
    ".N",
    ":=",
    "latitude",
    "longitude",
    "timestamp",
    "time_str",
    "direction",
    "bus_stop",
    "stop_id",
    "id",
    "i.stop_id",
    "grouped_ends",
    "vehicle_id",
    "trip_id",
    "speed",
    "day_of_week",
    "hour_of_day",
    "arrival_time",
    "is_weekday",
    "duration_in_mins",
    "terminal_id",
    "grouped_terminals",
    "entry_exit",
    "wrap__assign_trip_ids_rust",
    "wrap__match_points_to_buffers_rust",
    "wrap__propagate_trip_ids_rust",
    "_gps2gtfs_assign_trip_ids_cpp",
    "_gps2gtfs_haversine_distance_cpp",
    "_gps2gtfs_is_rust_compiled_cpp",
    "_gps2gtfs_match_points_to_buffers_cpp",
    "_gps2gtfs_propagate_trip_ids_cpp"
  ))
}
