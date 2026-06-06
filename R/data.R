#' GPS Trajectory Subset
#'
#' A lightweight subset of raw bus GPS data from Kandy, Sri Lanka.
#' This dataset is intended for examples and testing of the \code{gps2gtfs} pipeline.
#'
#' @format A data frame with 1045 rows and 6 variables:
#' \describe{
#'   \item{id}{Unique identifier for the GPS record.}
#'   \item{deviceid}{Unique identifier for the tracking device (bus).}
#'   \item{devicetime}{Timestamp of the GPS ping (UTC).}
#'   \item{latitude}{Latitude in WGS-84 degrees.}
#'   \item{longitude}{Longitude in WGS-84 degrees.}
#'   \item{speed}{Recorded speed of the vehicle.}
#' }
#' @source
#' Original data from the Python \code{gps2gtfs} package.
#' @references
#' Ratneswaran, S., & Thayasivam, U. (2023). Extracting potential Travel time information from raw GPS data and Evaluating the Performance of Public transit - a case study in Kandy, Sri Lanka. \emph{2023 3rd International Conference on Intelligent Communication and Computational Techniques (ICCT)}, 1-7. \doi{10.1109/ICCT56969.2023.10075789}
"g2g_data_gps"

#' Bus Stops Data
#'
#' Sample data containing the coordinates and metadata for bus stops.
#' This dataset works together with \code{g2g_data_gps} to extract stop times.
#'
#' @format A data frame with 23 rows and 6 variables:
#' \describe{
#'   \item{stop_id}{Unique identifier for the bus stop.}
#'   \item{route_id}{Route identifier the stop belongs to.}
#'   \item{direction}{Direction of the route the stop serves.}
#'   \item{latitude}{Latitude in WGS-84 degrees.}
#'   \item{longitude}{Longitude in WGS-84 degrees.}
#'   \item{address}{Address or name of the stop location.}
#' }
#' @source
#' Original data from the Python \code{gps2gtfs} package.
#' @references
#' Ratneswaran, S., & Thayasivam, U. (2023). Extracting potential Travel time information from raw GPS data and Evaluating the Performance of Public transit - a case study in Kandy, Sri Lanka. \emph{2023 3rd International Conference on Intelligent Communication and Computational Techniques (ICCT)}, 1-7. \doi{10.1109/ICCT56969.2023.10075789}
"g2g_data_stops"

#' Bus Terminals Data
#'
#' Sample data containing the coordinates for bus route terminals.
#' Used to define the start and end of trips.
#'
#' @format A data frame with 2 rows and 4 variables:
#' \describe{
#'   \item{terminal_id}{Unique identifier for the bus terminal.}
#'   \item{terminal_name}{Name of the terminal.}
#'   \item{latitude}{Latitude in WGS-84 degrees.}
#'   \item{longitude}{Longitude in WGS-84 degrees.}
#' }
#' @source
#' Original data from the Python \code{gps2gtfs} package.
#' @references
#' Ratneswaran, S., & Thayasivam, U. (2023). Extracting potential Travel time information from raw GPS data and Evaluating the Performance of Public transit - a case study in Kandy, Sri Lanka. \emph{2023 3rd International Conference on Intelligent Communication and Computational Techniques (ICCT)}, 1-7. \doi{10.1109/ICCT56969.2023.10075789}
"g2g_data_terminals"
