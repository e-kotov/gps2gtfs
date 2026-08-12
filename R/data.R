#' Sample bus GPS trajectories
#'
#' A lightweight subset of raw bus GPS pings from Kandy, Sri Lanka, intended for
#' examples and testing of the \code{gps2gtfs} pipeline. It is the \code{gps_data}
#' input, and works together with \code{\link{g2g_data_stops}} and
#' \code{\link{g2g_data_terminals}}, which describe the same service.
#'
#' @format A data frame with 2167 rows and 6 variables:
#' \describe{
#'   \item{id}{Unique identifier for the GPS record.}
#'   \item{vehicle_id}{Unique identifier for the tracking vehicle (bus). Column
#'     naming follows the GTFS-Realtime convention.}
#'   \item{timestamp}{Timestamp of the GPS ping (UTC).}
#'   \item{latitude}{Latitude in WGS-84 degrees.}
#'   \item{longitude}{Longitude in WGS-84 degrees.}
#'   \item{speed}{Recorded speed of the vehicle.}
#' }
#' @source
#' Original data from the Python \code{gps2gtfs} package.
#' @references
#' Ratneswaran, S., & Thayasivam, U. (2023). Extracting potential Travel time information from raw GPS data and Evaluating the Performance of Public transit - a case study in Kandy, Sri Lanka. \emph{2023 3rd International Conference on Intelligent Communication and Computational Techniques (ICCT)}, 1-7. \doi{10.1109/ICCT56969.2023.10075789}
"g2g_data_gps"

#' Sample bus stops
#'
#' Coordinates and metadata for the bus stops of the same Kandy, Sri Lanka
#' service as \code{\link{g2g_data_gps}}. It is the \code{stops_data} input, and
#' works together with that dataset and \code{\link{g2g_data_terminals}} to
#' extract stop times.
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

#' Sample bus terminals
#'
#' Coordinates for the two route terminals of the same Kandy, Sri Lanka service
#' as \code{\link{g2g_data_gps}}. It is the \code{terminals_data} input, and
#' works together with that dataset and \code{\link{g2g_data_stops}}: the
#' terminal buffers are what bound the start and end of each trip under
#' \code{segmentation = "terminals"}.
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
