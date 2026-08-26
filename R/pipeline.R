#' Check if Rcpp Backend is Available
#'
#' @return Logical. \code{TRUE} if the Rcpp backend is compiled and loaded, \code{FALSE} otherwise.
#' @noRd
is_rcpp_available <- function() {
  "gps2gtfs" %in% names(getLoadedDLLs())
}

#' Check if Rust Backend is Compiled/Available
#'
#' @return Logical. \code{TRUE} if the Rust backend is compiled and available, \code{FALSE} otherwise.
#' @noRd
is_rust_available <- function() {
  if (is_rcpp_available()) {
    is_rust_compiled_cpp()
  } else {
    FALSE
  }
}

#' Extract trips from GPS trajectories
#'
#' Cuts a stream of vehicle GPS pings into individual trips and returns one row
#' of features per trip: its vehicle, direction, start and end times, duration,
#' and day of week. Takes a data.frame or a path to a CSV, and optionally writes
#' the result to \code{output_path}.
#'
#' Where the cuts fall is the whole problem, and there are three regimes. If the
#' data already carries trip identities (\code{trip_col}, e.g. a GTFS-Realtime
#' \code{trip_id}), segmentation follows them and nothing is inferred. Otherwise
#' the trips are inferred from the trajectory: \code{segmentation = "terminals"}
#' cuts on crossings of two known terminal buffers, and
#' \code{segmentation = "layover"} cuts wherever the vehicle dwells longer than
#' \code{layover_gap}, anywhere on the route, which is what handles short-turns,
#' loops and multi-branch service. \code{session_gap} bounds all three: a vehicle
#' unseen for that long starts a new driving session and no trip spans one, so
#' overnight trips stay intact while overnight parking still separates one day's
#' operations from the next.
#'
#' Inferred boundaries are estimates, and \code{layover_gap} in particular is
#' mode- and feed-dependent: it must exceed the ping interval and the normal
#' in-service dwell, and stay below the shortest real layover. The result carries
#' a coverage table (\code{\link{g2g_diagnostics}}) reporting what the run
#' dropped, which is the way to tell a clean extraction from a plausible-looking
#' one.
#'
#' To get stop times as well, use
#' \code{\link{g2g_extract_trips_and_stop_times}}, which runs this segmentation
#' and then matches the pings of each trip to stop buffers.
#'
#' @param gps_data A data.frame or path to the raw GPS CSV.
#' @param terminals_data A data.frame or path to the terminal coordinates CSV.
#'   Optional (\code{NULL}) with \code{segmentation = "layover"} or with
#'   \code{direction_col}; required for terminal-buffer segmentation.
#' @param terminals_buffer_radius Numeric. Buffer radius for terminals (in
#'   meters). Only consumed by terminal-buffer segmentation; may be omitted
#'   otherwise.
#' @param output_path Character. Optional path to write output trip features as CSV. Default is \code{NULL} (no file written).
#' @param projected_crs Numeric. The EPSG code of a projected coordinate system
#'   used for metric distance calculations. Required when
#'   \code{projected = TRUE}; otherwise a suitable UTM projection is selected
#'   when omitted.
#' @param backend Character. The backend to use: \code{"auto"}, \code{"rust"},
#'   \code{"rcpp"}, or \code{"pure_r"}. Automatic selection prefers Rust, then
#'   Rcpp, then pure R. Explicit unavailable backends produce an error.
#' @param projected Logical. Whether plain-table coordinates are already
#'   projected. Out-of-bounds coordinates require explicit \code{TRUE}.
#' @param vehicle_col Character. Name of the vehicle identifier column in
#'   \code{gps_data}. Default \code{"vehicle_id"} (GTFS-Realtime convention).
#' @param time_col Character. Name of the timestamp column in \code{gps_data}.
#'   Default \code{"timestamp"} (GTFS-Realtime convention).
#' @param tz Character. Timezone in which non-\code{POSIXct} timestamps are
#'   interpreted; also the timezone whose midnight bounds service days and
#'   thus every downstream GTFS clock string. Pass the feed's operating
#'   timezone, or supply localized \code{POSIXct} timestamps. \code{NULL}
#'   (default) parses character input as UTC with a warning. Passed to
#'   \code{\link{g2g_clean_gps}}.
#' @param trip_col Character. Optional name(s) of column(s) holding supplied
#'   trip identities (e.g. \code{"trip_id"} from GTFS-Realtime Vehicle
#'   Positions). When given, trips are segmented by those identities (fast
#'   path) instead of inferred from terminal-buffer crossings. Without
#'   \code{direction_col}, the first/last ping of each segment is matched to
#'   the nearest terminal for direction assignment. May be several columns
#'   that jointly identify a trip - e.g. \code{c("route_id", "direction_id",
#'   "start_date", "start_time")}, the GTFS-Realtime TripDescriptor, for feeds
#'   whose positions carry no \code{trip_id}; they are combined into one
#'   identity. Default \code{NULL} (spatial inference).
#' @param direction_col Character. Optional column giving each ping's travel
#'   direction (e.g. GTFS-Realtime \code{"direction_id"}). Only used with
#'   \code{trip_col}. When supplied, trip direction is taken from the data
#'   rather than inferred from which of two terminals a trip started at, so
#'   \code{terminals_data} becomes optional and routes that are not simple
#'   two-terminal lines (short-turns, variants, one-way services) are handled.
#'   For stop-time extraction, stops are grouped for matching by their
#'   \code{direction} label, which must use the same values as
#'   \code{direction_col}. By default this column labels a trip's direction; it
#'   does not cut trips. A trip has exactly one direction, so if
#'   \code{direction_col} takes more than one value inside a segment that
#'   \code{trip_col} declared to be one trip, the two inputs contradict each
#'   other and extraction stops with an error: either add the direction column
#'   to \code{trip_col} to segment by it, set
#'   \code{cut_on_direction_change = TRUE}, or collapse it to one value per
#'   supplied trip identity. Default \code{NULL}.
#' @param cut_on_direction_change Logical. When \code{TRUE}, a change in
#'   \code{direction_col} cuts a trip rather than raising the mid-trip
#'   direction conflict error above. Raw direction fields flip spuriously, so
#'   the change is debounced first: only a run of consecutive identical values
#'   that is both long enough in pings and long enough in seconds counts as a
#'   real direction change, and shorter contrary bursts are absorbed into the
#'   direction around them. Requires \code{direction_col}. Default
#'   \code{FALSE}, which leaves behaviour unchanged.
#' @param direction_debounce_min_pings Integer. Minimum consecutive pings for a
#'   direction run to be treated as a real direction change. Inert unless
#'   \code{cut_on_direction_change = TRUE}. Note this floor only binds on
#'   high-cadence feeds or at small \code{direction_debounce_min_seconds}: at a
#'   15 s ping cadence a run needs about \code{min_seconds / 15 + 1} pings to
#'   span the seconds threshold at all, so from roughly 300 s upwards the
#'   seconds threshold binds first. Default \code{2L}.
#' @param direction_debounce_min_seconds Numeric. Minimum intra-run span, in
#'   seconds, for a direction run to be treated as a real direction change. A
#'   single-ping run spans 0 seconds and is therefore always absorbed. Inert
#'   unless \code{cut_on_direction_change = TRUE}. Default 600.
#' @param session_gap Numeric. Successive observations of a vehicle further
#'   apart than this many seconds start a new driving session; trips never
#'   span sessions. This replaces the former calendar-date boundary, so
#'   overnight trips crossing midnight stay intact while overnight parking
#'   still separates one day's operations from the next. Default 4 hours
#'   (\code{4 * 3600}).
#' @param segmentation Character. How the raw-GPS spatial path cuts trips when
#'   no \code{trip_col} is supplied: \code{"terminals"} (classic two-terminal
#'   buffer model), \code{"layover"} (cut wherever the vehicle dwells longer
#'   than \code{layover_gap}, anywhere on the route - handles short-turn,
#'   loop, and multi-branch services; all trips share a single direction
#'   group), or \code{"auto"} (default: \code{"terminals"} when
#'   \code{terminals_data} is given, \code{"layover"} otherwise). Not used
#'   with \code{trip_col}.
#' @param layover_gap Numeric. Layover segmentation only: dwells longer than
#'   this many seconds bound trips - whether a silent gap between successive
#'   pings or a stationary spell (pings present but staying within
#'   \code{layover_radius}). Mode-dependent: must exceed the feed's ping
#'   interval and normal in-service stop dwell, and stay below the shortest
#'   real layover. Default 10 minutes (\code{10 * 60}).
#' @param layover_radius Numeric. Layover segmentation only: a vehicle counts
#'   as stationary while successive pings stay within this many meters of the
#'   dwell's first ping (anchoring absorbs GPS jitter while parked).
#'   Default 50.
#' @param diagnostics_warn Logical. Emit a one-line \code{warning()} when the
#'   run drops coverage worth surfacing (pings with no usable trip identity,
#'   unmatched segments, stops out of range) so unattended or agent-driven
#'   pipelines notice silent loss. The full breakdown is always available via
#'   \code{\link{g2g_diagnostics}} regardless of this flag. Default
#'   \code{getOption("gps2gtfs.diagnostics_warn", TRUE)}.
#' @return A data.table containing extracted trip features. \code{start_time}
#'   and \code{end_time} are absolute \code{POSIXct} times in the timezone of
#'   the input timestamps. Rows are returned in a stable, backend-invariant
#'   order: sorted by the internal integer \code{trip_id}. The result carries
#'   an \code{attr(., "diagnostics")} coverage table (see
#'   \code{\link{g2g_diagnostics}}). See the "Inference tables, not GTFS files"
#'   section of \code{\link{g2g_extract_trips_and_stop_times}}.
#' @examples
#' \donttest{
#' data(g2g_data_gps)
#' data(g2g_data_terminals)
#' trips <- g2g_extract_trips(
#'   gps_data = g2g_data_gps,
#'   terminals_data = g2g_data_terminals,
#'   terminals_buffer_radius = 50
#' )
#' }
#' @importFrom data.table fread fwrite as.data.table
#' @importFrom stats median
#' @export
g2g_extract_trips <- function(
  gps_data,
  terminals_data = NULL,
  terminals_buffer_radius = NULL,
  output_path = NULL,
  projected_crs = NULL,
  backend = "auto",
  projected = NULL,
  vehicle_col = "vehicle_id",
  time_col = "timestamp",
  tz = NULL,
  trip_col = NULL,
  direction_col = NULL,
  cut_on_direction_change = FALSE,
  direction_debounce_min_pings = 2L,
  direction_debounce_min_seconds = 600,
  session_gap = 4 * 3600,
  segmentation = c("auto", "terminals", "layover"),
  layover_gap = 10 * 60,
  layover_radius = 50,
  diagnostics_warn = getOption("gps2gtfs.diagnostics_warn", TRUE)
) {
  backend <- resolve_backend(backend)
  validate_projected_crs(projected_crs)
  validate_session_gap(session_gap)
  validate_layover_gap(layover_gap)
  validate_layover_radius(layover_radius)
  if (isTRUE(projected) && is.null(projected_crs)) {
    stop("'projected_crs' is required when 'projected = TRUE'.", call. = FALSE)
  }
  if (!is.null(direction_col) && is.null(trip_col)) {
    stop(
      "'direction_col' is only used with 'trip_col'.",
      call. = FALSE
    )
  }
  if (isTRUE(cut_on_direction_change) && is.null(direction_col)) {
    stop(
      "'cut_on_direction_change = TRUE' requires 'direction_col': there is ",
      "no direction series to debounce without it.",
      call. = FALSE
    )
  }
  seg_mode <- resolve_segmentation(segmentation, trip_col, !is.null(terminals_data))
  # The terminal buffer radius is only consumed by terminal-buffer
  # segmentation; other modes may omit it.
  if (seg_mode == "terminals" || !is.null(terminals_buffer_radius)) {
    validate_positive_radius(terminals_buffer_radius, "terminals_buffer_radius")
  }
  if (seg_mode == "layover" && layover_gap >= session_gap) {
    warning(
      "'layover_gap' (",
      layover_gap,
      "s) is not smaller than 'session_gap' (",
      session_gap,
      "s); layover cuts are subsumed by session cuts.",
      call. = FALSE
    )
  }
  message("Starting Pipeline for extracting Trip Data using backend: ", backend)

  raw_gps_df <- if (is.character(gps_data) && length(gps_data) == 1L) data.table::fread(gps_data) else data.table::as.data.table(gps_data)
  if (!is.null(trip_col)) {
    if (!vehicle_col %in% names(raw_gps_df) || all(is.na(raw_gps_df[[vehicle_col]]))) {
      message(
        "[INFO] No usable '", vehicle_col,
        "'; filling a single placeholder (trip identities drive segmentation)."
      )
      raw_gps_df <- data.table::copy(raw_gps_df)
      raw_gps_df[[vehicle_col]] <- "veh_1"
    }
  }
  n_pings_in <- nrow(raw_gps_df)
  cleaned <- g2g_clean_gps(
    raw_gps_df,
    projected = projected,
    vehicle_col = vehicle_col,
    time_col = time_col,
    tz = tz
  )
  clean_drops <- attr(cleaned, "clean_drops")
  projected <- attr(cleaned, "projected")
  if (projected && is.null(projected_crs)) {
    stop(
      "'projected_crs' is required for projected coordinates.",
      call. = FALSE
    )
  }

  have_terminals <- !is.null(terminals_data)
  if (have_terminals) {
    trip_terminals_df <- if (is.character(terminals_data) && length(terminals_data) == 1L) data.table::fread(terminals_data) else data.table::as.data.table(terminals_data)
    terminals <- normalize_coordinates(
      trip_terminals_df,
      projected = projected,
      name = "trip_terminals_df"
    )$dt
    validate_required_columns(terminals, "terminal_id", "trip_terminals_df")
    validate_identifiers(terminals$terminal_id, "trip_terminals_df 'terminal_id'")
  } else {
    if (seg_mode == "fast" && is.null(direction_col)) {
      stop(
        "'terminals_data' is required unless 'direction_col' is supplied.",
        call. = FALSE
      )
    }
    terminals <- data.table::data.table(terminal_id = character(0))
  }
  if (!is.null(direction_col)) {
    validate_required_columns(cleaned, direction_col, "gps_data")
  }

  if (nrow(cleaned) == 0L) {
    result <- set_backend(empty_trip_features(cleaned$vehicle_id), backend)
    diag <- build_diagnostics(c(
      pings_in = as.integer(n_pings_in),
      clean_drops,
      pings_after_cleaning = 0L,
      trips_kept = 0L
    ))
    result <- set_diagnostics(result, diag)
    if (!is.null(output_path)) {
      data.table::fwrite(result, output_path)
    }
    maybe_warn_diagnostics(diag, diagnostics_warn)
    return(result)
  }

  if (is.null(projected_crs) && !projected) {
    med_lon <- stats::median(cleaned$longitude, na.rm = TRUE)
    med_lat <- stats::median(cleaned$latitude, na.rm = TRUE)
    zone <- floor((med_lon + 180) / 6) + 1
    is_northern <- med_lat >= 0
    projected_crs <- if (is_northern) (32600 + zone) else (32700 + zone)
    message(
      "[INFO] Auto-detected local UTM projection EPSG:",
      projected_crs,
      " (Zone ",
      zone,
      if (is_northern) "N" else "S",
      ")"
    )
  }

  if (seg_mode == "fast") {
    message(
      "[INFO] Using supplied trip identities from column(s) '",
      paste(trip_col, collapse = "', '"),
      "' (fast path)."
    )
    trips <- extract_trips_from_ids_r(
      cleaned,
      terminals,
      trip_col,
      projected = projected,
      session_gap = session_gap,
      direction_col = direction_col,
      cut_on_direction_change = cut_on_direction_change,
      direction_debounce_min_pings = direction_debounce_min_pings,
      direction_debounce_min_seconds = direction_debounce_min_seconds
    )
  } else if (seg_mode == "layover") {
    trips <- extract_trips_layover_r(
      cleaned,
      terminals,
      layover_gap = layover_gap,
      layover_radius = layover_radius,
      session_gap = session_gap,
      projected = projected
    )
  } else {
    trips <- extract_trips_r(
      cleaned,
      terminals,
      terminals_buffer_radius,
      projected_crs = projected_crs,
      backend = backend,
      projected = projected,
      session_gap = session_gap
    )
  }
  direction_levels <- if (seg_mode == "layover") {
    layover_direction_level
  } else if (!is.null(direction_col)) {
    sort(unique(as.character(stats::na.omit(cleaned[[direction_col]]))))
  } else {
    NULL
  }
  seg_drops <- attr(trips, "seg_drops")
  trip_features <- extract_trip_features_r(
    trips,
    unique(as.character(terminals$terminal_id)),
    direction_levels = direction_levels
  )
  trip_features <- set_backend(trip_features, backend)

  # Stable, backend-invariant row order (documented return contract).
  data.table::setorder(trip_features, trip_id)

  # Coverage diagnostics. Stop-time metrics are NA here: this entry point
  # never matches stops.
  diag <- build_diagnostics(c(
    pings_in = as.integer(n_pings_in),
    clean_drops,
    pings_after_cleaning = nrow(cleaned),
    seg_drops,
    trips_kept = nrow(trip_features),
    max_trip_duration_mins = max_trip_duration_metric(trip_features)
  ))
  trip_features <- set_diagnostics(trip_features, diag)

  if (!is.null(output_path)) {
    data.table::fwrite(trip_features, output_path)
    message("Pipeline finished successfully! Output saved to ", output_path)
  } else {
    message("Pipeline finished successfully!")
  }
  maybe_warn_diagnostics(diag, diagnostics_warn)
  trip_features
}

#' Extract trips and stop times from GPS trajectories
#'
#' The package's main entry point. Cuts a stream of vehicle GPS pings into
#' individual trips, then matches each trip's pings to stop buffers, returning
#' both a trip-feature table and a stop-time table with arrival, departure and
#' dwell per stop. Each of \code{gps_data}, \code{terminals_data} and
#' \code{stops_data} may be a data.frame or a path to a CSV, and either table can
#' optionally be written out.
#'
#' Segmentation works exactly as in \code{\link{g2g_extract_trips}} - supplied
#' \code{trip_col} identities, terminal buffers, or layover dwells, all bounded
#' by \code{session_gap} - and this function adds the stop-matching stage on top.
#' A ping is attributed to a stop when it falls inside
#' \code{stops_buffer_radius}, with \code{stops_extended_buffer_radius} as the
#' wider fallback; the stops of a trip are restricted to those labeled with its
#' direction, so \code{stops_data$direction} and the trips' direction labels must
#' use the same values (see \code{stop_direction_map}).
#'
#' Both stages estimate. Trip boundaries can be misplaced without the trip count
#' looking wrong, and a stop the vehicle passed outside every buffer simply
#' produces no row. The returned tables therefore carry a coverage table
#' (\code{\link{g2g_diagnostics}}) reporting pings with no usable trip identity,
#' unmatched segments and out-of-range stops, and that is what to read before
#' treating the output as complete.
#'
#' The result is inference tables, not GTFS files; see the section below for what
#' still has to happen before this is a publishable feed.
#'
#' @param gps_data A data.frame or path to the raw GPS CSV.
#' @param terminals_data A data.frame or path to the terminal coordinates CSV.
#'   Optional (\code{NULL}) with \code{segmentation = "layover"} or with
#'   \code{direction_col}; required for terminal-buffer segmentation.
#' @param stops_data A data.frame or path to the bus stops coordinates CSV.
#'   Its \code{direction} column groups stops for matching; with layover
#'   segmentation the column is optional and any labels are ignored (all
#'   stops match all trips).
#' @param terminals_buffer_radius Numeric. Buffer radius for terminals (in
#'   meters). Only consumed by terminal-buffer segmentation; may be omitted
#'   otherwise.
#' @param stops_buffer_radius Numeric. Buffer radius for bus stops (in meters).
#' @param stops_extended_buffer_radius Numeric. Extended buffer radius for bus stops (in meters).
#' @param output_trips_path Character. Optional path to write output trip features as CSV. Default is \code{NULL} (no file written).
#' @param output_stops_path Character. Optional path to write output stop times as CSV. Default is \code{NULL} (no file written).
#' @param projected_crs Numeric. The EPSG code of a projected coordinate system
#'   used for metric distance calculations. Required when
#'   \code{projected = TRUE}; otherwise a suitable UTM projection is selected
#'   when omitted.
#' @param backend Character. The backend to use: \code{"auto"}, \code{"rust"},
#'   \code{"rcpp"}, or \code{"pure_r"}. Automatic selection prefers Rust, then
#'   Rcpp, then pure R. Explicit unavailable backends produce an error.
#' @param projected Logical. Whether plain-table coordinates are already
#'   projected. Out-of-bounds coordinates require explicit \code{TRUE}.
#' @param stop_direction_map Named character vector mapping each raw
#'   stop-direction label to the terminal ID that trips in that direction
#'   \emph{start from}. Required whenever \code{stops_data} labels its
#'   directions with anything other than the terminal IDs themselves: which
#'   label belongs to which terminal is not recoverable from the data (both
#'   direction groups span the same corridor), and getting it backwards
#'   silently reverses every direction in the output while still producing
#'   plausible stop times. Omitting it in that case is an error listing both
#'   candidate maps. Not needed when the labels are already terminal IDs, as
#'   with \code{\link{g2g_stops_from_gtfs}}, nor in data-driven direction mode
#'   (\code{direction_col}) or layover segmentation.
#' @param vehicle_col Character. Name of the vehicle identifier column in
#'   \code{gps_data}. Default \code{"vehicle_id"} (GTFS-Realtime convention).
#' @param time_col Character. Name of the timestamp column in \code{gps_data}.
#'   Default \code{"timestamp"} (GTFS-Realtime convention).
#' @param tz Character. Timezone in which non-\code{POSIXct} timestamps are
#'   interpreted; also the timezone whose midnight bounds service days and
#'   thus every downstream GTFS clock string. Pass the feed's operating
#'   timezone, or supply localized \code{POSIXct} timestamps. \code{NULL}
#'   (default) parses character input as UTC with a warning. Passed to
#'   \code{\link{g2g_clean_gps}}.
#' @param trip_col Character. Optional name(s) of column(s) holding supplied
#'   trip identities (e.g. \code{"trip_id"} from GTFS-Realtime Vehicle
#'   Positions). When given, trips are segmented by those identities (fast
#'   path) instead of inferred from terminal-buffer crossings. Without
#'   \code{direction_col}, the first/last ping of each segment is matched to
#'   the nearest terminal for direction assignment. May be several columns
#'   that jointly identify a trip - e.g. \code{c("route_id", "direction_id",
#'   "start_date", "start_time")}, the GTFS-Realtime TripDescriptor, for feeds
#'   whose positions carry no \code{trip_id}; they are combined into one
#'   identity. Default \code{NULL} (spatial inference).
#' @param direction_col Character. Optional column giving each ping's travel
#'   direction (e.g. GTFS-Realtime \code{"direction_id"}). Only used with
#'   \code{trip_col}. When supplied, trip direction is taken from the data
#'   rather than inferred from which of two terminals a trip started at, so
#'   \code{terminals_data} becomes optional and routes that are not simple
#'   two-terminal lines (short-turns, variants, one-way services) are handled.
#'   For stop-time extraction, stops are grouped for matching by their
#'   \code{direction} label, which must use the same values as
#'   \code{direction_col}. By default this column labels a trip's direction; it
#'   does not cut trips. A trip has exactly one direction, so if
#'   \code{direction_col} takes more than one value inside a segment that
#'   \code{trip_col} declared to be one trip, the two inputs contradict each
#'   other and extraction stops with an error: either add the direction column
#'   to \code{trip_col} to segment by it, set
#'   \code{cut_on_direction_change = TRUE}, or collapse it to one value per
#'   supplied trip identity. Default \code{NULL}.
#' @param cut_on_direction_change Logical. When \code{TRUE}, a change in
#'   \code{direction_col} cuts a trip rather than raising the mid-trip
#'   direction conflict error above. Raw direction fields flip spuriously, so
#'   the change is debounced first: only a run of consecutive identical values
#'   that is both long enough in pings and long enough in seconds counts as a
#'   real direction change, and shorter contrary bursts are absorbed into the
#'   direction around them. Requires \code{direction_col}. Default
#'   \code{FALSE}, which leaves behaviour unchanged.
#' @param direction_debounce_min_pings Integer. Minimum consecutive pings for a
#'   direction run to be treated as a real direction change. Inert unless
#'   \code{cut_on_direction_change = TRUE}. Note this floor only binds on
#'   high-cadence feeds or at small \code{direction_debounce_min_seconds}: at a
#'   15 s ping cadence a run needs about \code{min_seconds / 15 + 1} pings to
#'   span the seconds threshold at all, so from roughly 300 s upwards the
#'   seconds threshold binds first. Default \code{2L}.
#' @param direction_debounce_min_seconds Numeric. Minimum intra-run span, in
#'   seconds, for a direction run to be treated as a real direction change. A
#'   single-ping run spans 0 seconds and is therefore always absorbed. Inert
#'   unless \code{cut_on_direction_change = TRUE}. Default 600.
#' @param session_gap Numeric. Successive observations of a vehicle further
#'   apart than this many seconds start a new driving session; trips never
#'   span sessions. This replaces the former calendar-date boundary, so
#'   overnight trips crossing midnight stay intact while overnight parking
#'   still separates one day's operations from the next. Default 4 hours
#'   (\code{4 * 3600}).
#' @param segmentation Character. How the raw-GPS spatial path cuts trips when
#'   no \code{trip_col} is supplied: \code{"terminals"} (classic two-terminal
#'   buffer model), \code{"layover"} (cut wherever the vehicle dwells longer
#'   than \code{layover_gap}, anywhere on the route - handles short-turn,
#'   loop, and multi-branch services; all trips share a single direction
#'   group), or \code{"auto"} (default: \code{"terminals"} when
#'   \code{terminals_data} is given, \code{"layover"} otherwise). Not used
#'   with \code{trip_col}.
#' @param layover_gap Numeric. Layover segmentation only: dwells longer than
#'   this many seconds bound trips - whether a silent gap between successive
#'   pings or a stationary spell (pings present but staying within
#'   \code{layover_radius}). Mode-dependent: must exceed the feed's ping
#'   interval and normal in-service stop dwell, and stay below the shortest
#'   real layover. Default 10 minutes (\code{10 * 60}).
#' @param layover_radius Numeric. Layover segmentation only: a vehicle counts
#'   as stationary while successive pings stay within this many meters of the
#'   dwell's first ping (anchoring absorbs GPS jitter while parked).
#'   Default 50.
#' @param return_trajectory Logical. Also return the ping-level trajectory
#'   (cleaned GPS records with assigned \code{trip_id} and \code{direction})
#'   as a \code{trajectory} element — the input for
#'   \code{\link{g2g_shapes_from_trips}}. Default \code{FALSE}.
#' @param diagnostics_warn Logical. Emit a one-line \code{warning()} when the
#'   run drops coverage worth surfacing (pings with no usable trip identity,
#'   unmatched segments, stops out of range) so unattended or agent-driven
#'   pipelines notice silent loss. The full breakdown is always available via
#'   \code{\link{g2g_diagnostics}} regardless of this flag. Default
#'   \code{getOption("gps2gtfs.diagnostics_warn", TRUE)}.
#' @return A list containing two data.tables: \code{trips} and
#'   \code{stop_times}, plus \code{trajectory} when
#'   \code{return_trajectory = TRUE}. All times (\code{start_time},
#'   \code{end_time}, \code{arrival_time}, \code{departure_time}) are absolute
#'   \code{POSIXct} values in the timezone of the input timestamps — never
#'   clock strings, so trips running past midnight stay unambiguous.
#'
#'   Row order is a stable, backend-invariant contract: \code{trips} are
#'   ordered by the internal integer \code{trip_id}, and \code{stop_times} by
#'   \code{(trip_id, arrival_time, stop_id, departure_time)} with every
#'   remaining column appended as a final tie-breaker. The sort is therefore
#'   total, so the three backends (Rust, Rcpp, pure R) return identical row
#'   order for identical input and summaries built on the result are
#'   reproducible regardless of backend or parallelism.
#'
#'   The result also carries an \code{attr(., "diagnostics")} coverage table
#'   (see \code{\link{g2g_diagnostics}}).
#'
#' @section Inference tables, not GTFS files:
#' The returned \code{trips} and \code{stop_times} use GTFS-style column
#' names but are *inference tables*, not valid \code{trips.txt}/
#' \code{stop_times.txt}: they carry no \code{route_id}, \code{service_id},
#' \code{stop_sequence}, or GTFS clock strings (\code{"HH:MM:SS"}, with
#' \code{>24:00:00} for post-midnight stops). This is deliberate:
#' \code{route_id}, \code{service_id}, and canonical stop identities cannot be
#' inferred from coordinates alone, so \code{gps2gtfs} stops at inference
#' rather than synthesizing them.
#'
#' Turning these tables into a standard-compliant feed — deriving
#' \code{stop_sequence}, attributing each trip to a service day, encoding
#' \code{>24:00:00} clock strings, and linking or synthesizing IDs — is the
#' job of the companion package \code{gtfsrt2static} (a separate, optional
#' install), which yields a feed the MobilityData \code{gtfs-validator}
#' accepts:
#' \preformatted{
#' res    <- g2g_extract_trips_and_stop_times(...)
#' events <- gtfsrt2static::snapshot_from_stop_times(
#'   res$stop_times, trip_id_col = "provided_trip_id"  # keep official IDs
#' )
#' feed   <- gtfsrt2static::snapshot_assemble(events)  # baseline: real IDs
#' # or, with no planned feed to lean on:
#' # feed <- gtfsrt2static::snapshot_scaffold(events, strict = TRUE)
#' gtfsio::export_gtfs(feed, "realized_gtfs.zip")      # gtfstools/tidytransit-ready
#' }
#'
#' Both tables carry a \code{provided_trip_id} column: the caller's official
#' trip identity (the \code{trip_col} value, e.g. a GTFS-Realtime
#' \code{trip_id}) when the fast path is used, \code{NA} otherwise. It lets a
#' downstream assembler preserve official trip IDs instead of synthesizing
#' them. The internal integer \code{trip_id} remains the join key between the
#' two tables.
#'
#' @section Orientation and pattern labels (C5 contract):
#' Both tables also carry an additive, versioned set of inference-label columns.
#' They are the contract slots for a future baseline-free
#' orientation/turnaround-detection stage and are currently always in the
#' \dQuote{no detector applied} state (no such detector is enabled yet), so the
#' legacy \code{direction} column - retained unchanged - is what downstream
#' consumers read today:
#' \describe{
#'   \item{\code{orientation_id}}{Integer \code{0}/\code{1}, geometric travel
#'     direction only; \code{NA} when unavailable/abstained (always \code{NA}
#'     today).}
#'   \item{\code{orientation_status}}{Factor, never \code{NA}. \code{"none"}
#'     (no orientation method applied) today; reserved values \code{"ok"} /
#'     \code{"single_group"} / \code{"abstain_<reason>"} make a future
#'     detector's abstentions observable rather than silent.}
#'   \item{\code{orientation_confidence}}{Double in \code{[0,1]}, \code{NA}
#'     when \code{orientation_id} is \code{NA} (always \code{NA} today).}
#'   \item{\code{pattern_ref}}{Character K-way branch/short-turn variant
#'     identity; reserved nullable (always \code{NA} today). The handoff to
#'     \code{gtfsrt2static}'s cross-trip stop-order stage.}
#'   \item{\code{start_anchor_ref}, \code{end_anchor_ref}}{Character discovered
#'     turnaround-cluster ids; \strong{\code{trips} only} (trip-level metadata,
#'     not per-stop), \code{NA} today.}
#' }
#' Downstream, \code{gtfsrt2static} maps \code{orientation_id} into the GTFS
#' \code{direction_id} (falling back to \code{direction} while
#' \code{orientation_id} is \code{NA}); \code{pattern_ref} rides through to C6;
#' anchors stay in C5. \code{orientation_*}/\code{pattern_ref} propagate from
#' \code{trips} onto \code{stop_times} on the internal \code{trip_id}, the same
#' mechanism as \code{provided_trip_id}.
#' @examples
#' \donttest{
#' data(g2g_data_gps)
#' data(g2g_data_terminals)
#' data(g2g_data_stops)
#' result <- g2g_extract_trips_and_stop_times(
#'   gps_data = g2g_data_gps,
#'   terminals_data = g2g_data_terminals,
#'   stops_data = g2g_data_stops,
#'   terminals_buffer_radius = 50,
#'   stops_buffer_radius = 30,
#'   stops_extended_buffer_radius = 50,
#'   # Each stop-direction label maps to the terminal its trips start from.
#'   # BT01 is Kandy, BT02 is Digana.
#'   stop_direction_map = c(
#'     "Kandy-Digana" = "BT01",
#'     "Digana-Kandy" = "BT02"
#'   )
#' )
#' head(result$trips)
#' head(result$stop_times)
#' }
#' @importFrom data.table fread fwrite as.data.table
#' @importFrom stats median
#' @export
g2g_extract_trips_and_stop_times <- function(
  gps_data,
  terminals_data = NULL,
  stops_data,
  terminals_buffer_radius = NULL,
  stops_buffer_radius,
  stops_extended_buffer_radius,
  output_trips_path = NULL,
  output_stops_path = NULL,
  projected_crs = NULL,
  backend = "auto",
  projected = NULL,
  stop_direction_map = NULL,
  vehicle_col = "vehicle_id",
  time_col = "timestamp",
  tz = NULL,
  trip_col = NULL,
  direction_col = NULL,
  cut_on_direction_change = FALSE,
  direction_debounce_min_pings = 2L,
  direction_debounce_min_seconds = 600,
  session_gap = 4 * 3600,
  segmentation = c("auto", "terminals", "layover"),
  layover_gap = 10 * 60,
  layover_radius = 50,
  return_trajectory = FALSE,
  diagnostics_warn = getOption("gps2gtfs.diagnostics_warn", TRUE)
) {
  backend <- resolve_backend(backend)
  validate_positive_radius(stops_buffer_radius, "stops_buffer_radius")
  validate_positive_radius(
    stops_extended_buffer_radius,
    "stops_extended_buffer_radius"
  )
  validate_projected_crs(projected_crs)
  validate_session_gap(session_gap)
  validate_layover_gap(layover_gap)
  validate_layover_radius(layover_radius)
  if (isTRUE(projected) && is.null(projected_crs)) {
    stop("'projected_crs' is required when 'projected = TRUE'.", call. = FALSE)
  }
  if (!is.null(direction_col) && is.null(trip_col)) {
    stop(
      "'direction_col' is only used with 'trip_col' (data-driven direction ",
      "applies to the supplied-trip-identity fast path).",
      call. = FALSE
    )
  }
  if (isTRUE(cut_on_direction_change) && is.null(direction_col)) {
    stop(
      "'cut_on_direction_change = TRUE' requires 'direction_col': there is ",
      "no direction series to debounce without it.",
      call. = FALSE
    )
  }
  seg_mode <- resolve_segmentation(segmentation, trip_col, !is.null(terminals_data))
  # The terminal buffer radius is only consumed by terminal-buffer
  # segmentation; other modes may omit it.
  if (seg_mode == "terminals" || !is.null(terminals_buffer_radius)) {
    validate_positive_radius(terminals_buffer_radius, "terminals_buffer_radius")
  }
  if (seg_mode == "layover" && layover_gap >= session_gap) {
    warning(
      "'layover_gap' (",
      layover_gap,
      "s) is not smaller than 'session_gap' (",
      session_gap,
      "s); layover cuts are subsumed by session cuts.",
      call. = FALSE
    )
  }
  if (seg_mode == "layover" && !is.null(stop_direction_map)) {
    message(
      "[INFO] 'stop_direction_map' is not used with layover segmentation."
    )
  }
  message(
    "Starting Pipeline for extracting Trip and Bus Stop Data using backend: ",
    backend
  )

  raw_gps_df <- if (is.character(gps_data) && length(gps_data) == 1L) data.table::fread(gps_data) else data.table::as.data.table(gps_data)
  # Some feeds (e.g. OVapi) leave vehicle_id empty. When trip identities are
  # supplied, segmentation does not need a vehicle id, so fill a placeholder
  # rather than dropping every row in g2g_clean_gps().
  if (!is.null(trip_col)) {
    if (!vehicle_col %in% names(raw_gps_df) || all(is.na(raw_gps_df[[vehicle_col]]))) {
      message(
        "[INFO] No usable '",
        vehicle_col,
        "'; filling a single placeholder (trip identities drive segmentation)."
      )
      raw_gps_df <- data.table::copy(raw_gps_df)
      raw_gps_df[[vehicle_col]] <- "veh_1"
    }
  }
  stops_df <- if (is.character(stops_data) && length(stops_data) == 1L) data.table::fread(stops_data) else data.table::as.data.table(stops_data)

  n_pings_in <- nrow(raw_gps_df)
  cleaned <- g2g_clean_gps(
    raw_gps_df,
    projected = projected,
    vehicle_col = vehicle_col,
    time_col = time_col,
    tz = tz
  )
  clean_drops <- attr(cleaned, "clean_drops")
  projected <- attr(cleaned, "projected")
  if (projected && is.null(projected_crs)) {
    stop(
      "'projected_crs' is required for projected coordinates.",
      call. = FALSE
    )
  }

  # Terminals are optional in data-driven direction mode (direction_col).
  have_terminals <- !is.null(terminals_data)
  if (have_terminals) {
    trip_terminals_df <- if (is.character(terminals_data) && length(terminals_data) == 1L) data.table::fread(terminals_data) else data.table::as.data.table(terminals_data)
    terminals <- normalize_coordinates(
      trip_terminals_df,
      projected = projected,
      name = "trip_terminals_df"
    )$dt
    validate_required_columns(terminals, "terminal_id", "trip_terminals_df")
    validate_identifiers(terminals$terminal_id, "trip_terminals_df 'terminal_id'")
    terminal_ids <- unique(as.character(terminals$terminal_id))
  } else {
    if (seg_mode == "fast" && is.null(direction_col)) {
      stop(
        "'terminals_data' is required unless 'direction_col' is supplied.",
        call. = FALSE
      )
    }
    terminals <- data.table::data.table(terminal_id = character(0))
    terminal_ids <- character(0)
  }

  # Data-driven direction levels: shared vocabulary across supplied trip
  # directions and stop direction labels, mapped to 1..K downstream.
  # Layover segmentation infers no direction; every trip and stop shares one
  # synthetic level (mapped to direction 1L downstream).
  direction_levels <- NULL
  if (seg_mode == "layover") {
    direction_levels <- layover_direction_level
  } else if (!is.null(direction_col)) {
    validate_required_columns(cleaned, direction_col, "gps_data")
    direction_levels <- sort(unique(as.character(stats::na.omit(
      c(as.character(cleaned[[direction_col]]), as.character(stops_df$direction))
    ))))
    if (length(direction_levels) == 0L) {
      stop("'direction_col' yielded no usable direction values.", call. = FALSE)
    }
  }

  stops <- normalize_coordinates(
    stops_df,
    projected = projected,
    name = "stops_df"
  )$dt
  if (seg_mode == "layover") {
    # Single direction group: the stop 'direction' column becomes optional,
    # and any supplied labels are ignored so all stops match all trips.
    stops <- data.table::copy(stops)
    if ("direction" %in% names(stops) && any(!is.na(stops$direction))) {
      message(
        "[INFO] Layover segmentation uses a single direction group; stop ",
        "'direction' labels are ignored."
      )
    }
    stops[, direction := layover_direction_level]
  }
  if (nrow(stops) > 0L) {
    resolve_stop_directions(
      stops, terminal_ids, stop_direction_map, direction_levels = direction_levels
    )
  } else {
    validate_required_columns(stops, c("stop_id", "direction"), "stops_df")
  }

  if (nrow(cleaned) == 0L) {
    result <- list(
      trips = set_backend(empty_trip_features(cleaned$vehicle_id), backend),
      stop_times = empty_stop_times(cleaned$vehicle_id)
    )
    if (isTRUE(return_trajectory)) {
      result$trajectory <- empty_trajectory(cleaned)
    }
    diag <- build_diagnostics(c(
      pings_in = as.integer(n_pings_in),
      clean_drops,
      pings_after_cleaning = 0L,
      pings_assigned_to_trips = 0L,
      pings_dropped_not_in_trip = 0L,
      trips_kept = 0L,
      stop_times_kept = 0L
    ))
    result <- set_diagnostics(set_backend(result, backend), diag)
    maybe_warn_diagnostics(diag, diagnostics_warn)
    return(result)
  }

  if (is.null(projected_crs) && !projected) {
    med_lon <- stats::median(cleaned$longitude, na.rm = TRUE)
    med_lat <- stats::median(cleaned$latitude, na.rm = TRUE)
    zone <- floor((med_lon + 180) / 6) + 1
    is_northern <- med_lat >= 0
    projected_crs <- if (is_northern) (32600 + zone) else (32700 + zone)
    message(
      "[INFO] Auto-detected local UTM projection EPSG:",
      projected_crs,
      " (Zone ",
      zone,
      if (is_northern) "N" else "S",
      ")"
    )
  }

  if (seg_mode == "fast") {
    message(
      "[INFO] Using supplied trip identities from column(s) '",
      paste(trip_col, collapse = "', '"),
      "' (fast path)."
    )
    trips <- extract_trips_from_ids_r(
      cleaned,
      terminals,
      trip_col,
      projected = projected,
      session_gap = session_gap,
      direction_col = direction_col,
      cut_on_direction_change = cut_on_direction_change,
      direction_debounce_min_pings = direction_debounce_min_pings,
      direction_debounce_min_seconds = direction_debounce_min_seconds
    )
  } else if (seg_mode == "layover") {
    trips <- extract_trips_layover_r(
      cleaned,
      terminals,
      layover_gap = layover_gap,
      layover_radius = layover_radius,
      session_gap = session_gap,
      projected = projected
    )
  } else {
    trips <- extract_trips_r(
      cleaned,
      terminals,
      terminals_buffer_radius,
      projected_crs = projected_crs,
      backend = backend,
      projected = projected,
      session_gap = session_gap
    )
  }
  seg_drops <- attr(trips, "seg_drops")
  trip_features <- extract_trip_features_r(
    trips,
    terminal_ids,
    direction_levels = direction_levels
  )
  trip_features <- set_backend(trip_features, backend)

  trajectory <- prepare_trajectory_r(
    cleaned,
    trips,
    trip_features,
    backend = backend
  )
  attr(trajectory, "projected") <- projected

  stop_times <- extract_stops_r(
    trajectory,
    stops,
    terminals,
    stops_buffer_radius,
    stops_extended_buffer_radius,
    projected_crs = projected_crs,
    backend = backend,
    projected = projected,
    stop_direction_map = stop_direction_map,
    terminal_ids = terminal_ids,
    direction_levels = direction_levels
  )

  if ("bus_stop" %in% names(stop_times)) {
    data.table::setnames(stop_times, "bus_stop", "stop_id")
  }

  # Carry trip-level metadata onto stop_times, keyed on the internal integer
  # trip_id shared by both tables. This is the single propagation mechanism for
  # provided_trip_id (the caller's official trip identity, fast path; NA in
  # spatial mode) AND the additive C5 orientation/pattern columns: when the
  # (not-yet-authorized) P2/P4 detectors populate them per trip, stop_times
  # inherits the exact same values here. Anchors are NOT propagated - they stay
  # trips-only (spike §3.4). With detectors off every value is the empty state.
  if (nrow(stop_times) > 0L && "provided_trip_id" %in% names(trip_features)) {
    stop_times[
      trip_features,
      `:=`(
        provided_trip_id = i.provided_trip_id,
        orientation_id = i.orientation_id,
        orientation_status = i.orientation_status,
        orientation_confidence = i.orientation_confidence,
        pattern_ref = i.pattern_ref
      ),
      on = "trip_id"
    ]
  }

  # Stable, backend-invariant row order (documented return contract). trips are
  # keyed by the internal integer trip_id (one row per trip, so unique). For
  # stop_times the meaningful key is (trip_id, arrival_time, stop_id,
  # departure_time); every remaining column is appended as a final tie-breaker
  # so the sort is total and the order is fully determined regardless of the
  # parallel, backend-specific extraction order - it never relies on any column
  # being derived from another.
  data.table::setorder(trip_features, trip_id)
  if (nrow(stop_times) > 0L) {
    stop_order_keys <- c(
      "trip_id",
      "arrival_time",
      "stop_id",
      "departure_time"
    )
    data.table::setorderv(
      stop_times,
      c(stop_order_keys, setdiff(names(stop_times), stop_order_keys))
    )
  }

  if (!is.null(output_trips_path)) {
    data.table::fwrite(trip_features, output_trips_path)
  }
  if (!is.null(output_stops_path)) {
    data.table::fwrite(stop_times, output_stops_path)
  }

  if (!is.null(output_trips_path) || !is.null(output_stops_path)) {
    message("Pipeline finished successfully! Outputs saved.")
  } else {
    message("Pipeline finished successfully!")
  }

  # Coverage diagnostics: boundary ping/trip counts plus per-reason drops
  # (cleaning breakdown from g2g_clean_gps, segment drops from the extractor).
  diag <- build_diagnostics(c(
    pings_in = as.integer(n_pings_in),
    clean_drops,
    pings_after_cleaning = nrow(cleaned),
    seg_drops,
    pings_assigned_to_trips = nrow(trajectory),
    pings_dropped_not_in_trip = nrow(cleaned) - nrow(trajectory),
    trips_kept = nrow(trip_features),
    max_trip_duration_mins = max_trip_duration_metric(trip_features),
    stop_times_kept = nrow(stop_times)
  ))

  result <- list(trips = trip_features, stop_times = stop_times)
  if (isTRUE(return_trajectory)) {
    result$trajectory <- trajectory
  }
  # Attach to the top-level result only: stamping the sub-tables would make
  # two otherwise-identical tables compare unequal on the diagnostics attribute.
  result <- set_diagnostics(set_backend(result, backend), diag)
  maybe_warn_diagnostics(diag, diagnostics_warn)
  result
}
