# Extraction coverage diagnostics -------------------------------------------
#
# Every public extractor attaches an `attr(., "diagnostics")` table describing
# how many GPS pings, trip segments, and trips survived each stage of the
# pipeline. The table has a fixed (stage, metric) schema so results are
# directly comparable across runs; a metric that a given run cannot measure
# (e.g. stop-time metrics from `g2g_extract_trips()`, which never matches
# stops) is reported as NA rather than a misleading 0.

# Canonical metric schema in display order. `n` is filled per run.
g2g_diagnostics_schema <- function() {
  data.table::data.table(
    stage = c(
      "input",
      "cleaning",
      "cleaning",
      "cleaning",
      "cleaning",
      "segmentation",
      "segmentation",
      "segmentation",
      "trips",
      "trips",
      "trips",
      "trips",
      "stops"
    ),
    metric = c(
      "pings_in",
      "pings_dropped_zero_coord",
      "pings_dropped_missing_vehicle",
      "pings_dropped_duplicate",
      "pings_after_cleaning",
      "rows_dropped_no_trip_identity",
      "segments_dropped_single_ping",
      "segments_dropped_stationary",
      "pings_assigned_to_trips",
      "pings_dropped_not_in_trip",
      "trips_kept",
      # Reported, never judged: a mis-segmented run merges a whole shift into
      # one "trip", and no other metric shows it - the trip count can be off by
      # 1% while boundaries are badly wrong. There is no threshold worth
      # hard-coding (a long-distance coach trip really can run for hours), so
      # surface the number and let the caller decide.
      "max_trip_duration_mins",
      "stop_times_kept"
    )
  )
}

# Longest extracted trip, in whole minutes, for the `max_trip_duration_mins`
# metric. NA when nothing was extracted, so an empty run does not report 0.
max_trip_duration_metric <- function(trip_features) {
  if (
    is.null(trip_features) ||
      nrow(trip_features) == 0L ||
      !"duration_in_mins" %in% names(trip_features)
  ) {
    return(NA_integer_)
  }
  longest <- suppressWarnings(max(trip_features$duration_in_mins, na.rm = TRUE))
  if (!is.finite(longest)) {
    return(NA_integer_)
  }
  as.integer(round(longest))
}

# Drop metrics worth warning about: every "dropped" metric except routine
# duplicate removal (archived realtime feeds re-report unchanged pings), which
# stays in the table but never triggers the coverage warning.
diagnostics_concerning_drops <- function(diagnostics) {
  diagnostics[
    grepl("dropped", diagnostics$metric) &
      diagnostics$metric != "pings_dropped_duplicate" &
      !is.na(diagnostics$n) &
      diagnostics$n > 0L
  ]
}

# Build the diagnostics table from a named vector of measured counts. Unlisted
# metrics stay NA (not measured for this run).
build_diagnostics <- function(counts = NULL) {
  schema <- g2g_diagnostics_schema()
  n <- stats::setNames(rep(NA_integer_, nrow(schema)), schema$metric)
  if (length(counts) > 0L) {
    known <- intersect(names(counts), names(n))
    n[known] <- as.integer(counts[known])
  }
  out <- data.table::data.table(
    stage = schema$stage,
    metric = schema$metric,
    n = unname(n)
  )
  data.table::setattr(out, "class", c("g2g_diagnostics", class(out)))
  out
}

set_diagnostics <- function(x, diagnostics) {
  attr(x, "diagnostics") <- diagnostics
  x
}

diagnostics_value <- function(diagnostics, metric) {
  v <- diagnostics$n[diagnostics$metric == metric]
  if (length(v) == 0L) NA_integer_ else v[[1L]]
}

fmt_count <- function(x) {
  if (length(x) == 0L || is.na(x)) {
    return("NA")
  }
  formatC(x, format = "d", big.mark = ",")
}

# Human-readable summary lines shared by the print method and the warning.
diagnostics_summary_lines <- function(diagnostics) {
  pings_in <- diagnostics_value(diagnostics, "pings_in")
  trips <- diagnostics_value(diagnostics, "trips_kept")
  stop_times <- diagnostics_value(diagnostics, "stop_times_kept")

  headline <- if (is.na(stop_times)) {
    sprintf(
      "%s pings in -> %s trips",
      fmt_count(pings_in),
      fmt_count(trips)
    )
  } else {
    sprintf(
      "%s pings in -> %s trips, %s stop_times",
      fmt_count(pings_in),
      fmt_count(trips),
      fmt_count(stop_times)
    )
  }

  dropped <- diagnostics[
    grepl("dropped", diagnostics$metric) &
      !is.na(diagnostics$n) &
      diagnostics$n > 0L
  ]
  data.table::setorderv(dropped, "n", order = -1L)
  if (nrow(dropped) == 0L) {
    reasons <- "  no pings, segments, or trips dropped"
  } else {
    reasons <- c(
      "  dropped:",
      sprintf("    %s: %s", dropped$metric, vapply(dropped$n, fmt_count, ""))
    )
  }
  c(headline, reasons)
}

# Warning message when a run lost coverage worth surfacing, else NULL. This is
# what makes an unattended (agentic) run notice silent, non-random trip loss:
# the loss shows up in the run's warning stream, not only in an attribute.
# Each reason is reported in its own unit (pings, segments, rows) and never
# summed across units, so the count for a reason always matches its metric.
diagnostics_warning_message <- function(diagnostics) {
  dropped <- diagnostics_concerning_drops(diagnostics)
  if (nrow(dropped) == 0L) {
    return(NULL)
  }
  data.table::setorderv(dropped, "n", order = -1L)
  reasons <- paste(
    sprintf("%s (%s)", dropped$metric, vapply(dropped$n, fmt_count, "")),
    collapse = ", "
  )
  pings_in <- diagnostics_value(diagnostics, "pings_in")
  trips <- diagnostics_value(diagnostics, "trips_kept")
  sprintf(
    paste0(
      "gps2gtfs extraction lost coverage: kept %s trips from %s input pings. ",
      "Dropped %s. Inspect the full coverage table with g2g_diagnostics(result) ",
      "(also attr(result, \"diagnostics\")). Silence with diagnostics_warn = ",
      "FALSE or options(gps2gtfs.diagnostics_warn = FALSE)."
    ),
    fmt_count(trips),
    fmt_count(pings_in),
    reasons
  )
}

maybe_warn_diagnostics <- function(diagnostics, enabled) {
  if (!isTRUE(enabled)) {
    return(invisible(NULL))
  }
  msg <- diagnostics_warning_message(diagnostics)
  if (!is.null(msg)) {
    warning(msg, call. = FALSE)
  }
  invisible(NULL)
}

#' Extraction coverage diagnostics
#'
#' Retrieve the coverage diagnostics attached to the return value of
#' \code{\link{g2g_extract_trips}} or
#' \code{\link{g2g_extract_trips_and_stop_times}}. The diagnostics record how
#' many GPS pings, trip segments, and trips survived each stage of extraction,
#' so a non-random loss of coverage (a feed whose vehicle positions carry no
#' usable trip identity, routes the two-terminal model cannot segment, stops
#' out of buffer range) is visible as data instead of vanishing silently.
#'
#' The extractors also emit a one-line \code{warning()} whenever a run loses
#' coverage worth surfacing, pointing back here — so unattended or agent-driven
#' pipelines notice the loss without having to inspect attributes. Duplicate
#' removal is treated as routine and never triggers that warning (it still
#' appears in the table). Suppress the warning with \code{diagnostics_warn =
#' FALSE} or \code{options(gps2gtfs.diagnostics_warn = FALSE)}.
#'
#' @param x The value returned by
#'   \code{\link{g2g_extract_trips_and_stop_times}} (a list) or by
#'   \code{\link{g2g_extract_trips}} (a data.table). Diagnostics are attached to
#'   that top-level object, not to the individual \code{trips}/\code{stop_times}
#'   tables inside the list.
#' @return A \code{g2g_diagnostics} data.table with columns \code{stage},
#'   \code{metric}, and \code{n} (integer count; \code{NA} where a metric does
#'   not apply to the run). It has a compact \code{print} method; treat it as a
#'   normal data.table for programmatic use.
#' @examples
#' \donttest{
#' data(g2g_data_gps)
#' data(g2g_data_terminals)
#' data(g2g_data_stops)
#' res <- g2g_extract_trips_and_stop_times(
#'   gps_data = g2g_data_gps,
#'   terminals_data = g2g_data_terminals,
#'   stops_data = g2g_data_stops,
#'   terminals_buffer_radius = 50,
#'   stops_buffer_radius = 30,
#'   stops_extended_buffer_radius = 50
#' )
#' g2g_diagnostics(res)
#' }
#' @export
g2g_diagnostics <- function(x) {
  d <- attr(x, "diagnostics")
  if (is.null(d)) {
    stop(
      "No diagnostics found. Pass the value returned by g2g_extract_trips() ",
      "or g2g_extract_trips_and_stop_times() (not an individual trips/",
      "stop_times table extracted from the result list).",
      call. = FALSE
    )
  }
  d
}

#' @export
print.g2g_diagnostics <- function(x, ...) {
  cat("<gps2gtfs extraction diagnostics>\n")
  cat(diagnostics_summary_lines(x), sep = "\n")
  cat("\n")
  body <- data.table::copy(x)
  data.table::setattr(body, "class", c("data.table", "data.frame"))
  print(body, ...)
  invisible(x)
}
