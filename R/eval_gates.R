# Pre-registration, leakage-safe splits, and the two authorization gates for
# the terminal-detection program (private/terminal-detection-spike.md §4, §6e).
# Internal research functions - not the public gps2gtfs API. No detector logic:
# these freeze the decision rules and structure the evidence, so that whether a
# detector is authorized is decided by pre-committed thresholds, not post-hoc.

# ---- §6e Leakage-safe splits ------------------------------------------------
# Partition a labelled dataset into the fixed evaluation splits: A-calibration
# (fit/tune every baseline and detector here ONLY), A-held-out (the within-
# agency test), B-transfer (a fully held-out agency, required only for a
# transfer claim). The four-step protocol (choose metrics -> tune on
# calibration -> freeze -> evaluate once) is enforced by convention + the
# pre-registration object below; this helper only carves the partitions.
#
# `data` needs an `agency` column and a splitter: either a `period` column
# (e.g. service_day) with `calibration_periods`, or an explicit `split` column
# already valued "calibration"/"heldout".
eval_make_splits <- function(
  data, agency_a, agency_b = NULL,
  period_col = NULL, calibration_periods = NULL
) {
  data <- data.table::as.data.table(data)
  if (!"agency" %in% names(data)) {
    stop("`data` must have an `agency` column.", call. = FALSE)
  }
  if (!is.null(agency_b) && identical(agency_b, agency_a)) {
    stop(
      "`agency_b` must differ from `agency_a`: a transfer claim requires a ",
      "genuinely held-out agency, not the calibration agency itself.",
      call. = FALSE
    )
  }
  a <- data[agency == agency_a]
  if (!is.null(period_col)) {
    if (is.null(calibration_periods)) {
      stop("supply `calibration_periods` with `period_col`.", call. = FALSE)
    }
    calib <- a[a[[period_col]] %in% calibration_periods]
    heldout <- a[!a[[period_col]] %in% calibration_periods]
  } else if ("split" %in% names(a)) {
    calib <- a[split == "calibration"]
    heldout <- a[split == "heldout"]
  } else {
    stop(
      "supply `period_col` + `calibration_periods`, or a `split` column.",
      call. = FALSE
    )
  }
  transfer <- if (!is.null(agency_b)) data[agency == agency_b] else NULL
  if (nrow(calib) == 0L || nrow(heldout) == 0L) {
    warning(
      "calibration or held-out split is empty; within-agency test is not ",
      "possible.", call. = FALSE
    )
  }
  structure(
    list(
      calibration = calib, heldout = heldout, transfer = transfer,
      transfer_claim_supported = !is.null(transfer) && nrow(transfer) > 0L
    ),
    class = "g2g_eval_splits"
  )
}

# ---- Pre-registration object ------------------------------------------------
# Freeze the primary metric, IoU thresholds, the fragment-scoring convention,
# and every gate threshold BEFORE a method exists (spike §6). `evaluate` must
# be called against a frozen registration; there is no way to alter thresholds
# after freezing except to create a new, dated registration - which is the
# audit trail.
eval_preregister <- function(
  primary_metric = "end_to_end_orientation_recovery",
  iou_thresholds = c(0.5, 0.8),
  fragment_convention = c("once_via_assigned_segment", "ping_duration_weighted"),
  min_orientation_coverage = 0.7,
  # Gate 1 (use-case): fraction of intended datasets that satisfy the
  # eligibility predicate (spike §4 gate 1). Gate 2 (residual-performance):
  # the residual left after the best leakage-safe simple baseline must clear
  # these floors to justify building beyond it.
  gate1_min_eligible_fraction = 0.2,
  gate2_min_residual_recovery_gain = 0.1,
  gate2_max_merge_rate = 0.2,
  gate2_max_fragment_rate = 0.2,
  frozen_on = NULL
) {
  fragment_convention <- match.arg(fragment_convention)
  if (is.null(frozen_on)) {
    stop(
      "`frozen_on` (a date string) is required - a registration must be dated ",
      "to be the pre-commitment audit trail.", call. = FALSE
    )
  }
  gates <- list(
    gate1_min_eligible_fraction = gate1_min_eligible_fraction,
    gate2_min_residual_recovery_gain = gate2_min_residual_recovery_gain,
    gate2_max_merge_rate = gate2_max_merge_rate,
    gate2_max_fragment_rate = gate2_max_fragment_rate
  )
  pr <- structure(
    list(
      primary_metric = primary_metric,
      iou_thresholds = iou_thresholds,
      fragment_convention = fragment_convention,
      min_orientation_coverage = min_orientation_coverage,
      gates = gates,
      frozen_on = frozen_on,
      fingerprint = NA_character_
    ),
    class = "g2g_eval_preregistration"
  )
  pr$fingerprint <- eval_prereg_fingerprint(pr)
  pr
}

# Deterministic fingerprint recomputed from a registration's CURRENT fields.
# Never trust a stored `$fingerprint`: it can go stale if the object is copied
# and mutated (review critical 1), so callers recompute here and use the stored
# value only to detect tampering.
eval_prereg_fingerprint <- function(pr) {
  paste(c(
    pr$primary_metric, paste(format(pr$iou_thresholds), collapse = ","),
    pr$fragment_convention, format(pr$min_orientation_coverage),
    vapply(pr$gates, format, character(1)), pr$frozen_on
  ), collapse = "|")
}

# Recompute the fingerprint and reject a registration whose stored fingerprint
# no longer matches its fields (i.e. it was mutated after freezing). Returns the
# authoritative (recomputed) fingerprint.
eval_check_prereg <- function(prereg) {
  recomputed <- eval_prereg_fingerprint(prereg)
  if (!identical(prereg$fingerprint, recomputed)) {
    stop(
      "pre-registration was mutated after freezing (its stored fingerprint is ",
      "stale). Re-freeze with eval_preregister() rather than editing fields.",
      call. = FALSE
    )
  }
  recomputed
}

# Validate that an authorization input is one finite numeric scalar in [0, 1].
# The gate boundary must fail closed on malformed evidence (review critical 2).
eval_check_unit_scalar <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x < 0 || x > 1) {
    stop("`", name, "` must be one finite number in [0, 1].", call. = FALSE)
  }
  invisible(x)
}

# recovery_gain = detector - baseline recovery, so it lives in [-1, 1].
eval_check_signed_unit <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x < -1 || x > 1) {
    stop("`", name, "` must be one finite number in [-1, 1].", call. = FALSE)
  }
  invisible(x)
}

# A count must be one finite, non-negative, WHOLE-valued scalar (a fraction of a
# dataset is impossible evidence; review round-6). Double-encoded whole numbers
# (e.g. 5.0) are fine; 0.2, Inf, NA, and vectors are not.
eval_check_count <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x < 0 || x != round(x)) {
    stop("`", name, "` must be one finite, whole, non-negative number.",
      call. = FALSE)
  }
  invisible(x)
}

# Pure gate DECISION rules - the single source of truth used both when a gate is
# constructed AND when eval_gate_report() recomputes the decision from evidence
# under the live pre-registration. The report never trusts a cached `$pass`
# field (review round-4 blocking 1); it re-derives pass from these.
eval_gate1_decision <- function(eligible_fraction, prereg) {
  # NA/missing evidence cannot authorize -> fail closed; a numeric out of range
  # is malformed -> error.
  if (length(eligible_fraction) != 1L || !is.numeric(eligible_fraction) ||
      !is.finite(eligible_fraction)) {
    return(FALSE)
  }
  eval_check_unit_scalar(eligible_fraction, "eligible_fraction")
  isTRUE(eligible_fraction >= prereg$gates$gate1_min_eligible_fraction)
}
eval_gate2_decision <- function(recovery_gain, merge_rate, fragment_rate,
                                orientation_coverage, prereg) {
  eval_check_signed_unit(recovery_gain, "recovery_gain")
  eval_check_unit_scalar(merge_rate, "merge_rate")
  eval_check_unit_scalar(fragment_rate, "fragment_rate")
  eval_check_unit_scalar(orientation_coverage, "orientation_coverage")
  g <- prereg$gates
  coverage_gate <- isTRUE(orientation_coverage >= prereg$min_orientation_coverage)
  list(
    coverage_gate = coverage_gate,
    pass = isTRUE(recovery_gain >= g$gate2_min_residual_recovery_gain) &&
      isTRUE(merge_rate <= g$gate2_max_merge_rate) &&
      isTRUE(fragment_rate <= g$gate2_max_fragment_rate) &&
      coverage_gate
  )
}

# ---- §4 Two-gate report -----------------------------------------------------
# Gate 1: the use-case gate. `eligibility` is a per-dataset logical vector (or a
# data.frame with an `eligible` column) computed by the caller from the §4
# predicate: usable per-vehicle identity AND (one-route OR usable route
# identity) AND no usable trip identity AND no supplied/discoverable terminals.
# Datasets are STRATIFIED (not excluded) by baseline/stops/bearing availability
# upstream; this only counts the eligible fraction against the frozen floor.
eval_gate1_use_case <- function(eligibility, prereg) {
  fp <- eval_check_prereg(prereg)
  if (is.data.frame(eligibility)) eligibility <- eligibility$eligible
  # Require a genuine logical: as.logical() would turn any nonzero numeric
  # (invalid category codes, 2, ...) into TRUE and silently inflate eligibility
  # (review round-5 critical 2). Logical NA keeps its conservative treatment.
  if (!is.logical(eligibility)) {
    stop(
      "`eligibility` must be a logical vector (or a data.frame with a logical ",
      "`eligible` column); numeric/character codes are not accepted.",
      call. = FALSE
    )
  }
  n_unknown <- sum(is.na(eligibility))
  if (n_unknown > 0L) {
    warning(
      n_unknown, " dataset(s) have unknown eligibility (NA); counted as NOT ",
      "eligible. Resolve them before relying on the gate-1 fraction.",
      call. = FALSE
    )
  }
  # An undetermined (NA) dataset cannot be claimed as eligible: count only TRUE
  # in the numerator, over ALL datasets in the denominator (no na.rm).
  frac <- if (length(eligibility)) {
    sum(eligibility %in% TRUE) / length(eligibility)
  } else NA_real_
  list(
    eligible_fraction = frac,
    n_datasets = length(eligibility),
    n_eligible = sum(eligibility %in% TRUE),
    n_unknown = n_unknown,
    threshold = prereg$gates$gate1_min_eligible_fraction,
    pass = eval_gate1_decision(frac, prereg),
    prereg_fingerprint = fp
  )
}

# Gate 2: residual-performance gate. Compares the detector's held-out recovery
# against the best properly-tuned, leakage-safe SIMPLE baseline (layover at a
# calibration-fitted gap + the trivial bearing/displacement classifier). Only a
# non-trivial residual GAIN over the baseline - with merge/fragment rates under
# their frozen caps - justifies building beyond the simple baseline. The
# single-group layover floor has NO two-class orientation skill and must be
# passed in as baseline_recovery = its recovery scored as "orientation
# unavailable", never as a classifier (spike §6e).
eval_gate2_residual <- function(
  detector_recovery, baseline_recovery, merge_rate, fragment_rate,
  orientation_coverage, prereg
) {
  if (missing(orientation_coverage)) {
    stop(
      "`orientation_coverage` is required: the frozen minimum-coverage floor ",
      "(", prereg$min_orientation_coverage, ") is part of gate 2 - a detector ",
      "must not pass by orienting only the easy trips.", call. = FALSE
    )
  }
  fp <- eval_check_prereg(prereg)
  # Fail closed on malformed evidence: recovery inputs must be finite [0,1].
  eval_check_unit_scalar(detector_recovery, "detector_recovery")
  eval_check_unit_scalar(baseline_recovery, "baseline_recovery")
  gain <- detector_recovery - baseline_recovery
  # Decision via the shared rule (validates the remaining evidence too).
  decision <- eval_gate2_decision(
    gain, merge_rate, fragment_rate, orientation_coverage, prereg
  )
  list(
    detector_recovery = detector_recovery,
    baseline_recovery = baseline_recovery,
    recovery_gain = gain,
    merge_rate = merge_rate,
    fragment_rate = fragment_rate,
    orientation_coverage = orientation_coverage,
    min_orientation_coverage = prereg$min_orientation_coverage,
    coverage_gate = decision$coverage_gate,
    thresholds = prereg$gates,
    pass = decision$pass,
    prereg_fingerprint = fp
  )
}

# Assemble the full gate report. Detector implementation beyond the simple
# baseline is authorized ONLY when BOTH gates pass (spike §4, §12).
eval_gate_report <- function(gate1, gate2 = NULL, prereg) {
  fp <- eval_check_prereg(prereg)  # recompute from live fields, reject tampering
  # The report must NOT trust cached `$pass`/`$coverage_gate` fields on the gate
  # objects - those are mutable and could be flipped or fabricated (review
  # round-4 blocking 1). It re-derives every decision from the gates' EVIDENCE
  # under the live pre-registration, failing closed on incomplete/mismatched
  # gate objects.
  # Gate 1 is judged from the raw COUNTS (n_eligible / n_datasets), not from a
  # derived `eligible_fraction` that could be mutated independently of them
  # (review round-5 critical 1). A stored fraction contradicting the counts is
  # rejected as tampering.
  need1 <- c("n_eligible", "n_datasets", "prereg_fingerprint")
  if (!is.list(gate1) || !all(need1 %in% names(gate1))) {
    stop("gate 1 is incomplete (missing evidence fields).", call. = FALSE)
  }
  if (!identical(gate1$prereg_fingerprint, fp)) {
    stop(
      "gate 1 was computed under a different pre-registration than this ",
      "report's (fingerprint mismatch); recompute it under the same prereg.",
      call. = FALSE
    )
  }
  ne <- gate1$n_eligible; nd <- gate1$n_datasets
  eval_check_count(ne, "n_eligible")
  eval_check_count(nd, "n_datasets")
  if (ne > nd) {
    stop("gate 1 n_eligible cannot exceed n_datasets.", call. = FALSE)
  }
  frac_rc <- if (nd > 0) ne / nd else NA_real_
  if (!is.null(gate1$eligible_fraction) && !is.na(gate1$eligible_fraction) &&
      !isTRUE(all.equal(gate1$eligible_fraction, frac_rc))) {
    stop("gate 1 stored eligible_fraction contradicts its counts.", call. = FALSE)
  }
  gate1_pass <- eval_gate1_decision(frac_rc, prereg)

  have_gate2 <- !is.null(gate2)
  gate2_pass <- FALSE
  gate2_decision <- NULL
  if (have_gate2) {
    # Gate 2 recomputes the gain from the raw detector/baseline recoveries, not
    # a stored `recovery_gain` (round-5 critical 1); a contradiction is rejected.
    need2 <- c("detector_recovery", "baseline_recovery", "merge_rate",
               "fragment_rate", "orientation_coverage", "prereg_fingerprint")
    if (!is.list(gate2) || !all(need2 %in% names(gate2))) {
      stop("gate 2 is incomplete (missing evidence fields).", call. = FALSE)
    }
    if (!identical(gate2$prereg_fingerprint, fp)) {
      stop(
        "gate 2 was computed under a different pre-registration than this ",
        "report's (fingerprint mismatch); recompute it under the same prereg.",
        call. = FALSE
      )
    }
    eval_check_unit_scalar(gate2$detector_recovery, "detector_recovery")
    eval_check_unit_scalar(gate2$baseline_recovery, "baseline_recovery")
    gain_rc <- gate2$detector_recovery - gate2$baseline_recovery
    if (!is.null(gate2$recovery_gain) && !is.na(gate2$recovery_gain) &&
        !isTRUE(all.equal(gate2$recovery_gain, gain_rc))) {
      stop("gate 2 stored recovery_gain contradicts detector/baseline recovery.",
        call. = FALSE)
    }
    gate2_decision <- eval_gate2_decision(
      gain_rc, gate2$merge_rate, gate2$fragment_rate,
      gate2$orientation_coverage, prereg
    )
    gate2_pass <- gate2_decision$pass
  }

  authorized <- isTRUE(gate1_pass) && have_gate2 && isTRUE(gate2_pass)
  structure(
    list(
      preregistration = prereg,
      gate1_use_case = gate1,
      gate2_residual_performance = gate2,
      # Authoritative, RECOMPUTED decisions AND derived values (never the
      # gates' cached/optional fields, which may be absent or tampered).
      gate1_pass = gate1_pass,
      gate1_eligible_fraction = frac_rc,
      gate2_pass = if (have_gate2) gate2_pass else NA,
      gate2_coverage_gate = if (have_gate2) gate2_decision$coverage_gate else NA,
      gate2_recovery_gain = if (have_gate2) gain_rc else NA_real_,
      gate2_orientation_coverage =
        if (have_gate2) gate2$orientation_coverage else NA_real_,
      build_authorized = authorized,
      note = if (authorized) {
        "Both gates pass: building beyond the simple baseline is authorized."
      } else if (!have_gate2) {
        "Gate 2 not yet evaluated (needs held-out detector vs baseline runs)."
      } else {
        "At least one gate fails: only the simple baseline is justified."
      }
    ),
    class = "g2g_eval_gate_report"
  )
}

#' @exportS3Method
print.g2g_eval_gate_report <- function(x, ...) {
  # Thresholds are printed from the LIVE pre-registration used for the
  # authoritative decisions, never from the mutable gate objects' cached copies
  # (review round-5). PASS/FAIL are the report's recomputed decisions.
  pr <- x$preregistration
  cat("<g2g eval gate report>\n")
  cat("  pre-registered:", pr$frozen_on, "\n")
  cat(sprintf(
    "  gate 1 (use-case): eligible=%.3f  threshold=%.3f  %s\n",
    x$gate1_eligible_fraction %||% NA_real_,   # report's recomputed value
    pr$gates$gate1_min_eligible_fraction,
    if (isTRUE(x$gate1_pass)) "PASS" else "FAIL"
  ))
  if (is.null(x$gate2_residual_performance)) {
    cat("  gate 2 (residual): not evaluated\n")
  } else {
    cat(sprintf(
      "  gate 2 (residual): gain=%.3f  coverage=%.3f(>=%.2f:%s)  %s\n",
      x$gate2_recovery_gain %||% NA_real_,          # recomputed
      x$gate2_orientation_coverage %||% NA_real_,   # raw evidence
      pr$min_orientation_coverage,
      if (isTRUE(x$gate2_coverage_gate)) "ok" else "FAIL",
      if (isTRUE(x$gate2_pass)) "PASS" else "FAIL"
    ))
  }
  cat("  build authorized:", x$build_authorized, "\n")
  cat("  ", x$note, "\n", sep = "")
  invisible(x)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a)) b else a
