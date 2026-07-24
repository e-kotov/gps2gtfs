# Exercises the pre-registered evaluation harness (R/eval_harness.R,
# R/eval_gates.R) against the synthetic fixtures. The harness is frozen BEFORE
# any detector exists; these tests pin its behaviour so method selection cannot
# later move the goalposts. No detector is implemented or evaluated here.

test_that("exact assignment beats greedy on a blocking-edge matrix", {
  # Greedy would take the single big edge (0.5) and lose the optimal two-pair
  # assignment (0.45 + 0.45 = 0.9). The exact solver must find both pairs.
  w <- matrix(c(0.5, 0.45, 0.45, 0), nrow = 2, byrow = TRUE)
  asg <- eval_assign_max_weight(w)
  expect_equal(nrow(asg), 2L)
  total <- sum(w[cbind(asg[, "row"], asg[, "col"])])
  expect_equal(total, 0.9, tolerance = 1e-9)
  # It pairs off-diagonal (1->2, 2->1), not the greedy diagonal pick.
  expect_setequal(asg[, "col"], c(1L, 2L))
})

test_that("temporal IoU is midnight-safe (absolute time, no service-day split)", {
  a0 <- as.POSIXct("2026-07-14 23:40", tz = "UTC")
  a1 <- as.POSIXct("2026-07-15 00:20", tz = "UTC")
  b0 <- as.POSIXct("2026-07-14 23:41", tz = "UTC")
  b1 <- as.POSIXct("2026-07-15 00:19", tz = "UTC")
  iou <- eval_temporal_iou(a0, a1, b0, b1)
  expect_gt(iou, 0.9)
  expect_lte(iou, 1)
})

test_that("segment matching separates split (fragment) from merge and flags clean", {
  segs <- make_eval_segments()
  mm <- eval_match_segments(segs$pred, segs$truth)
  expect_gte(mm$splits, 1L)   # R3 fragment
  expect_gte(mm$merges, 1L)   # R2 merge
  # Overnight truth is matched 1:1 and CLEAN.
  night <- mm$matches[truth_id == "T7"]
  expect_gt(night$iou, 0.9)
  expect_true(night$clean)
  # The merged/fragmented matches are flagged NOT clean.
  expect_false(all(mm$matches$clean))
  expect_true(any(mm$matches$route_id %in% c("R2", "R3") & !mm$matches$clean))
})

test_that("segment metrics: weighted micro, rates with denominators, coverage, CI", {
  segs <- make_eval_segments()
  res <- eval_segment_metrics(segs$pred, segs$truth, iou_threshold = 0.5,
                              n_boot = 50L)
  expect_true(all(c("precision", "recall", "f1") %in% names(res$micro)))
  expect_true(all(c("precision", "recall", "f1") %in% names(res$micro_unweighted)))
  # Weighted micro differs from the unweighted counts (genuine weighting).
  expect_false(isTRUE(all.equal(res$micro, res$micro_unweighted)))
  # Rates are proportions with defined denominators.
  expect_gte(res$fragment_rate, 0); expect_lte(res$fragment_rate, 1)
  expect_gte(res$merge_rate, 0); expect_lte(res$merge_rate, 1)
  expect_true(is.finite(res$duration_coverage))
  expect_true(is.finite(res$unassigned_duration))
  expect_length(res$f1_ci, 2L)
})

test_that("orientation: per-route mapping + coverage over clean matches", {
  m <- make_eval_orientation_matched()
  res <- eval_orientation(m, n_truth_total = 7L, min_coverage = 0.7)
  expect_equal(res$coverage, 5 / 7, tolerance = 1e-8)  # over clean 1:1
  expect_true(res$min_coverage_gate)
  expect_false(eval_orientation(m, n_truth_total = 7L,
                                min_coverage = 0.75)$min_coverage_gate)
  expect_equal(res$balanced_accuracy, 1, tolerance = 1e-8)
  expect_equal(unname(res$conditional_accuracy), 1, tolerance = 1e-8)
  expect_s3_class(res$risk_coverage, "data.table")
  expect_true(all(diff(res$risk_coverage$coverage) > 0))
  expect_false(is.null(res$calibration))
  expect_true(is.finite(res$calibration$ece))
})

test_that("end-to-end recovery counts merged/fragmented trips as UNRECOVERED", {
  # The critical fix: a merged pred (R2) and a fragmented truth (R3) must NOT
  # count as recovered. Build matched from real matches so `clean` is present.
  segs <- make_eval_segments()
  mm <- eval_match_segments(segs$pred, segs$truth)
  m <- mm$matches
  m[segs$pred, pred_orientation := i.orientation_id,
    on = c(pred_id = "seg_id")]
  m[segs$truth, truth_direction := i.direction_id,
    on = c(truth_id = "seg_id")]
  res <- eval_orientation(m, n_truth_total = nrow(segs$truth))
  # 4 clean, correctly-oriented trips (R1 x3 + overnight); the 2 merged (R2)
  # and 1 fragmented (R3) truths are excluded -> 4/7, NOT 6/7 or 7/7.
  expect_equal(res$end_to_end_recovery, 4 / 7, tolerance = 1e-8)
  expect_equal(res$n_clean, 4L)
  expect_lt(res$end_to_end_recovery, res$conditional_accuracy)
  # No R2/R3 truth appears among the clean matches.
  clean_truths <- m$truth_id[m$clean]
  expect_false(any(c("T4", "T5", "T6") %in% clean_truths))
})

test_that("orientation FAILS CLOSED when `clean` is absent (end-to-end mode)", {
  m <- make_eval_orientation_matched()
  m$clean <- NULL
  # A primary gate metric must not fail open: end-to-end mode errors.
  expect_error(eval_orientation(m, n_truth_total = 7L), "no `clean` column")
  # conditional-only is the explicit opt-in; it reports NA end-to-end.
  res <- eval_orientation(m, n_truth_total = 7L, mode = "conditional_only")
  expect_true(is.na(res$end_to_end_recovery))
  expect_equal(unname(res$conditional_accuracy), 1, tolerance = 1e-8)
})

test_that("exact matcher scales to many independent trips (no combinatorial cap)", {
  # 25 non-overlapping, perfectly matched trips over a continuous service day
  # (5-min gaps) stay one session but must NOT blow up the solver: connected-
  # component decomposition makes them 25 trivial 1x1 problems.
  n <- 25L
  base <- as.POSIXct("2026-07-14 06:00", tz = "UTC")
  starts <- base + (seq_len(n) - 1L) * (30L * 60L)  # 30-min cadence
  pred <- data.table::data.table(
    seg_id = paste0("P", seq_len(n)), vehicle_id = "V1", route_id = "R1",
    start = starts + 60, end = starts + 25 * 60 - 60
  )
  truth <- data.table::data.table(
    seg_id = paste0("T", seq_len(n)), vehicle_id = "V1", route_id = "R1",
    start = starts, end = starts + 25 * 60
  )
  expect_error(mm <- eval_match_segments(pred, truth), NA)  # no error/blowup
  expect_equal(nrow(mm$matches), n)
  expect_true(all(mm$matches$clean))
})

test_that("segment matching rejects non-unique seg_id (silent-collapse guard)", {
  segs <- make_eval_segments()
  bad <- data.table::copy(segs$pred)
  bad$seg_id[2] <- bad$seg_id[1]  # duplicate id across routes
  expect_error(eval_match_segments(bad, segs$truth), "globally unique")
})

test_that("cluster bootstrap keeps perfect data perfect (independent replicas)", {
  # Two routes, each a single perfect 1:1 match. Every resample of perfect data
  # must stay F1 = 1; a naive resample that collapsed duplicate ids gave 0.5.
  base <- as.POSIXct("2026-07-14 08:00", tz = "UTC")
  seg <- function(pfx, r) data.table::data.table(
    seg_id = paste0(pfx, r), vehicle_id = "V1", route_id = r,
    start = base, end = base + 1800
  )
  pred <- rbind(seg("P", "R1"), seg("P", "R2"))
  truth <- rbind(seg("T", "R1"), seg("T", "R2"))
  res <- eval_segment_metrics(pred, truth, n_boot = 100L)
  expect_equal(unname(res$micro["f1"]), 1, tolerance = 1e-8)
  expect_equal(unname(res$f1_ci[1]), 1, tolerance = 1e-8)  # lower CI stays 1
})

test_that("weighted metrics require globally unique seg_id (validated upstream)", {
  # Repeated ids across routes must be rejected, not silently down-weighted.
  base <- as.POSIXct("2026-07-14 08:00", tz = "UTC")
  mk <- function(r) data.table::data.table(
    seg_id = "S1", vehicle_id = "V1", route_id = r, start = base, end = base + 1800
  )
  pred <- rbind(mk("R1"), mk("R2")); truth <- rbind(mk("R1"), mk("R2"))
  expect_error(eval_segment_metrics(pred, truth), "globally unique")
})

test_that("matching rejects missing/empty seg_id (NA-id weight-NA guard)", {
  base <- as.POSIXct("2026-07-14 08:00", tz = "UTC")
  pred <- data.table::data.table(seg_id = NA_character_, vehicle_id = "V1",
    route_id = "R1", start = base, end = base + 1800)
  truth <- data.table::data.table(seg_id = "T1", vehicle_id = "V1",
    route_id = "R1", start = base, end = base + 1800)
  expect_error(eval_match_segments(pred, truth), "missing or empty")
  pred$seg_id <- "  "  # blank
  expect_error(eval_match_segments(pred, truth), "missing or empty")
})

test_that("false-positive-only routes lower macro and widen the bootstrap CI", {
  # Two perfect truth routes + one route with ONLY a false-positive prediction
  # (no true trip). Point F1 must drop below 1 and the FP route must be
  # represented in macro and the resample (review required-change 1).
  base <- as.POSIXct("2026-07-14 08:00", tz = "UTC")
  seg <- function(pfx, r) data.table::data.table(
    seg_id = paste0(pfx, r), vehicle_id = "V1", route_id = r,
    start = base, end = base + 1800
  )
  pred <- rbind(seg("P", "R1"), seg("P", "R2"), seg("P", "R3fp"))
  truth <- rbind(seg("T", "R1"), seg("T", "R2"))  # no R3fp truth
  res <- eval_segment_metrics(pred, truth, n_boot = 100L)
  expect_lt(unname(res$micro["f1"]), 1)            # FP route drags F1 down
  expect_lt(unname(res$f1_ci[1]), 1)               # CI no longer pinned at 1
  expect_true(is.finite(unname(res$macro["precision"])))
  # macro F1 must DROP: R1, R2 perfect (1) + R3fp (0) -> 2/3, not 1.
  expect_lt(unname(res$macro["f1"]), 1)
  expect_equal(unname(res$macro["f1"]), 2 / 3, tolerance = 1e-8)
})

test_that("false-negative-only routes also score macro F1 = 0 (not dropped)", {
  base <- as.POSIXct("2026-07-14 08:00", tz = "UTC")
  seg <- function(pfx, r) data.table::data.table(
    seg_id = paste0(pfx, r), vehicle_id = "V1", route_id = r,
    start = base, end = base + 1800
  )
  # R2fn is present in truth but has NO prediction -> false-negative only.
  pred <- seg("P", "R1")
  truth <- rbind(seg("T", "R1"), seg("T", "R2fn"))
  res <- eval_segment_metrics(pred, truth, n_boot = 20L)
  # R1 perfect (F1 1), R2fn one-sided (F1 0) -> macro F1 = 0.5.
  expect_equal(unname(res$macro["f1"]), 0.5, tolerance = 1e-8)
})

test_that("moving-segment median includes zero-movement routes", {
  base <- as.POSIXct("2026-07-14 05:00", tz = "UTC")
  # R1: one vehicle-day with two moving segments (a gap of stillness between).
  moving_leg <- function(t0, r, v) data.table::data.table(
    vehicle_id = v, route_id = r,
    timestamp = base + t0 + (0:9) * 60,
    latitude = 60.17 + (0:9) * 0.002, longitude = 24.94 + (0:9) * 0.002
  )
  still <- function(t0, r, v) data.table::data.table(
    vehicle_id = v, route_id = r,
    timestamp = base + t0 + (0:9) * 60,
    latitude = 60.30, longitude = 25.10   # parked: no movement
  )
  vp <- data.table::rbindlist(list(
    moving_leg(0, "R1", "v1"), still(700, "R1", "v1"), moving_leg(1400, "R1", "v1"),
    still(0, "R2", "v2")     # R2: stationary only -> zero moving segments
  ))
  res <- eval_data_sufficiency(vp)
  # Two routes: R1 has 2 moving segments, R2 has 0. Median must be 1, not 2.
  expect_equal(res$diagnostics$median_moving_segments_per_route, 1)
})

test_that("pattern ARI is permutation-invariant and penalises branch merging", {
  p <- make_eval_patterns()
  expect_equal(eval_pattern_ari(p$good, p$true), 1, tolerance = 1e-8)
  expect_lt(eval_pattern_ari(p$merged, p$true), 1)
})

test_that("anchor detection: precision/recall + min-support filter", {
  a <- make_eval_anchors()
  res <- eval_anchor_detection(a$discovered, a$truth, dist_tol = 150,
                               min_recurrence = 3L, min_coverage = 0.1)
  expect_equal(res$n_dropped_low_support, 1L)
  expect_equal(res$recall, 1, tolerance = 1e-8)
  expect_equal(res$tp, 2L)
  expect_lt(res$precision, 1)
})

test_that("terminal-stop association has an explicit unmatched state", {
  anchors <- data.table::data.table(lat = c(60.170, 60.400),
                                    lon = c(24.940, 25.500))
  stops <- data.table::data.table(stop_id = c("s1", "s2"),
    lat = c(60.1701, 60.250), lon = c(24.9402, 25.010))
  res <- eval_terminal_stop_association(anchors, stops, dist_tol = 100)
  expect_equal(res$stop_id[1], "s1")       # close anchor -> matched
  expect_true(is.na(res$stop_id[2]))        # far anchor -> explicit NA
  expect_false(res$matched[2])
})

test_that("data-sufficiency rejects single-ping vehicle-days", {
  # The review counterexample: 20 vehicles, ONE ping each, 12h overall span.
  base <- as.POSIXct("2026-07-14 06:00", tz = "UTC")
  vp <- data.table::data.table(
    vehicle_id = paste0("v", 1:20),
    route_id = "R1",
    timestamp = base + (0:19) * 3600,   # one ping per vehicle, spread over 19h
    latitude = 60.17, longitude = 24.94
  )
  res <- eval_data_sufficiency(vp, min_span_hours = 12, min_vehicle_days = 20)
  expect_equal(res$diagnostics$complete_vehicle_days, 0L)  # none are complete
  expect_false(res$sufficient)
})

test_that("data-sufficiency passes a genuinely rich dataset", {
  base <- as.POSIXct("2026-07-14 05:00", tz = "UTC")
  mk <- function(v) {
    n <- 60
    data.table::data.table(
      vehicle_id = v, route_id = "R1",
      timestamp = base + seq_len(n) * 300,           # 5h span, 60 pings
      latitude = 60.17 + seq_len(n) * 0.001,          # moves ~ km
      longitude = 24.94 + seq_len(n) * 0.001
    )
  }
  vp <- data.table::rbindlist(lapply(paste0("v", 1:25), mk))
  # Push the overall span past 12h by shifting later vehicles' days.
  vp[vehicle_id %in% paste0("v", 13:25), timestamp := timestamp + 8 * 3600]
  res <- eval_data_sufficiency(vp, min_span_hours = 12, min_vehicle_days = 20)
  expect_gte(res$diagnostics$complete_vehicle_days, 20L)
  expect_true(res$sufficient)
  # Moving-segments-per-route is actually computed (not just a returned
  # threshold): each moving vehicle-day contributes a moving segment on R1.
  expect_true(is.finite(res$diagnostics$median_moving_segments_per_route))
  expect_gte(res$diagnostics$median_moving_segments_per_route, 1)
  expect_false(is.na(res$diagnostics$routes_meeting_min_segments))
})
