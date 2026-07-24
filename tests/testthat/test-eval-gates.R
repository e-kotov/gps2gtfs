# The two authorization gates and the pre-registration audit trail
# (R/eval_gates.R; spike §4, §6e). No detector is authorized until BOTH gates
# report pass against a frozen, dated registration - and gate 2 now enforces
# the frozen minimum orientation coverage.

test_that("pre-registration must be dated (the audit trail)", {
  expect_error(eval_preregister(), "frozen_on")
  pr <- eval_preregister(frozen_on = "2026-07-24")
  expect_s3_class(pr, "g2g_eval_preregistration")
  expect_identical(pr$iou_thresholds, c(0.5, 0.8))
  expect_identical(pr$fragment_convention, "once_via_assigned_segment")
  expect_equal(pr$min_orientation_coverage, 0.7)
})

test_that("leakage-safe splits carve calibration / held-out / transfer", {
  dat <- data.table::data.table(
    agency = c(rep("A", 6), rep("B", 3)),
    service_day = c(rep(c("d1", "d1", "d2"), 2), rep("d9", 3)),
    x = 1:9
  )
  sp <- eval_make_splits(
    dat, agency_a = "A", agency_b = "B",
    period_col = "service_day", calibration_periods = "d1"
  )
  expect_s3_class(sp, "g2g_eval_splits")
  expect_true(all(sp$calibration$service_day == "d1"))
  expect_true(all(sp$heldout$service_day == "d2"))
  expect_true(sp$transfer_claim_supported)
  sp2 <- eval_make_splits(dat, agency_a = "A",
    period_col = "service_day", calibration_periods = "d1")
  expect_false(sp2$transfer_claim_supported)
})

test_that("a transfer claim cannot reuse the calibration agency", {
  dat <- data.table::data.table(agency = "A", split = c("calibration", "heldout"))
  expect_error(
    eval_make_splits(dat, agency_a = "A", agency_b = "A"),
    "must differ"
  )
})

test_that("gate 1 does not invent eligibility from unknown (NA) datasets", {
  pr <- eval_preregister(frozen_on = "2026-07-24",
                         gate1_min_eligible_fraction = 0.2)
  # 1 TRUE + 9 NA must be 0.1 (NA counted as NOT eligible), NOT 1.0.
  expect_warning(
    g1 <- eval_gate1_use_case(c(TRUE, rep(NA, 9)), pr),
    "unknown eligibility"
  )
  expect_equal(g1$eligible_fraction, 0.1)
  expect_equal(g1$n_unknown, 9L)
  expect_false(g1$pass)
  # Clean case: 1 of 5 eligible -> 0.2 -> passes at threshold 0.2.
  g1b <- eval_gate1_use_case(c(TRUE, FALSE, FALSE, FALSE, FALSE), pr)
  expect_equal(g1b$eligible_fraction, 0.2)
  expect_true(g1b$pass)
})

test_that("gate 2 requires orientation_coverage and enforces the frozen floor", {
  pr <- eval_preregister(frozen_on = "2026-07-24",
    gate2_min_residual_recovery_gain = 0.1,
    gate2_max_merge_rate = 0.2, gate2_max_fragment_rate = 0.2,
    min_orientation_coverage = 0.7)
  # Omitting coverage is an error (it is part of the gate).
  expect_error(
    eval_gate2_residual(0.8, 0.6, 0.05, 0.1, prereg = pr),
    "orientation_coverage"
  )
  # Good gain + rates + coverage -> pass.
  g2 <- eval_gate2_residual(0.8, 0.6, merge_rate = 0.05, fragment_rate = 0.1,
    orientation_coverage = 0.85, prereg = pr)
  expect_true(g2$pass)
  # Coverage below the frozen floor fails EVEN with a big gain and low rates -
  # this is the review's critical issue #1.
  g2_low_cov <- eval_gate2_residual(0.8, 0.6, merge_rate = 0.05,
    fragment_rate = 0.1, orientation_coverage = 0.69, prereg = pr)
  expect_false(g2_low_cov$coverage_gate)
  expect_false(g2_low_cov$pass)
  # Merge rate over cap fails.
  expect_false(eval_gate2_residual(0.8, 0.6, merge_rate = 0.5,
    fragment_rate = 0.1, orientation_coverage = 0.85, prereg = pr)$pass)
  # Trivial gain fails (simple baseline suffices).
  expect_false(eval_gate2_residual(0.62, 0.6, merge_rate = 0.05,
    fragment_rate = 0.1, orientation_coverage = 0.85, prereg = pr)$pass)
})

test_that("gate report authorizes a build ONLY when both gates fully pass", {
  pr <- eval_preregister(frozen_on = "2026-07-24")
  g1_pass <- eval_gate1_use_case(c(TRUE, FALSE, FALSE, FALSE, FALSE), pr)
  g2_pass <- eval_gate2_residual(0.8, 0.6, 0.05, 0.1,
    orientation_coverage = 0.85, prereg = pr)

  rep_partial <- eval_gate_report(g1_pass, gate2 = NULL, prereg = pr)
  expect_false(rep_partial$build_authorized)
  expect_match(rep_partial$note, "Gate 2 not yet evaluated")

  rep_full <- eval_gate_report(g1_pass, g2_pass, pr)
  expect_true(rep_full$build_authorized)

  # Coverage-only failure blocks authorization (critical issue #1, end-to-end).
  g2_low_cov <- eval_gate2_residual(0.8, 0.6, 0.05, 0.1,
    orientation_coverage = 0.69, prereg = pr)
  expect_false(eval_gate_report(g1_pass, g2_low_cov, pr)$build_authorized)

  # The S3 print method is registered, so generic dispatch works.
  expect_output(print(rep_full), "build authorized: TRUE")
  # Thresholds print from the LIVE prereg, not a mutated gate copy.
  tampered_rep <- rep_full
  tampered_rep$gate1_use_case$threshold <- 0.999
  expect_output(print(tampered_rep), "threshold=0.200")
})

test_that("gate 1 rejects non-logical eligibility (no numeric coercion)", {
  pr <- eval_preregister(frozen_on = "2026-07-24")
  # Review round-5 critical 2: as.logical(2) == TRUE must not sneak through.
  expect_error(eval_gate1_use_case(2, pr), "logical")
  expect_error(eval_gate1_use_case(c(1L, 0L, 1L), pr), "logical")
  expect_error(eval_gate1_use_case(c("yes", "no"), pr), "logical")
  expect_error(
    eval_gate1_use_case(data.frame(eligible = c(2, 0)), pr), "logical"
  )
  # A genuine logical still works.
  expect_silent(g <- suppressWarnings(eval_gate1_use_case(c(TRUE, FALSE), pr)))
  expect_equal(g$eligible_fraction, 0.5)
})

test_that("report rejects derived evidence mutated inconsistent with sources", {
  # Review round-5 critical 1: FAIL-to-PASS mutation of a derived summary must
  # be caught by recomputing from the source fields.
  pr <- eval_preregister(frozen_on = "2026-07-24",
    gate1_min_eligible_fraction = 0.2,
    gate2_min_residual_recovery_gain = 0.1, min_orientation_coverage = 0.7)
  g1_ok <- eval_gate1_use_case(c(TRUE, FALSE, FALSE, FALSE, FALSE), pr)
  g2_ok <- eval_gate2_residual(0.8, 0.6, 0.05, 0.1,
    orientation_coverage = 0.85, prereg = pr)

  # Gate 1: 0/5 eligible (fails), but eligible_fraction hand-set to 1.
  g1_fail <- eval_gate1_use_case(rep(FALSE, 5), pr)
  g1_frac_mut <- g1_fail; g1_frac_mut$eligible_fraction <- 1
  expect_error(eval_gate_report(g1_frac_mut, g2_ok, pr), "contradicts its counts")

  # Gate 2: detector 0.61 / baseline 0.60 (gain 0.01, fails), but recovery_gain
  # retained/hand-set to 0.20.
  g2_fail <- eval_gate2_residual(0.61, 0.60, 0.05, 0.1,
    orientation_coverage = 0.85, prereg = pr)
  g2_gain_mut <- g2_fail; g2_gain_mut$recovery_gain <- 0.20
  expect_error(
    eval_gate_report(g1_ok, g2_gain_mut, pr),
    "contradicts detector/baseline"
  )

  # Missing counts / recoveries fail closed (incomplete), not authorize.
  g1_nocounts <- g1_ok; g1_nocounts$n_eligible <- NULL
  expect_error(eval_gate_report(g1_nocounts, g2_ok, pr), "incomplete")
  g2_norec <- g2_ok; g2_norec$detector_recovery <- NULL
  expect_error(eval_gate_report(g1_ok, g2_norec, pr), "incomplete")
})

test_that("gate 1 rejects non-whole / non-finite / non-scalar counts", {
  # Review round-6 critical: a fraction of a dataset is impossible evidence.
  pr <- eval_preregister(frozen_on = "2026-07-24",
    gate1_min_eligible_fraction = 0.2, min_orientation_coverage = 0.7,
    gate2_min_residual_recovery_gain = 0.1)
  g2_ok <- eval_gate2_residual(0.8, 0.6, 0.05, 0.1,
    orientation_coverage = 0.85, prereg = pr)
  g1 <- eval_gate1_use_case(c(TRUE, FALSE, FALSE, FALSE, FALSE), pr)  # 1/5

  frac <- g1; frac$n_eligible <- 0.2; frac$n_datasets <- 1
  frac$eligible_fraction <- 0.2
  expect_error(eval_gate_report(frac, g2_ok, pr), "whole")

  inf <- g1; inf$n_datasets <- Inf
  expect_error(eval_gate_report(inf, g2_ok, pr), "whole")

  vec <- g1; vec$n_eligible <- c(1L, 2L)
  expect_error(eval_gate_report(vec, g2_ok, pr), "whole")

  over <- g1; over$n_eligible <- 6; over$n_datasets <- 5
  expect_error(eval_gate_report(over, g2_ok, pr), "cannot exceed")

  # Valid double-encoded whole counts (1.0 / 5.0) are accepted.
  dbl <- g1; dbl$n_eligible <- 1.0; dbl$n_datasets <- 5.0
  expect_true(eval_gate_report(dbl, g2_ok, pr)$build_authorized)
})

test_that("print uses the report's recomputed derived values, not cached ones", {
  # Review round-6 required: with the optional derived fields absent, the
  # printer must still show the authoritative recomputed fraction/gain.
  pr <- eval_preregister(frozen_on = "2026-07-24",
    gate1_min_eligible_fraction = 0.2, min_orientation_coverage = 0.7,
    gate2_min_residual_recovery_gain = 0.1)
  g1 <- eval_gate1_use_case(c(TRUE, FALSE, FALSE, FALSE, FALSE), pr)  # 0.2
  g2 <- eval_gate2_residual(0.8, 0.6, 0.05, 0.1,
    orientation_coverage = 0.85, prereg = pr)                        # gain 0.2
  g1$eligible_fraction <- NULL
  g2$recovery_gain <- NULL
  rep <- eval_gate_report(g1, g2, pr)
  expect_true(rep$build_authorized)
  out <- capture.output(print(rep))
  expect_true(any(grepl("eligible=0.200", out)))
  expect_false(any(grepl("eligible=NA", out)))
  expect_true(any(grepl("gain=0.200", out)))
})

test_that("gate report rejects gates computed under a different registration", {
  # Review critical issue #3: lenient gates must not be laundered through a
  # report carrying the frozen strict registration.
  lenient <- eval_preregister(frozen_on = "2026-07-24",
    gate1_min_eligible_fraction = 0.0,
    gate2_min_residual_recovery_gain = 0.0,
    min_orientation_coverage = 0.0)
  strict <- eval_preregister(frozen_on = "2026-07-24",
    gate1_min_eligible_fraction = 0.2,
    gate2_min_residual_recovery_gain = 0.1,
    min_orientation_coverage = 0.7)
  expect_false(identical(lenient$fingerprint, strict$fingerprint))

  g1 <- eval_gate1_use_case(c(TRUE, rep(FALSE, 9)), lenient)  # 0.1, passes lenient
  g2 <- eval_gate2_residual(0.61, 0.6, 0.05, 0.1,
    orientation_coverage = 0.5, prereg = lenient)             # passes lenient
  expect_true(g1$pass); expect_true(g2$pass)

  # Combining them under the STRICT report must error, not report TRUE.
  expect_error(eval_gate_report(g1, g2, strict), "different pre-registration")
})

test_that("a prereg mutated after freezing is rejected (stale fingerprint)", {
  # Review critical issue #1: copy a strict registration, mutate the copy's
  # thresholds to lenient values (leaving the stale strict fingerprint), and
  # try to authorize through it. The stored fingerprint no longer matches the
  # fields, so every gate call must reject it.
  strict <- eval_preregister(frozen_on = "2026-07-24",
    gate2_min_residual_recovery_gain = 0.1, min_orientation_coverage = 0.7)
  tampered <- strict
  tampered$gates$gate2_min_residual_recovery_gain <- 0.0
  tampered$min_orientation_coverage <- 0.0
  # (fingerprint field left stale on purpose)
  expect_error(eval_gate1_use_case(c(TRUE), tampered), "mutated after freezing")
  expect_error(
    eval_gate2_residual(0.61, 0.6, 0.05, 0.1, orientation_coverage = 0.5,
      prereg = tampered),
    "mutated after freezing"
  )
})

test_that("report recomputes decisions from evidence, ignoring cached $pass", {
  # Review round-4 blocking 1: the report must not trust mutable $pass fields.
  pr <- eval_preregister(frozen_on = "2026-07-24",
    gate1_min_eligible_fraction = 0.2,
    gate2_min_residual_recovery_gain = 0.1, min_orientation_coverage = 0.7)

  # Two LEGITIMATELY FAILING gates (evidence below thresholds).
  g1_fail <- eval_gate1_use_case(rep(FALSE, 10), pr)   # 0.0 eligible
  g2_fail <- eval_gate2_residual(0.61, 0.6, 0.05, 0.1,
    orientation_coverage = 0.5, prereg = pr)           # coverage below floor
  expect_false(g1_fail$pass); expect_false(g2_fail$pass)

  # Flip both cached $pass to TRUE - the report must STILL not authorize,
  # because it recomputes from evidence.
  g1_flip <- g1_fail; g1_flip$pass <- TRUE
  g2_flip <- g2_fail; g2_flip$pass <- TRUE; g2_flip$coverage_gate <- TRUE
  rep <- eval_gate_report(g1_flip, g2_flip, pr)
  expect_false(rep$build_authorized)
  expect_false(rep$gate1_pass)
  expect_false(rep$gate2_pass)

  # Mutating evidence after generation is re-judged: push merge_rate over cap
  # while leaving pass = TRUE -> recomputed FAIL.
  g2_ok <- eval_gate2_residual(0.8, 0.6, 0.05, 0.1,
    orientation_coverage = 0.85, prereg = pr)
  g1_ok <- eval_gate1_use_case(c(TRUE, FALSE, FALSE, FALSE, FALSE), pr)
  g2_bad_ev <- g2_ok; g2_bad_ev$merge_rate <- 0.9  # over the 0.2 cap
  expect_false(eval_gate_report(g1_ok, g2_bad_ev, pr)$build_authorized)

  # Tampering with embedded thresholds/coverage flag has no effect - the report
  # uses the LIVE prereg, not the gate's cached copy.
  g2_tampered <- g2_fail
  g2_tampered$thresholds$gate2_min_residual_recovery_gain <- 0
  g2_tampered$coverage_gate <- TRUE
  g2_tampered$pass <- TRUE
  expect_false(eval_gate_report(g1_ok, g2_tampered, pr)$build_authorized)

  # A fabricated minimal gate object (no evidence) fails closed.
  fake <- list(pass = TRUE, prereg_fingerprint = pr$fingerprint)
  expect_error(eval_gate_report(fake, g2_ok, pr), "incomplete")
  expect_error(eval_gate_report(g1_ok, fake, pr), "incomplete")

  # Sanity: genuinely passing evidence still authorizes.
  expect_true(eval_gate_report(g1_ok, g2_ok, pr)$build_authorized)
})

test_that("gate 2 fails closed on malformed (out-of-range) evidence", {
  pr <- eval_preregister(frozen_on = "2026-07-24")
  # Impossible values must error, never authorize.
  expect_error(
    eval_gate2_residual(detector_recovery = 2, baseline_recovery = 0.6,
      merge_rate = -1, fragment_rate = -1, orientation_coverage = 2, prereg = pr),
    "\\[0, 1\\]"
  )
  expect_error(
    eval_gate2_residual(0.8, 0.6, c(0.1, 0.2), 0.1, orientation_coverage = 0.9,
      prereg = pr),
    "\\[0, 1\\]"  # non-scalar
  )
})
