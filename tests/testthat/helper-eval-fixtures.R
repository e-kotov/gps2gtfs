# Synthetic fixtures for the pre-registered evaluation harness
# (private/terminal-detection-spike.md §6, §9). Each exercises one hard case
# the two-terminal model cannot handle: merge, fragment, overnight,
# Y-branch/shared-trunk, one-way loop, and abstention. They are segment-level
# (not full GPS pipelines): predicted vs hidden-truth trip segments plus the
# orientation / pattern / anchor labels the harness scores against.

et <- function(x) as.POSIXct(x, tz = "UTC")

# Predicted and true trip segments covering clean 1:1, merge, fragment, and
# overnight. Columns match the harness segment contract. `orientation_id` on
# pred is INTENTIONALLY the per-route swap of the truth for R1 (publishers
# differ), so the harness's per-route mapping must recover it.
make_eval_segments <- function() {
  pred <- data.table::data.table(
    seg_id = c("P1", "P2", "P3", "Pmerge", "Pf1", "Pf2", "Pnight"),
    vehicle_id = c("V1", "V1", "V1", "V2", "V3", "V3", "V4"),
    route_id = c("R1", "R1", "R1", "R2", "R3", "R3", "R4"),
    start = et(c(
      "2026-07-14 08:00", "2026-07-14 08:40", "2026-07-14 09:20", # clean
      "2026-07-14 08:00",                                          # merge
      "2026-07-14 08:00", "2026-07-14 08:32",                      # fragment
      "2026-07-14 23:41"                                            # overnight
    )),
    end = et(c(
      "2026-07-14 08:30", "2026-07-14 09:10", "2026-07-14 09:50",
      "2026-07-14 09:05",
      "2026-07-14 08:28", "2026-07-14 09:00",
      "2026-07-15 00:19"
    )),
    orientation_id = c(1L, 0L, 1L, 0L, 0L, 0L, 0L),  # R1 swapped vs truth
    orientation_status = "ok"
  )
  truth <- data.table::data.table(
    seg_id = c("T1", "T2", "T3", "T4", "T5", "T6", "T7"),
    vehicle_id = c("V1", "V1", "V1", "V2", "V2", "V3", "V4"),
    route_id = c("R1", "R1", "R1", "R2", "R2", "R3", "R4"),
    start = et(c(
      "2026-07-14 08:00", "2026-07-14 08:40", "2026-07-14 09:20",
      "2026-07-14 08:00", "2026-07-14 08:35",  # two truths under one merge pred
      "2026-07-14 08:00",                       # one truth under two frag preds
      "2026-07-14 23:40"                         # overnight truth
    )),
    end = et(c(
      "2026-07-14 08:30", "2026-07-14 09:10", "2026-07-14 09:50",
      "2026-07-14 08:30", "2026-07-14 09:05",
      "2026-07-14 09:00",
      "2026-07-15 00:20"
    )),
    direction_id = c(0L, 1L, 0L, 0L, 1L, 0L, 0L)
  )
  list(pred = pred, truth = truth)
}

# An orientation "matched" table (as produced by joining eval_match_segments()
# output to labels) that also carries a one-way LOOP abstention (R5, pred
# orientation NA) and a genuine wrong call, plus confidences for the
# risk-coverage curve.
make_eval_orientation_matched <- function() {
  data.table::data.table(
    route_id = c("R1", "R1", "R1", "R2", "R2", "R5", "R5"),
    pred_orientation = c(1L, 0L, 1L, 0L, 1L, NA_integer_, NA_integer_),
    pred_status = c(rep("ok", 5), "single_group", "single_group"),
    pred_confidence = c(0.95, 0.90, 0.60, 0.80, 0.55, NA, NA),
    truth_direction = c(0L, 1L, 0L, 0L, 1L, 0L, 1L),  # R1 swapped
    clean = TRUE  # all clean 1:1 matches (the loop rows abstain, but are 1:1)
  )
}

# Y-branch / shared-trunk pattern labels: two branches (A, B) share a trunk.
# `true` = the two genuine patterns; `good` recovers them; `merged` collapses
# both branches into one cluster (the failure the stop-order spike inherits).
make_eval_patterns <- function() {
  true <- c("A", "A", "A", "B", "B", "B")
  list(
    true = true,
    good = c("c1", "c1", "c1", "c2", "c2", "c2"),
    merged = c("c1", "c1", "c1", "c1", "c1", "c1")
  )
}

# Turnaround-anchor discovery fixture: two true anchors; discovered clusters
# include a close match to each, one low-support noise cluster (filtered), and
# one far spurious cluster (a false positive).
make_eval_anchors <- function() {
  list(
    truth = data.table::data.table(
      lat = c(60.170, 60.250),
      lon = c(24.940, 25.010)
    ),
    discovered = data.table::data.table(
      lat = c(60.1701, 60.2499, 60.170, 60.400),
      lon = c(24.9402, 25.0101, 24.9401, 25.500),
      recurrence = c(40L, 35L, 1L, 12L),   # 3rd is low-support noise
      coverage = c(0.8, 0.7, 0.02, 0.4)    # -> filtered by min support
    )
  )
}
