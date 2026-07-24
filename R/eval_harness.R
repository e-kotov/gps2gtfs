# Pre-registered evaluation harness for baseline-free trip detection,
# orientation, pattern, and turnaround-anchor discovery
# (private/terminal-detection-spike.md §6). Built and frozen BEFORE any
# detector exists so method selection cannot move the goalposts: the metrics
# and gate thresholds are fixed here, once, and evaluated once on held-out data.
#
# These are internal research functions (not part of the public gps2gtfs API).
# They evaluate a detector's output against hidden ground-truth labels; they do
# NOT implement any detector. A fresh agent runs them via
# pkgload::load_all() and gps2gtfs:::eval_*.
#
# Segment tables passed in are data.tables/data.frames with, at minimum:
#   seg_id, vehicle_id, route_id, start (POSIXct), end (POSIXct)
# Predicted segments may also carry orientation_id (0/1/NA), orientation_status
# (never NA), orientation_confidence ([0,1]/NA), pattern_ref, n_pings.
# Truth segments carry direction_id (0/1) and optionally pattern.

# ---- Exact maximum-weight one-to-one assignment -----------------------------

# Hungarian (Kuhn-Munkres) algorithm for the square minimum-cost assignment
# problem, O(n^3). Returns, for each row, its assigned column (1-based).
# Validated against brute force on random matrices. Polynomial, so it does not
# blow up on a full service day (unlike the previous exponential subset DP).
eval_hungarian_min <- function(a) {
  n <- nrow(a)
  if (n == 0L) return(integer(0))
  INF <- .Machine$double.xmax / 4
  u <- numeric(n + 1L); v <- numeric(n + 1L)
  p <- integer(n + 1L); way <- integer(n + 1L)
  for (i in seq_len(n)) {
    p[1L] <- i
    j0 <- 1L
    minv <- rep(INF, n + 1L); used <- rep(FALSE, n + 1L)
    repeat {
      used[j0] <- TRUE
      i0 <- p[j0]; delta <- INF; j1 <- -1L
      for (j in seq_len(n)) {
        jj <- j + 1L
        if (!used[jj]) {
          cur <- a[i0, j] - u[i0 + 1L] - v[jj]
          if (cur < minv[jj]) { minv[jj] <- cur; way[jj] <- j0 }
          if (minv[jj] < delta) { delta <- minv[jj]; j1 <- jj }
        }
      }
      for (jj in seq_len(n + 1L)) {
        if (used[jj]) {
          u[p[jj] + 1L] <- u[p[jj] + 1L] + delta; v[jj] <- v[jj] - delta
        } else {
          minv[jj] <- minv[jj] - delta
        }
      }
      j0 <- j1
      if (p[j0] == 0L) break
    }
    repeat { j1 <- way[j0]; p[j0] <- p[j1]; j0 <- j1; if (j0 == 1L) break }
  }
  asg <- integer(n)
  for (jj in 2L:(n + 1L)) if (p[jj] >= 1L) asg[p[jj]] <- jj - 1L
  asg
}

# Connected components of the bipartite positive-edge graph (rows + cols as
# nodes, union-find). Independent matches fall into their own tiny component,
# so a full service day of non-overlapping trips never forms one giant matrix.
eval_bipartite_components <- function(pos) {
  nr <- nrow(pos); nc <- ncol(pos)
  parent <- seq_len(nr + nc)
  find <- function(x) { while (parent[x] != x) { parent[x] <- parent[parent[x]]; x <- parent[x] }; x }
  edges <- which(pos, arr.ind = TRUE)
  for (k in seq_len(nrow(edges))) {
    ra <- find(edges[k, 1L]); rb <- find(nr + edges[k, 2L])
    if (ra != rb) parent[rb] <- ra
  }
  active_rows <- which(rowSums(pos) > 0)
  active_cols <- which(colSums(pos) > 0)
  if (!length(active_rows)) return(list())
  rroot <- vapply(active_rows, find, integer(1))
  croot <- vapply(active_cols, function(c) find(nr + c), integer(1))
  lapply(unique(c(rroot, croot)), function(root) list(
    rows = active_rows[rroot == root], cols = active_cols[croot == root]
  ))
}

# Exact max-weight assignment on one connected component: pad to square with
# 0-weight (= unmatched) edges, convert max->min, solve with Hungarian, keep
# only in-range pairs with positive original weight.
eval_assign_component <- function(w) {
  nr <- nrow(w); nc <- ncol(w)
  n <- max(nr, nc)
  maxw <- max(w)
  W <- matrix(0, n, n)
  W[seq_len(nr), seq_len(nc)] <- w
  asg <- eval_hungarian_min(maxw - W)
  pairs <- list()
  for (i in seq_len(nr)) {
    j <- asg[i]
    if (j <= nc && w[i, j] > 0) pairs[[length(pairs) + 1L]] <- c(i, j)
  }
  if (!length(pairs)) return(matrix(integer(0), ncol = 2L))
  do.call(rbind, pairs)
}

# Exact max-weight bipartite matching on positive-overlap edges only, allowing
# rows/cols to stay unmatched. Decomposes the positive-edge graph into connected
# components and solves each exactly (trivial 1x1 components directly; larger
# ones via the polynomial Hungarian solver). Greedy is NOT used (review rejected
# it: a greedy single-edge pick can lose an optimal two-pair assignment).
# Returns an integer matrix with columns (row, col) indexing into `w`.
eval_assign_max_weight <- function(w) {
  empty <- matrix(integer(0), ncol = 2L, dimnames = list(NULL, c("row", "col")))
  if (length(w) == 0L) return(empty)
  pos <- w > 0
  if (!any(pos)) return(empty)
  out <- list()
  for (cp in eval_bipartite_components(pos)) {
    rs <- cp$rows; cs <- cp$cols
    if (length(rs) == 1L && length(cs) == 1L) {
      out[[length(out) + 1L]] <- c(rs, cs)  # trivial independent 1:1 match
      next
    }
    asg <- eval_assign_component(w[rs, cs, drop = FALSE])
    for (k in seq_len(nrow(asg))) {
      out[[length(out) + 1L]] <- c(rs[asg[k, 1L]], cs[asg[k, 2L]])
    }
  }
  if (!length(out)) return(empty)
  m <- do.call(rbind, out)
  dimnames(m) <- list(NULL, c("row", "col"))
  m
}

# ---- §6a Segment matching (midnight-safe, session-bounded) ------------------

# Temporal intersection-over-union of two closed intervals, in [0, 1]. Works on
# absolute POSIXct (seconds since epoch), so a segment crossing midnight is a
# single interval - there is no service-day partition to split it.
eval_temporal_iou <- function(a_start, a_end, b_start, b_end) {
  a0 <- as.numeric(a_start); a1 <- as.numeric(a_end)
  b0 <- as.numeric(b_start); b1 <- as.numeric(b_end)
  inter <- pmax(0, pmin(a1, b1) - pmax(a0, b0))
  union <- (a1 - a0) + (b1 - b0) - inter
  ifelse(union > 0, inter / union, 0)
}

# Driving-session id within a set of chronologically ordered segments: a new
# session starts when the gap from the running max end-time to the next start
# exceeds `session_gap`. Bounds matching to a continuous window (spike §6a) and
# keeps the exact-assignment matrices small.
eval_session_ids <- function(start, end, session_gap) {
  o <- order(start)
  s <- as.numeric(start)[o]; e <- as.numeric(end)[o]
  sess <- integer(length(s))
  cur <- 1L; run_end <- e[1L]
  sess[1L] <- 1L
  for (k in seq_along(s)[-1L]) {
    if (s[k] - run_end > session_gap) cur <- cur + 1L
    sess[k] <- cur
    run_end <- max(run_end, e[k])
  }
  out <- integer(length(s)); out[o] <- sess
  out
}

# Match predicted to true segments within each (vehicle_id, route_id) group and
# driving session (NOT service day). Returns the 1:1 matches, per-match `clean`
# flag (truth overlapped by exactly one pred AND pred overlapping exactly one
# truth - so merged/fragmented trips are observable and excluded downstream),
# plus split (fragmented truth) and merge (merging pred) counts kept SEPARATE
# from the assignment. Overnight segments are handled by absolute-time IoU.
eval_match_segments <- function(pred, truth, session_gap = 4 * 3600) {
  pred <- data.table::as.data.table(pred)
  truth <- data.table::as.data.table(truth)
  # seg_id is the match identity AND the weight key downstream: it must be
  # present and globally unique within each table, or matches and service-hour
  # weights would silently collapse or go NA (review required-changes 2/3).
  check_seg_id <- function(ids, what) {
    v <- as.character(ids)
    if (anyNA(v) || any(!nzchar(trimws(v)))) {
      stop("`", what, "$seg_id` must not contain missing or empty ids.",
        call. = FALSE)
    }
    if (anyDuplicated(v)) {
      stop("`", what, "$seg_id` must be globally unique (found duplicates).",
        call. = FALSE)
    }
  }
  check_seg_id(pred$seg_id, "pred")
  check_seg_id(truth$seg_id, "truth")
  keys <- c("vehicle_id", "route_id")
  groups <- unique(rbind(pred[, ..keys], truth[, ..keys]))
  all_matches <- list()
  split_count <- 0L; merge_count <- 0L
  fragmented_truth <- 0L; merging_pred <- 0L

  for (g in seq_len(nrow(groups))) {
    vk <- groups$vehicle_id[g]; rk <- groups$route_id[g]
    p <- pred[vehicle_id == vk & route_id == rk]
    t <- truth[vehicle_id == vk & route_id == rk]
    if (nrow(p) == 0L || nrow(t) == 0L) next

    # Partition this (vehicle, route) into driving sessions over the union of
    # pred + truth segments; match within each session.
    both_start <- c(p$start, t$start); both_end <- c(p$end, t$end)
    sess <- eval_session_ids(both_start, both_end, session_gap)
    p_sess <- sess[seq_len(nrow(p))]
    t_sess <- sess[nrow(p) + seq_len(nrow(t))]

    for (sid in unique(sess)) {
      pi <- which(p_sess == sid); ti <- which(t_sess == sid)
      if (!length(pi) || !length(ti)) next
      ps <- p[pi]; tsg <- t[ti]
      iou_mat <- outer(
        seq_len(nrow(ps)), seq_len(nrow(tsg)),
        function(i, j) eval_temporal_iou(
          ps$start[i], ps$end[i], tsg$start[j], tsg$end[j]
        )
      )
      overlap <- iou_mat > 0
      per_truth_preds <- colSums(overlap)   # >1 => that truth is fragmented
      per_pred_truths <- rowSums(overlap)   # >1 => that pred merges truths
      split_count <- split_count + sum(per_truth_preds > 1L)
      merge_count <- merge_count + sum(per_pred_truths > 1L)
      fragmented_truth <- fragmented_truth + sum(per_truth_preds > 1L)
      merging_pred <- merging_pred + sum(per_pred_truths > 1L)

      asg <- eval_assign_max_weight(iou_mat)
      if (nrow(asg) == 0L) next
      pr <- asg[, "row"]; tr <- asg[, "col"]
      m <- data.table::data.table(
        iou = iou_mat[cbind(pr, tr)],
        pred_id = ps$seg_id[pr],
        truth_id = tsg$seg_id[tr],
        vehicle_id = vk, route_id = rk,
        boundary_start_err = abs(as.numeric(ps$start[pr]) -
          as.numeric(tsg$start[tr])),
        boundary_end_err = abs(as.numeric(ps$end[pr]) -
          as.numeric(tsg$end[tr])),
        # Clean 1:1 = neither merged nor fragmented.
        clean = per_pred_truths[pr] == 1L & per_truth_preds[tr] == 1L,
        truth_dur = as.numeric(tsg$end[tr]) - as.numeric(tsg$start[tr]),
        pred_dur = as.numeric(ps$end[pr]) - as.numeric(ps$start[pr])
      )
      all_matches[[length(all_matches) + 1L]] <- m
    }
  }
  matches <- if (length(all_matches)) {
    data.table::rbindlist(all_matches)
  } else {
    data.table::data.table(
      iou = numeric(), pred_id = character(), truth_id = character(),
      vehicle_id = character(), route_id = character(),
      boundary_start_err = numeric(), boundary_end_err = numeric(),
      clean = logical(), truth_dur = numeric(), pred_dur = numeric()
    )
  }
  list(
    matches = matches,
    n_pred = nrow(pred), n_truth = nrow(truth),
    splits = split_count, merges = merge_count,
    fragmented_truth = fragmented_truth, merging_pred = merging_pred
  )
}

# Segment weight in seconds ("service-hours"): a supplied weight column, else
# the segment duration.
eval_seg_weight <- function(dt, weight = NULL) {
  dt <- data.table::as.data.table(dt)
  if (!is.null(weight) && weight %in% names(dt)) return(as.numeric(dt[[weight]]))
  as.numeric(dt$end) - as.numeric(dt$start)
}

# Detection metrics at one IoU threshold: unweighted (count) micro, genuinely
# service-hour-WEIGHTED micro, macro (by route), split/merge counts AND rates
# with explicit denominators, boundary error, ping coverage / unassigned
# duration, and a route-cluster bootstrap CI (resampling routes, one of the two
# uncertainty units §6a names).
eval_segment_metrics <- function(
  pred, truth, iou_threshold = 0.5, weight = NULL, session_gap = 4 * 3600,
  n_boot = 200L, seed = 1L, boot_unit = c("route", "vehicle")
) {
  boot_unit <- match.arg(boot_unit)
  pred <- data.table::as.data.table(pred)
  truth <- data.table::as.data.table(truth)
  mm <- eval_match_segments(pred, truth, session_gap = session_gap)
  m <- mm$matches
  tp_mask <- m$iou >= iou_threshold
  tp <- sum(tp_mask)
  fp <- mm$n_pred - tp
  fn <- mm$n_truth - tp

  prf <- function(tp, fp, fn) {
    prec <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    rec <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    f1 <- if (!is.na(prec) && !is.na(rec) && prec + rec > 0) {
      2 * prec * rec / (prec + rec)
    } else NA_real_
    c(precision = prec, recall = rec, f1 = f1)
  }
  micro_unweighted <- prf(tp, fp, fn)

  # Weighted (service-hours) micro. matched pred/truth = those in an at-
  # threshold pair; weight by duration (or the supplied weight column).
  tw <- eval_seg_weight(truth, weight)
  pw <- eval_seg_weight(pred, weight)
  names(tw) <- as.character(truth$seg_id); names(pw) <- as.character(pred$seg_id)
  matched_truth <- unique(m$truth_id[tp_mask])
  matched_pred <- unique(m$pred_id[tp_mask])
  w_rec <- if (sum(tw) > 0) sum(tw[matched_truth]) / sum(tw) else NA_real_
  w_prec <- if (sum(pw) > 0) sum(pw[matched_pred]) / sum(pw) else NA_real_
  w_f1 <- if (!is.na(w_prec) && !is.na(w_rec) && w_prec + w_rec > 0) {
    2 * w_prec * w_rec / (w_prec + w_rec)
  } else NA_real_
  micro <- c(precision = w_prec, recall = w_rec, f1 = w_f1)

  # Macro over the UNION of truth and predicted routes, so a route that has
  # only false-positive predictions (no true trips) still lowers macro
  # precision instead of being invisible (review required-change 1).
  routes <- union(as.character(truth$route_id), as.character(pred$route_id))
  route_stat <- function(rk) {
    tp_r <- nrow(m[route_id == rk & iou >= iou_threshold])
    fp_r <- nrow(pred[route_id == rk]) - tp_r
    fn_r <- nrow(truth[route_id == rk]) - tp_r
    prec <- if (tp_r + fp_r > 0) tp_r / (tp_r + fp_r) else NA_real_
    rec <- if (tp_r + fn_r > 0) tp_r / (tp_r + fn_r) else NA_real_
    # Compute F1 DIRECTLY as 2tp/(2tp+fp+fn), not from precision*recall. A
    # false-positive-only route (fp>0, tp=fn=0) or a false-negative-only route
    # (fn>0, tp=fp=0) then scores F1 = 0 - it is not dropped as NA and averaged
    # away (review round-4 required 1). Defined for every union route.
    f1 <- if (2 * tp_r + fp_r + fn_r > 0) {
      2 * tp_r / (2 * tp_r + fp_r + fn_r)
    } else NA_real_
    c(precision = prec, recall = rec, f1 = f1)
  }
  macro <- colMeans(do.call(rbind, lapply(routes, route_stat)), na.rm = TRUE)
  names(macro) <- c("precision", "recall", "f1")

  # Split/merge RATES with explicit denominators (consumed by Gate 2):
  # fragment_rate = fragmented truths / all truths; merge_rate = merging preds
  # / all preds.
  fragment_rate <- if (mm$n_truth > 0) mm$fragmented_truth / mm$n_truth else NA_real_
  merge_rate <- if (mm$n_pred > 0) mm$merging_pred / mm$n_pred else NA_real_

  # Duration coverage: fraction of true service-time captured by an at-
  # threshold match, and the complementary unassigned duration.
  assigned_truth_dur <- sum(tw[matched_truth])
  total_truth_dur <- sum(tw)
  unassigned_duration <- total_truth_dur - assigned_truth_dur
  duration_coverage <- if (total_truth_dur > 0) {
    assigned_truth_dur / total_truth_dur
  } else NA_real_

  # Ping coverage (only if predicted segments carry n_pings).
  ping_coverage <- NA_real_
  if ("n_pings" %in% names(pred) && sum(pred$n_pings, na.rm = TRUE) > 0) {
    ping_coverage <- sum(
      pred$n_pings[as.character(pred$seg_id) %in% matched_pred], na.rm = TRUE
    ) / sum(pred$n_pings, na.rm = TRUE)
  }

  # Cluster bootstrap CI of the weighted micro-F1, resampling whole routes (or
  # vehicles) - clusters, not individual segments.
  set.seed(seed)
  ucol <- if (boot_unit == "route") "route_id" else "vehicle_id"
  # Union of truth AND predicted clusters, so false-positive-only clusters stay
  # represented in the resample (review required-change 1).
  units <- union(as.character(truth[[ucol]]), as.character(pred[[ucol]]))
  boot_f1 <- rep(NA_real_, n_boot)
  # Each resampled cluster occurrence becomes an INDEPENDENT replica: tag its
  # seg_id AND its grouping column with the draw index so a cluster sampled
  # twice does not collapse via shared ids (review required-change 1). A
  # perfect dataset then stays perfect under every resample.
  make_replica <- function(dt, uval, k) {
    r <- data.table::copy(dt[get(ucol) == uval])
    if (nrow(r) == 0L) return(r)
    tag <- paste0("##b", k)
    r[, seg_id := paste0(seg_id, tag)]
    r[, (ucol) := paste0(get(ucol), tag)]
    r
  }
  if (length(units) > 1L) {
    for (b in seq_len(n_boot)) {
      sampled <- sample(units, replace = TRUE)
      pp <- data.table::rbindlist(lapply(seq_along(sampled),
        function(k) make_replica(pred, sampled[k], k)))
      tt <- data.table::rbindlist(lapply(seq_along(sampled),
        function(k) make_replica(truth, sampled[k], k)))
      if (nrow(tt) == 0L) next
      bm <- eval_match_segments(pp, tt, session_gap = session_gap)
      bmask <- bm$matches$iou >= iou_threshold
      btw <- eval_seg_weight(tt, weight); bpw <- eval_seg_weight(pp, weight)
      names(btw) <- as.character(tt$seg_id); names(bpw) <- as.character(pp$seg_id)
      mt <- unique(bm$matches$truth_id[bmask]); mp <- unique(bm$matches$pred_id[bmask])
      br <- if (sum(btw) > 0) sum(btw[mt]) / sum(btw) else NA_real_
      bp <- if (sum(bpw) > 0) sum(bpw[mp]) / sum(bpw) else NA_real_
      boot_f1[b] <- if (!is.na(bp) && !is.na(br) && bp + br > 0) {
        2 * bp * br / (bp + br)
      } else NA_real_
    }
  }
  ci <- stats::quantile(boot_f1, c(0.025, 0.975), na.rm = TRUE)

  list(
    iou_threshold = iou_threshold,
    tp = tp, fp = fp, fn = fn,
    micro = micro, micro_unweighted = micro_unweighted, macro = macro,
    splits = mm$splits, merges = mm$merges,
    fragment_rate = fragment_rate, merge_rate = merge_rate,
    duration_coverage = duration_coverage,
    unassigned_duration = unassigned_duration,
    ping_coverage = ping_coverage,
    boundary_start_err = summary(m$boundary_start_err[tp_mask]),
    boundary_end_err = summary(m$boundary_end_err[tp_mask]),
    f1_ci = ci, boot_unit = boot_unit,
    matches = m
  )
}

# ---- §6b Orientation scoring (coverage AND accuracy, both primary) ----------

# `matched` is eval_match_segments()'s matches joined to pred orientation and
# true direction_id: columns pred_orientation (0/1/NA), pred_status,
# pred_confidence (opt), truth_direction (0/1), route_id, and `clean`. In the
# default `end_to_end` mode a missing `clean` is an ERROR (recovery cannot fail
# open); `mode = "conditional_only"` is the explicit opt-in that scores accepted
# matches and returns NA end-to-end. `n_truth_total` is the full count of true
# trips (for end-to-end recovery). Merged/fragmented trips
# (clean == FALSE) count as UNRECOVERED and are excluded from conditional
# accuracy and the per-route mapping - they are not clean 1:1 matches.
eval_orientation <- function(
  matched, n_truth_total, min_coverage = 0.7,
  mode = c("end_to_end", "conditional_only")
) {
  mode <- match.arg(mode)
  d <- data.table::as.data.table(data.table::copy(matched))
  if (!"clean" %in% names(d)) {
    # A primary gate metric must not fail OPEN. Without `clean`, merged/
    # fragmented trips cannot be excluded, so end-to-end recovery is refused;
    # conditional-only scoring is the explicit opt-in (review critical 2).
    if (mode == "end_to_end") {
      stop(
        "`matched` has no `clean` column, so end-to-end recovery cannot exclude ",
        "merged/fragmented trips. Pass matches from eval_match_segments(), or ",
        "call with mode = \"conditional_only\" to score accepted matches only.",
        call. = FALSE
      )
    }
    d[, clean := TRUE]
  }
  n_matched <- nrow(d)
  # Coverage is over clean 1:1 matches (a merged pair is not an accepted
  # single-trip prediction), so abstaining-by-merging cannot inflate coverage.
  clean_d <- d[clean == TRUE]
  covered <- clean_d[!is.na(pred_orientation)]
  coverage <- if (nrow(clean_d) > 0L) nrow(covered) / nrow(clean_d) else NA_real_

  # Per-route 0/1 mapping on covered clean matches.
  map_route <- function(sub) {
    ident <- mean(sub$pred_orientation == sub$truth_direction)
    swap <- mean(sub$pred_orientation == (1L - sub$truth_direction))
    sub$pred_mapped <- if (is.na(ident) || is.na(swap) || ident >= swap) {
      sub$pred_orientation
    } else {
      1L - sub$pred_orientation
    }
    sub
  }
  mapped <- if (nrow(covered) > 0L) {
    data.table::rbindlist(lapply(split(covered, covered$route_id), map_route))
  } else {
    data.table::copy(covered)[, pred_mapped := integer()]
  }

  per_class_recall <- function(dd) {
    vapply(c(0L, 1L), function(cl) {
      idx <- dd$truth_direction == cl
      if (sum(idx) == 0L) NA_real_ else mean(dd$pred_mapped[idx] == cl)
    }, numeric(1))
  }
  recalls <- if (nrow(mapped) > 0L) per_class_recall(mapped) else c(NA, NA)
  balanced_accuracy <- mean(recalls, na.rm = TRUE)

  # Routes with only one observed class: per-route mapping is degenerate there,
  # so report them explicitly rather than letting them inflate accuracy.
  single_class_routes <- if (nrow(clean_d) > 0L) {
    tab <- clean_d[!is.na(truth_direction),
      .(k = data.table::uniqueN(truth_direction)), by = route_id]
    tab$route_id[tab$k < 2L]
  } else character(0)

  # Class-conditional abstention over clean matches whose class is known.
  abst_rate <- vapply(c(0L, 1L), function(cl) {
    idx <- clean_d$truth_direction == cl
    if (sum(idx) == 0L) NA_real_ else mean(is.na(clean_d$pred_orientation[idx]))
  }, numeric(1))

  # Risk-coverage curve (needs confidences).
  risk_coverage <- NULL
  if ("pred_confidence" %in% names(mapped) && nrow(mapped) > 0L &&
      any(!is.na(mapped$pred_confidence))) {
    o <- order(mapped$pred_confidence, decreasing = TRUE)
    correct <- (mapped$pred_mapped == mapped$truth_direction)[o]
    risk_coverage <- data.table::data.table(
      coverage = seq_along(correct) / nrow(clean_d),
      selective_error = cumsum(!correct) / seq_along(correct)
    )
  }

  # Confidence calibration: reliability bins + expected calibration error.
  calibration <- NULL
  if ("pred_confidence" %in% names(mapped) && nrow(mapped) > 0L &&
      any(!is.na(mapped$pred_confidence))) {
    cc <- mapped[!is.na(pred_confidence)]
    br <- cut(cc$pred_confidence, breaks = seq(0, 1, by = 0.1),
              include.lowest = TRUE)
    rel <- data.table::data.table(
      bin = br,
      correct = as.integer(cc$pred_mapped == cc$truth_direction),
      conf = cc$pred_confidence
    )[, .(n = .N, mean_conf = mean(conf), acc = mean(correct)), by = bin]
    ece <- sum(rel$n / nrow(cc) * abs(rel$acc - rel$mean_conf))
    calibration <- list(reliability = rel, ece = ece)
  }

  # End-to-end recovery: over ALL true trips, only clean AND correctly oriented
  # count. Missed/merged/fragmented/abstained -> unrecovered. Fragment
  # convention: each true trip scored once, via its assigned clean segment.
  recovered <- if (nrow(mapped) > 0L) {
    sum(mapped$pred_mapped == mapped$truth_direction)
  } else 0L
  end_to_end_recovery <- if (mode == "end_to_end") {
    recovered / n_truth_total
  } else {
    NA_real_  # conditional-only mode does not claim an end-to-end figure
  }
  list(
    mode = mode,
    coverage = coverage,
    min_coverage_gate = isTRUE(coverage >= min_coverage),
    min_coverage = min_coverage,
    balanced_accuracy = balanced_accuracy,
    per_class_recall = stats::setNames(recalls, c("class0", "class1")),
    class_abstention = stats::setNames(abst_rate, c("class0", "class1")),
    single_class_routes = single_class_routes,
    conditional_accuracy = if (nrow(mapped) > 0L) {
      mean(mapped$pred_mapped == mapped$truth_direction)
    } else NA_real_,
    end_to_end_recovery = end_to_end_recovery,
    risk_coverage = risk_coverage,
    calibration = calibration,
    n_matched = n_matched, n_clean = nrow(clean_d),
    n_truth_total = n_truth_total
  )
}

# ---- §6c Pattern (P4) scoring: permutation-invariant clustering metric ------
# Adjusted Rand Index (dependency-free). P4 stays reserved until a detector
# clears a threshold on this metric (spike §6c).
eval_pattern_ari <- function(pred_labels, true_labels) {
  tab <- table(pred_labels, true_labels)
  n <- sum(tab)
  if (n < 2L) return(NA_real_)
  choose2 <- function(x) x * (x - 1) / 2
  sum_ij <- sum(choose2(tab))
  sum_a <- sum(choose2(rowSums(tab)))
  sum_b <- sum(choose2(colSums(tab)))
  expected <- sum_a * sum_b / choose2(n)
  max_index <- (sum_a + sum_b) / 2
  if (max_index == expected) return(1)
  (sum_ij - expected) / (max_index - expected)
}

# ---- §6d Anchor detection + terminal-stop association -----------------------
# Turnaround-cluster DETECTION: exact 1:1 spatial assignment of discovered
# clusters to audited truth anchors within `dist_tol` metres; precision/recall,
# cluster split/merge, and a declared min-support filter (dropped count is
# reported, never silently hidden). Optional stability: if `discovered` carries
# n_groups_present / n_groups_total (days or vehicles a cluster recurs in),
# report mean stability of matched clusters (spike §6d).
eval_anchor_detection <- function(
  discovered, truth, dist_tol = 150, min_recurrence = 3L, min_coverage = 0.1
) {
  discovered <- data.table::as.data.table(discovered)
  truth <- data.table::as.data.table(truth)
  kept <- discovered[recurrence >= min_recurrence & coverage >= min_coverage]
  n_dropped <- nrow(discovered) - nrow(kept)
  base <- list(
    tp = 0L, n_discovered_kept = nrow(kept), n_truth = nrow(truth),
    n_dropped_low_support = n_dropped, splits = 0L, merges = 0L,
    stability = NA_real_
  )
  if (nrow(kept) == 0L || nrow(truth) == 0L) {
    return(c(list(
      precision = if (nrow(kept) == 0L) NA_real_ else 0,
      recall = if (nrow(truth) == 0L) NA_real_ else 0
    ), base))
  }
  dmat <- outer(seq_len(nrow(kept)), seq_len(nrow(truth)), function(i, j) {
    haversine_m_r(kept$lat[i], kept$lon[i], truth$lat[j], truth$lon[j])
  })
  within <- dmat <= dist_tol
  splits <- sum(colSums(within) > 1L)
  merges <- sum(rowSums(within) > 1L)
  # Exact max-weight assignment on closeness (proximity as weight) within tol.
  w <- ifelse(within, dist_tol - dmat + 1, 0)
  asg <- eval_assign_max_weight(w)
  tp <- nrow(asg)
  stability <- NA_real_
  if (all(c("n_groups_present", "n_groups_total") %in% names(kept)) && tp > 0L) {
    matched_rows <- asg[, "row"]
    stability <- mean(
      kept$n_groups_present[matched_rows] / kept$n_groups_total[matched_rows]
    )
  }
  c(list(precision = tp / nrow(kept), recall = tp / nrow(truth)),
    utils::modifyList(base, list(
      tp = tp, splits = splits, merges = merges, stability = stability
    )))
}

# Terminal-stop ASSOCIATION (a separate step from detection): nearest scheduled
# stop to each discovered anchor within `dist_tol`, with an explicit unmatched
# state (NA stop, distance beyond tolerance). `stops` needs stop_id/lat/lon.
eval_terminal_stop_association <- function(anchors, stops, dist_tol = 100) {
  anchors <- data.table::as.data.table(anchors)
  stops <- data.table::as.data.table(stops)
  if (nrow(anchors) == 0L) {
    return(data.table::data.table(
      anchor = integer(), stop_id = character(), distance_m = numeric(),
      matched = logical()
    ))
  }
  res <- lapply(seq_len(nrow(anchors)), function(i) {
    d <- haversine_m_r(anchors$lat[i], anchors$lon[i], stops$lat, stops$lon)
    j <- which.min(d)
    matched <- length(j) > 0L && d[j] <= dist_tol
    data.table::data.table(
      anchor = i,
      stop_id = if (matched) as.character(stops$stop_id[j]) else NA_character_,
      distance_m = if (length(j)) d[j] else NA_real_,
      matched = isTRUE(matched)
    )
  })
  data.table::rbindlist(res)
}

# ---- §5 Data-sufficiency diagnostics (abstain early when unmet) -------------
# A COMPLETE vehicle-day is one with enough pings AND enough within-day span to
# carry a real trajectory - not merely one ping on a date (the review's
# counterexample). When lat/lon are present, also report per-vehicle-day
# movement (total displacement) and route coverage. `vp` needs vehicle_id,
# timestamp (POSIXct); route_id, latitude, longitude optional.
eval_data_sufficiency <- function(
  vp, min_span_hours = 12, min_vehicle_days = 20,
  min_pings_per_vehicle_day = 30L, min_vehicle_day_span_hours = 4,
  min_move_m_per_vehicle_day = 1000, min_segments_per_route = 30,
  move_ping_thresh = 25
) {
  vp <- data.table::as.data.table(data.table::copy(vp))
  data.table::setorderv(vp, c("vehicle_id", "timestamp"))
  span_hours <- as.numeric(difftime(
    max(vp$timestamp), min(vp$timestamp), units = "hours"
  ))
  vp[, service_day := as.Date(timestamp)]
  has_coords <- all(c("latitude", "longitude") %in% names(vp))

  vd <- vp[, {
    span <- as.numeric(difftime(max(timestamp), min(timestamp), units = "hours"))
    move <- if (has_coords && .N > 1L) {
      sum(haversine_m_r(
        latitude[-.N], longitude[-.N], latitude[-1L], longitude[-1L]
      ), na.rm = TRUE)
    } else NA_real_
    .(n_pings = .N, day_span_hours = span, move_m = move)
  }, by = .(vehicle_id, service_day)]

  vd[, complete := n_pings >= min_pings_per_vehicle_day &
        day_span_hours >= min_vehicle_day_span_hours &
        (is.na(move_m) | move_m >= min_move_m_per_vehicle_day)]
  complete_vehicle_days <- sum(vd$complete)

  # Moving-segments-per-route (spike §5): a moving segment is a maximal run of
  # consecutive pings whose step displacement exceeds `move_ping_thresh` within
  # a vehicle-day - a trip proxy. `min_segments_per_route` is ADVISORY (per the
  # frozen pre-registration), so it is EVALUATED and reported but does not gate
  # `sufficient`.
  routes <- NA_integer_
  routes_meeting_min <- NA_integer_
  median_moving_segments_per_route <- NA_real_
  if ("route_id" %in% names(vp)) {
    routes <- data.table::uniqueN(vp$route_id)
    if (has_coords) {
      vp[, step_disp := {
        if (.N > 1L) {
          c(0, haversine_m_r(latitude[-.N], longitude[-.N],
                             latitude[-1L], longitude[-1L]))
        } else 0
      }, by = .(vehicle_id, service_day)]
      vp[, moving := step_disp > move_ping_thresh]
      vp[, run_id := cumsum(moving & !data.table::shift(moving, fill = FALSE)),
        by = .(vehicle_id, service_day)]
      seg <- vp[moving == TRUE,
        .(n = .N), by = .(route_id, vehicle_id, service_day, run_id)]
      ms <- seg[, .(moving_segments = .N), by = route_id]
      # Every route must appear, including ones with ZERO moving segments, or
      # the median is biased upward (review required-change 2).
      ms <- merge(
        data.table::data.table(route_id = unique(vp$route_id)), ms,
        by = "route_id", all.x = TRUE
      )
      ms[is.na(moving_segments), moving_segments := 0L]
      routes_meeting_min <- sum(ms$moving_segments >= min_segments_per_route)
      median_moving_segments_per_route <- stats::median(ms$moving_segments)
    }
  }

  diagnostics <- list(
    observation_span_hours = span_hours,
    complete_vehicle_days = complete_vehicle_days,
    total_vehicle_days = nrow(vd),
    n_routes = routes,
    routes_meeting_min_segments = routes_meeting_min,
    median_moving_segments_per_route = median_moving_segments_per_route,
    median_vehicle_day_move_m = stats::median(vd$move_m, na.rm = TRUE),
    median_pings_per_vehicle_day = stats::median(vd$n_pings),
    n_pings = nrow(vp)
  )
  sufficient <- span_hours >= min_span_hours &&
    complete_vehicle_days >= min_vehicle_days
  list(
    diagnostics = diagnostics,
    sufficient = sufficient,
    thresholds = list(
      min_span_hours = min_span_hours,
      min_vehicle_days = min_vehicle_days,
      min_pings_per_vehicle_day = min_pings_per_vehicle_day,
      min_vehicle_day_span_hours = min_vehicle_day_span_hours,
      min_move_m_per_vehicle_day = min_move_m_per_vehicle_day,
      min_segments_per_route = min_segments_per_route
    )
  )
}
