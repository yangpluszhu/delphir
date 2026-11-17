#' 按条目汇总一轮或多轮评分
#'
#' 对 Likert 有序评分计算有效人数、中位数、四分位距、高/中/低区比例，
#' 以及作为补充的均数、标准差与变异系数。“无法判断”不进入分母。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。
#' @param rounds 要汇总的轮次，默认全部。
#' @param dimensions 维度，默认全部。
#'
#' @return 数据框，一类 `"delphi_item_summary"`。主要列：
#'   `item_id`, `round`, `dimension`, `n_valid`, `n_unable`, `median`,
#'   `q1`, `q3`, `iqr`, `p30`, `p70`, `ipr`, `mean`, `sd`, `cv`, `n_full`, `full_rate`,
#'   `n_high`, `n_mid`, `n_low`, `p_high`, `p_mid`, `p_low`,
#'   `median_iqr`, `polarized`。`ipr` 为第 70 与第 30 百分位数之差（type 7），
#'   与 RAND 的 IPR 同一口径。`full_rate` 为满分比（0–100）：得分等于
#'   量表上端的人数占有效评分人数的百分比。满分比和变异系数都不单独决定去留。
#'
#' @section 方法学要点:
#' Median 通常比 Mean 更适合作为有序数据的集中趋势。IQR 小表示意见集中。
#' 两条目 Median 相同而 IQR 不同时，共识程度并不相同。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 12, n_items = 6, seed = 4)
#' s <- summarize_items(study, rounds = 1)
#' head(s[, c("item_id", "median", "iqr", "p_high", "p_low")])
#'
#' @export
summarize_items <- function(study, rounds = NULL, dimensions = NULL) {
  study <- .match_study(study)
  p <- study$protocol
  r <- .ensure_unable(.ensure_dimension(study$ratings))
  if (!is.null(rounds)) r <- r[r$round %in% rounds, , drop = FALSE]
  if (!is.null(dimensions)) r <- r[r$dimension %in% dimensions, , drop = FALSE]
  .stop_if(nrow(r) == 0L, "\u6ca1\u6709\u7b26\u5408\u6761\u4ef6\u7684\u8bc4\u5206\u3002")

  keys <- unique(r[, c("item_id", "round", "dimension")])
  out <- lapply(seq_len(nrow(keys)), function(i) {
    sub <- r[r$item_id == keys$item_id[i] &
               r$round == keys$round[i] &
               r$dimension == keys$dimension[i], ]
    .summarize_one(sub, keys[i, ], p)
  })
  res <- do.call(rbind, out)
  res <- merge(res, study$items[, intersect(c("item_id", "item_text", "domain", "pattern",
                                              "outcome_domain", "indication", "evidence_level"),
                                            names(study$items)), drop = FALSE],
               by = "item_id", all.x = TRUE, sort = FALSE)
  res <- res[order(res$round, res$dimension, res$item_id), ]
  rownames(res) <- NULL
  .add_class(res, "delphi_item_summary")
}

.summarize_one <- function(sub, key, protocol) {
  unable <- as.logical(sub$unable) | is.na(sub$score)
  xs <- as.numeric(sub$score[!unable])
  n_valid <- length(xs)
  n_unable <- sum(unable)
  n_resp <- nrow(sub)
  qq <- .safe_quantile(xs, c(0.25, 0.30, 0.70, 0.75))
  med <- .safe_median(xs)
  mn <- .safe_mean(xs)
  sdv <- .safe_sd(xs)
  n_high <- sum(.in_range(xs, protocol$high_range))
  n_mid <- sum(.in_range(xs, protocol$mid_range))
  n_low <- sum(.in_range(xs, protocol$low_range))
  p_high <- .pct(n_high, n_valid)
  p_low <- .pct(n_low, n_valid)
  polarized <- !is.na(p_high) && !is.na(p_low) &&
    (p_high / 100 >= protocol$rules$polarization_threshold) &&
    (p_low / 100 >= protocol$rules$polarization_threshold)
  data.frame(
    item_id = key$item_id,
    round = key$round,
    dimension = key$dimension,
    n_resp = n_resp,
    n_valid = n_valid,
    n_unable = n_unable,
    p_unable = .pct(n_unable, n_resp),
    median = med,
    q1 = qq[1],
    q3 = qq[4],
    iqr = qq[4] - qq[1],
    p30 = qq[2],
    p70 = qq[3],
    ipr = qq[3] - qq[2],
    mean = mn,
    sd = sdv,
    cv = coefficient_variation(xs),
    n_full = if (n_valid == 0L) 0L else sum(xs == protocol$scale_max),
    full_rate = full_score_ratio(xs, protocol$scale_max),
    n_high = n_high,
    n_mid = n_mid,
    n_low = n_low,
    p_high = p_high,
    p_mid = .pct(n_mid, n_valid),
    p_low = p_low,
    median_iqr = .fmt_median_iqr(med, qq[1], qq[4]),
    polarized = polarized,
    stringsAsFactors = FALSE
  )
}

#' 检测条目是否两极分化
#'
#' 当低区与高区比例同时达到方案中的 `polarization_threshold`（默认各 25%）
#' 时判定为两极分化。均数接近 5 并不能代表“大家认为一般”。
#'
#' @param summary [summarize_items()] 的结果；或直接提供 `p_high` 与 `p_low`。
#' @param p_high,p_low 高区与低区百分比（0–100）。
#' @param threshold 两端比例阈值（0–1），默认取 0.25。
#'
#' @return 逻辑向量。
#'
#' @examples
#' detect_polarization(p_high = c(87, 50), p_low = c(5, 40))
#'
#' @export
detect_polarization <- function(summary = NULL, p_high = NULL, p_low = NULL,
                                threshold = 0.25) {
  if (!is.null(summary)) {
    p_high <- summary$p_high
    p_low <- summary$p_low
  }
  .stop_if(is.null(p_high) || is.null(p_low), "\u8bf7\u63d0\u4f9b summary \u6216 p_high \u4e0e p_low\u3002")
  (p_high / 100 >= threshold) & (p_low / 100 >= threshold)
}
