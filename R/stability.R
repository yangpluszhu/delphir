#' 评价相邻轮次的稳定性
#'
#' 比较 Median、IQR、高区比例和共识分类是否还在明显变化。
#' 刚跨过阈值就停止，可能过早。DCAT 强调应评价轮次间反应稳定性。
#'
#' @param summary 含多轮的 [summarize_items()] 或 [classify_consensus()] 结果。
#' @param protocol [delphi_protocol()] 对象。
#' @param ratings 可选，用于计算个体评分变化与 Wilcoxon 符号秩检验。
#' @param wilcoxon 是否计算配对 Wilcoxon，默认 `TRUE`，但不可用 P>0.05 定义稳定。
#'
#' @return 数据框，一行表示一个条目在相邻两轮的变化；属性 `stopping` 给出终止判断。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 16, n_items = 8, n_rounds = 2, seed = 11)
#' s <- classify_consensus(summarize_items(study), study$protocol)
#' st <- analyze_stability(s, study$protocol, ratings = study$ratings)
#' attr(st, "stopping")
#'
#' @export
analyze_stability <- function(summary, protocol, ratings = NULL, wilcoxon = TRUE) {
  .stop_if(!inherits(protocol, "delphi_protocol"), "\u9700\u8981 delphi_protocol\u3002")
  .stop_if(!"round" %in% names(summary), "summary \u9700\u8981 round \u5217\u3002")
  if (!"class" %in% names(summary) && protocol$scenario != "rand_ucla") {
    summary <- classify_consensus(summary, protocol)
  }
  rounds <- .unique_sorted(summary$round)
  .stop_if(length(rounds) < 2L, "\u7a33\u5b9a\u6027\u5206\u6790\u81f3\u5c11\u9700\u8981\u4e24\u8f6e\u3002")
  dims <- unique(summary$dimension)
  rows <- list()
  for (d in dims) {
    for (i in seq_len(length(rounds) - 1L)) {
      a <- summary[summary$round == rounds[i] & summary$dimension == d, ]
      b <- summary[summary$round == rounds[i + 1L] & summary$dimension == d, ]
      common <- intersect(a$item_id, b$item_id)
      for (id in common) {
        ra <- a[a$item_id == id, ][1, ]
        rb <- b[b$item_id == id, ][1, ]
        med_d <- rb$median - ra$median
        iqr_d <- rb$iqr - ra$iqr
        ph_d <- (rb$p_high - ra$p_high) / 100
        class_same <- identical(ra$class, rb$class)
        wp <- NA_real_
        n_pair <- NA_integer_
        pct_changed <- NA_real_
        if (!is.null(ratings) && isTRUE(wilcoxon)) {
          pair <- .paired_scores(ratings, id, rounds[i], rounds[i + 1L], d)
          n_pair <- nrow(pair)
          if (n_pair >= 6L && stats::sd(pair$diff) > 0) {
            wp <- tryCatch(
              stats::wilcox.test(pair$r1, pair$r2, paired = TRUE, exact = FALSE)$p.value,
              error = function(e) NA_real_
            )
          }
          if (n_pair > 0L) pct_changed <- .pct(sum(pair$diff != 0), n_pair)
        }
        rows[[length(rows) + 1L]] <- data.frame(
          item_id = id,
          dimension = d,
          from_round = rounds[i],
          to_round = rounds[i + 1L],
          median_r1 = ra$median,
          median_r2 = rb$median,
          median_delta = med_d,
          iqr_r1 = ra$iqr,
          iqr_r2 = rb$iqr,
          iqr_delta = iqr_d,
          p_high_r1 = ra$p_high,
          p_high_r2 = rb$p_high,
          p_high_delta = ph_d * 100,
          class_r1 = ra$class %||% NA_character_,
          class_r2 = rb$class %||% NA_character_,
          class_unchanged = class_same,
          n_paired = n_pair,
          pct_score_changed = pct_changed,
          wilcoxon_p = wp,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0L) {
    stop("\u6ca1\u6709\u53ef\u6bd4\u8f83\u7684\u76f8\u90bb\u8f6e\u6761\u76ee\u3002", call. = FALSE)
  }
  last <- out[out$to_round == max(out$to_round), ]
  rule <- protocol$stability
  class_ok <- mean(last$class_unchanged, na.rm = TRUE) >= rule$class_pct
  median_ok <- mean(abs(last$median_delta) <= rule$median_delta, na.rm = TRUE) >= 0.80
  ph_ok <- mean(abs(last$p_high_delta) / 100 <= rule$high_pct_delta, na.rm = TRUE) >= 0.80
  stop_now <- class_ok && median_ok && ph_ok
  attr(out, "stopping") <- list(
    recommend_stop = stop_now,
    class_unchanged_pct = mean(last$class_unchanged, na.rm = TRUE),
    median_stable_pct = mean(abs(last$median_delta) <= rule$median_delta, na.rm = TRUE),
    p_high_stable_pct = mean(abs(last$p_high_delta) / 100 <= rule$high_pct_delta, na.rm = TRUE),
    max_rounds = rule$max_rounds,
    current_round = max(out$to_round),
    note = "Wilcoxon P>0.05 \u4e0d\u80fd\u5355\u72ec\u5b9a\u4e49\u4e3a\u610f\u89c1\u7a33\u5b9a\uff0c\u56e0\u4e3a P \u503c\u4f9d\u8d56\u6837\u672c\u91cf\u3002"
  )
  .add_class(out, "delphi_stability")
}

.paired_scores <- function(ratings, item_id, r1, r2, dimension) {
  ratings <- .ensure_unable(.ensure_dimension(ratings))
  a <- ratings[ratings$item_id == item_id & ratings$round == r1 &
                 ratings$dimension == dimension & !ratings$unable, ]
  b <- ratings[ratings$item_id == item_id & ratings$round == r2 &
                 ratings$dimension == dimension & !ratings$unable, ]
  m <- merge(
    a[, c("expert_id", "score")],
    b[, c("expert_id", "score")],
    by = "expert_id",
    suffixes = c("_1", "_2")
  )
  data.frame(r1 = m$score_1, r2 = m$score_2, diff = m$score_2 - m$score_1)
}

#' 个体评分变化比例
#'
#' 报告在两轮都评分的专家中，评分发生变化的人数比例。这是稳定性的个体层面补充。
#'
#' @param study [delphi_study()] 对象。
#' @param from_round,to_round 比较的两轮。
#' @param dimension 维度。
#'
#' @return 条目级数据框。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 12, n_items = 5, n_rounds = 2, seed = 12)
#' head(score_shift(study))
#'
#' @export
score_shift <- function(study, from_round = 1, to_round = 2, dimension = NULL) {
  study <- .match_study(study)
  r <- .ensure_dimension(study$ratings)
  if (is.null(dimension)) dimension <- unique(r$dimension)[1]
  ids <- unique(r$item_id)
  do.call(rbind, lapply(ids, function(id) {
    pair <- .paired_scores(r, id, from_round, to_round, dimension)
    data.frame(
      item_id = id,
      dimension = dimension,
      n_paired = nrow(pair),
      n_changed = sum(pair$diff != 0),
      pct_changed = .pct(sum(pair$diff != 0), nrow(pair)),
      mean_abs_change = if (nrow(pair)) mean(abs(pair$diff)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}
