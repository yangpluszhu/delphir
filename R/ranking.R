#' 优先级 / 排序分析
#'
#' 当研究问题是“哪些最优先”而不是“是否同意”时使用。报告平均秩、中位秩、
#' Top-k 入选比例，并可计算 Kendall W。
#'
#' @param ratings 长表，`score` 解释为秩（1 表示最优先）或 Likert。
#' @param k Top-k，默认取方案或 5。
#' @param score_is_rank `TRUE` 表示数字越小越优先。
#' @param study 可选 [delphi_study()]。
#' @param round 轮次。
#'
#' @return 条目排序表，属性含 Kendall W。
#'
#' @examples
#' ranks <- data.frame(
#'   expert_id = rep(paste0("E", 1:6), each = 4),
#'   item_id = rep(paste0("Q", 1:4), 6),
#'   round = 1,
#'   score = c(1, 2, 3, 4, 1, 3, 2, 4, 2, 1, 4, 3,
#'             1, 2, 4, 3, 1, 2, 3, 4, 2, 1, 3, 4)
#' )
#' rank_priorities(ranks, k = 2)
#'
#' @export
rank_priorities <- function(ratings = NULL, k = 5, score_is_rank = TRUE,
                            study = NULL, round = NULL) {
  if (!is.null(study)) {
    study <- .match_study(study)
    ratings <- study$ratings
    if (!is.null(study$protocol$rules$ranking_topk)) {
      k <- study$protocol$rules$ranking_topk
    }
  }
  ratings <- .as_df(ratings)
  .need_cols(ratings, c("expert_id", "item_id", "score"), "\u6392\u5e8f\u5206\u6790")
  if (!is.null(round)) ratings <- ratings[ratings$round == round, ]
  ratings <- ratings[!is.na(ratings$score), ]
  items <- unique(ratings$item_id)
  out <- do.call(rbind, lapply(items, function(id) {
    xs <- ratings$score[ratings$item_id == id]
    if (score_is_rank) {
      top <- xs <= k
      med <- .safe_median(xs)
      mn <- .safe_mean(xs)
    } else {
      top <- xs >= (max(ratings$score, na.rm = TRUE) - k + 1)
      med <- .safe_median(-xs)
      mn <- .safe_mean(xs)
    }
    data.frame(
      item_id = id,
      n = length(xs),
      mean_rank = if (score_is_rank) mn else NA_real_,
      median_rank = if (score_is_rank) med else .safe_median(xs),
      topk = k,
      topk_n = sum(top),
      topk_pct = .pct(sum(top), length(xs)),
      stringsAsFactors = FALSE
    )
  }))
  out <- out[order(out$median_rank, out$mean_rank), ]
  rownames(out) <- NULL
  w <- tryCatch(kendall_w(ratings), error = function(e) NULL)
  attr(out, "kendall_w") <- w
  .add_class(out, "delphi_ranking")
}

#' 简易层次分析法（AHP）定权
#'
#' Delphi 负责“哪些指标留下”，AHP 负责“各指标权重多少”。
#' 本函数接受成对比较矩阵，返回主特征向量权重与一致性比率 CR。
#' CR < 0.10 通常视为通过。
#'
#' @param pairwise 正互反方阵，`a[j,i] = 1/a[i,j]`，对角线为 1。
#'   可用数据框，行/列名作为指标名。
#' @param names 指标名称，默认取矩阵的 dimnames。
#'
#' @return 列表：`weights`, `lambda_max`, `ci`, `cr`, `consistent`。
#'
#' @examples
#' A <- matrix(c(
#'   1, 3, 5,
#'   1/3, 1, 2,
#'   1/5, 1/2, 1
#' ), 3, 3, byrow = TRUE)
#' ahp_weights(A, names = c("重要性", "可行性", "敏感性"))
#'
#' @export
ahp_weights <- function(pairwise, names = NULL) {
  A <- as.matrix(pairwise)
  storage.mode(A) <- "numeric"
  n <- nrow(A)
  .stop_if(n != ncol(A), "\u6210\u5bf9\u6bd4\u8f83\u5fc5\u987b\u662f\u65b9\u9635\u3002")
  .stop_if(any(A <= 0, na.rm = TRUE), "AHP \u77e9\u9635\u5143\u7d20\u5fc5\u987b\u4e3a\u6b63\u3002")
  if (is.null(names)) names <- colnames(A) %||% paste0("I", seq_len(n))
  ev <- eigen(A, symmetric = FALSE)
  idx <- which.max(Re(ev$values))
  w <- Re(ev$vectors[, idx])
  w <- abs(w)
  w <- w / sum(w)
  names(w) <- names
  lmax <- Re(ev$values[idx])
  ci <- if (n > 1) (lmax - n) / (n - 1) else 0
  ri_map <- c(
    `1` = 0, `2` = 0, `3` = 0.58, `4` = 0.90, `5` = 1.12,
    `6` = 1.24, `7` = 1.32, `8` = 1.41, `9` = 1.45, `10` = 1.49
  )
  ri <- if (as.character(n) %in% names(ri_map)) unname(ri_map[as.character(n)]) else 1.56
  cr <- if (ri == 0) 0 else ci / ri
  list(
    weights = w,
    lambda_max = lmax,
    ci = ci,
    cr = cr,
    consistent = cr < 0.10,
    note = "AHP \u7528\u4e8e\u5b9a\u6743\uff0c\u4e0d\u80fd\u66ff\u4ee3 Delphi \u7684\u6761\u76ee\u7b5b\u9009\u3002"
  )
}
