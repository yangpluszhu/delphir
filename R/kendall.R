#' Kendall 协调系数 W
#'
#' 评价多位专家对一组项目的**相对排序**是否协调。W 接近 0 表示排序几乎无关，
#' 接近 1 表示完全一致。适合优先级排序或指标体系赋权前的排序一致性，
#' 不适合单独作为“该条目是否形成共识”的判据。
#'
#' @param ratings 宽表（行=专家，列=条目）或长表（需提供下列列名）。
#' @param expert_col,item_col,score_col 长表列名。
#' @param round,dimension 若传入 [delphi_study()]，用于筛选。
#'
#' @return 列表：`w`, `chi_sq`, `df`, `p_value`, `n_expert`, `n_item`, `note`。
#'
#' @section 方法学要点:
#' 不要把“W 显著 (P<0.05)”解释为每条都已达成共识。显著性依赖样本量。
#'
#' @examples
#' x <- matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE)
#' kendall_w(x)$w
#'
#' @export
kendall_w <- function(ratings, expert_col = "expert_id", item_col = "item_id",
                      score_col = "score", round = NULL, dimension = NULL) {
  if (inherits(ratings, "delphi_study") || inherits(ratings, "delphi_result")) {
    study <- .match_study(ratings)
    r <- study$ratings
    if (!is.null(round)) r <- r[r$round == round, ]
    if (!is.null(dimension)) r <- r[r$dimension == dimension, ]
    ratings <- r
  }
  if (is.matrix(ratings) || (is.data.frame(ratings) && !expert_col %in% names(ratings))) {
    mat <- as.matrix(ratings)
  } else {
    ratings <- .as_df(ratings)
    .need_cols(ratings, c(expert_col, item_col, score_col), "Kendall W")
    ratings <- ratings[!is.na(ratings[[score_col]]), ]
    wide <- stats::reshape(
      ratings[, c(expert_col, item_col, score_col)],
      idvar = expert_col,
      timevar = item_col,
      direction = "wide"
    )
    mat <- as.matrix(wide[, -1, drop = FALSE])
  }
  storage.mode(mat) <- "numeric"
  complete <- stats::complete.cases(mat)
  mat <- mat[complete, , drop = FALSE]
  m <- nrow(mat)
  n <- ncol(mat)
  .stop_if(m < 2L || n < 2L, "Kendall W \u81f3\u5c11\u9700\u8981 2 \u4f4d\u4e13\u5bb6\u548c 2 \u4e2a\u6761\u76ee\u7684\u5b8c\u6574\u8bc4\u5206\u3002")
  ranks <- t(apply(mat, 1, function(row) {
    rk <- rank(row, na.last = "keep", ties.method = "average")
    rk
  }))
  Rj <- colSums(ranks)
  Rbar <- mean(Rj)
  S <- sum((Rj - Rbar)^2)
  tie_adj <- sum(apply(ranks, 1, function(row) {
    tb <- table(row)
    sum(tb^3 - tb)
  }))
  denom <- (m^2 * (n^3 - n) - m * tie_adj)
  .stop_if(denom <= 0, "\u65e0\u6cd5\u8ba1\u7b97 W\uff08\u53ef\u80fd\u5e76\u5217\u8fc7\u591a\uff09\u3002")
  w <- 12 * S / denom
  chi <- m * (n - 1) * w
  df <- n - 1
  p <- stats::pchisq(chi, df = df, lower.tail = FALSE)
  list(
    w = as.numeric(w),
    chi_sq = as.numeric(chi),
    df = df,
    p_value = as.numeric(p),
    n_expert = m,
    n_item = n,
    note = "W \u8bc4\u4ef7\u7684\u662f\u6392\u5e8f\u534f\u8c03\u6027\uff0c\u4e0d\u662f\u6761\u76ee\u7ea7\u5171\u8bc6\u3002"
  )
}

#' 变异系数 CV = SD / Mean
#'
#' 国内 Delphi 常用补充离散指标。Likert 为有序数据时，Median + IQR 更稳健，
#' CV 不应作为唯一共识标准。
#'
#' @param x 数值向量。
#'
#' @return 标量；均数为 0 或样本不足时返回 `NA`。
#'
#' @examples
#' coefficient_variation(c(7, 8, 8, 9, 9))
#'
#' @export
coefficient_variation <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) < 2L) return(NA_real_)
  m <- mean(x)
  if (is.na(m) || abs(m) < .Machine$double.eps) return(NA_real_)
  stats::sd(x) / m
}
