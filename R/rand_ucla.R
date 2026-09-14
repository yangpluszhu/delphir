#' 计算 RAND/UCLA 分歧指标
#'
#' 按 Fitch 等（2001）手册实现两套定义：
#' 经典规则（两端各至少约 1/3）与 IPRAS / 分歧指数 DI。
#'
#' @param scores 同一指征的有效 1–9 评分向量。
#' @param method `"both"`、`"classic"` 或 `"ipras"`。
#' @param di_threshold DI 阈值，默认 1。
#'
#' @return 单行数据框：`n_valid`, `p30`, `p70`, `ipr`, `iprcp`, `ai`, `ipras`,
#'   `di`, `classic_disagreement`, `ipras_disagreement`, `disagreement`。
#'
#' @section 公式:
#' \deqn{IPR = P_{70}-P_{30}}
#' \deqn{AI = |(P_{30}+P_{70})/2 - 5|}
#' \deqn{IPRAS = 2.35 + 1.5 \times AI}
#' \deqn{DI = IPR / IPRAS}
#' `DI >= 1` 视为分歧。任何分歧都将使最终分类变为 Uncertain。
#'
#' @examples
#' # 高度一致
#' rand_disagreement(c(8, 8, 9, 9, 8, 7, 9, 8, 8))
#' # 两端拉扯
#' rand_disagreement(c(1, 1, 2, 2, 5, 8, 9, 9, 9))
#'
#' @export
rand_disagreement <- function(scores, method = c("both", "classic", "ipras"),
                              di_threshold = 1) {
  method <- match.arg(method)
  xs <- as.numeric(scores[!is.na(scores)])
  n <- length(xs)
  if (n == 0L) {
    return(data.frame(
      n_valid = 0L, p30 = NA_real_, p70 = NA_real_, ipr = NA_real_,
      iprcp = NA_real_, ai = NA_real_, ipras = NA_real_, di = NA_real_,
      n_low = 0L, n_high = 0L, classic_threshold = NA_integer_,
      classic_disagreement = NA, ipras_disagreement = NA, disagreement = NA,
      stringsAsFactors = FALSE
    ))
  }
  q <- .safe_quantile(xs, c(0.30, 0.70))
  p30 <- q[1]
  p70 <- q[2]
  ipr <- p70 - p30
  iprcp <- (p30 + p70) / 2
  ai <- abs(iprcp - 5)
  ipras <- 2.35 + 1.5 * ai
  di <- if (ipras <= 0) Inf else ipr / ipras
  n_low <- sum(xs >= 1 & xs <= 3)
  n_high <- sum(xs >= 7 & xs <= 9)
  thr <- max(3L, as.integer(ceiling(n / 3)))
  if (n < 8L) thr <- max(2L, as.integer(ceiling(n / 3)))
  classic <- (n_low >= thr) && (n_high >= thr)
  ipras_d <- is.finite(di) && di >= di_threshold
  disagreement <- switch(
    method,
    classic = classic,
    ipras = ipras_d,
    both = classic || ipras_d
  )
  data.frame(
    n_valid = n,
    p30 = p30,
    p70 = p70,
    ipr = ipr,
    iprcp = iprcp,
    ai = ai,
    ipras = ipras,
    di = di,
    n_low = n_low,
    n_high = n_high,
    classic_threshold = thr,
    classic_disagreement = classic,
    ipras_disagreement = ipras_d,
    disagreement = disagreement,
    stringsAsFactors = FALSE
  )
}

#' 按 RAND/UCLA 规则判定适宜性
#'
#' Median 1–3 且无分歧 → Inappropriate；Median 7–9 且无分歧 → Appropriate；
#' Median 4–6 **或任何分歧** → Uncertain。不强迫面板达成一致。
#'
#' @param study [delphi_study()] 或条目汇总。
#' @param round 使用哪一轮，默认最后一轮。
#' @param method 分歧定义，默认取方案中的设定。
#' @param summary 若已计算 [summarize_items()]，可直接传入。
#'
#' @return 数据框，含 Median、DI、两种分歧标记与最终 `class`。
#'
#' @examples
#' ram <- simulate_rand_ucla(n_items = 10, seed = 8)
#' cls <- classify_rand(ram)
#' table(cls$class_zh)
#'
#' @export
classify_rand <- function(study, round = NULL, method = NULL, summary = NULL) {
  study <- .match_study(study)
  p <- study$protocol
  if (is.null(method)) method <- p$rand$disagreement
  if (is.null(summary)) summary <- summarize_items(study)
  if (is.null(round)) round <- max(summary$round)
  summary <- summary[summary$round == round, , drop = FALSE]
  r <- .ensure_unable(study$ratings)
  r <- r[r$round == round, ]

  extra <- lapply(seq_len(nrow(summary)), function(i) {
    sub <- r[r$item_id == summary$item_id[i] & r$dimension == summary$dimension[i], ]
    xs <- .valid_scores(sub$score, sub$unable)
    d <- rand_disagreement(xs, method = method, di_threshold = p$rand$di_threshold)
    d
  })
  extra <- do.call(rbind, extra)
  out <- cbind(summary, extra[, setdiff(names(extra), names(summary)), drop = FALSE])
  med <- out$median
  disag <- out$disagreement
  class <- rep("uncertain", nrow(out))
  class[!is.na(med) & med <= 3 & !isTRUE(disag)] <- "inappropriate"
  class[!is.na(med) & med >= 7 & !isTRUE(disag)] <- "appropriate"
  class[isTRUE(disag)] <- "uncertain"
  class[is.na(med)] <- NA_character_
  out$class <- class
  out$class_zh <- .label_zh(class)
  out$action <- ifelse(class == "uncertain", "discuss", "keep")
  out$rule_set <- "rand_ucla"
  .add_class(out, "delphi_rand")
}
