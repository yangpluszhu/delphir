#' 计算每轮参与度、保留率与脱落
#'
#' 专家积极系数即问卷回收率。对 Delphi 更关键的是跨轮保留率（retention）
#' 与脱落（attrition），因为脱落可能改变面板构成。
#'
#' @param study [delphi_study()] 对象。
#'
#' @return 列表：`by_round` 每轮回收率；`retention` 相邻轮保留率；
#'   `attrition_ids` 脱落专家。`by_round$positive_coefficient` 与
#'   `response_rate` 相同，都是积极系数（0–100）。`meets_reference`
#'   只表示是否不低于方案里的参考线，不是合格结论，也不加权。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 20, n_items = 6, n_rounds = 2, seed = 9)
#' response_summary(study)$by_round
#'
#' @export
response_summary <- function(study) {
  study <- .match_study(study)
  panel <- study$panel
  r <- study$ratings
  n_invited <- sum(panel$invited %||% TRUE)
  if (n_invited == 0L) n_invited <- nrow(panel)
  rounds <- .unique_sorted(r$round)
  ref <- study$protocol$rules$response_rate_min %||% 0.70
  if (!is.finite(ref)) ref <- 0.70
  by_round <- do.call(rbind, lapply(rounds, function(rnd) {
    ids <- unique(r$expert_id[r$round == rnd])
    rate <- .pct(length(ids), n_invited)
    data.frame(
      round = rnd,
      n_invited = n_invited,
      n_responded = length(ids),
      response_rate = rate,
      positive_coefficient = rate,
      reference_min = ref * 100,
      meets_reference = !is.na(rate) & rate >= ref * 100,
      stringsAsFactors = FALSE
    )
  }))
  attr(by_round, "note") <- paste0(
    "\u79ef\u6781\u7cfb\u6570\u7b49\u4e8e\u95ee\u5377\u56de\u6536\u7387\uff0c\u53ea\u8bf4\u660e\u662f\u5426\u56de\u5377\uff0c",
    "\u4e0d\u662f\u5171\u8bc6\uff0c\u4e5f\u4e0d\u7528\u4e8e\u52a0\u6743\u3002",
    "\u53c2\u8003\u7ebf\u4e0d\u662f\u5408\u683c\u7ebf\u3002"
  )
  retention <- NULL
  attrition_ids <- list()
  if (length(rounds) >= 2L) {
    retention <- do.call(rbind, lapply(seq_len(length(rounds) - 1L), function(i) {
      a <- unique(r$expert_id[r$round == rounds[i]])
      b <- unique(r$expert_id[r$round == rounds[i + 1L]])
      lost <- setdiff(a, b)
      attrition_ids[[paste0("R", rounds[i], "_to_R", rounds[i + 1L])]] <<- lost
      data.frame(
        from_round = rounds[i],
        to_round = rounds[i + 1L],
        n_previous = length(a),
        n_retained = length(intersect(a, b)),
        n_lost = length(lost),
        n_new = length(setdiff(b, a)),
        retention_rate = .pct(length(intersect(a, b)), length(a)),
        stringsAsFactors = FALSE
      )
    }))
  }
  list(by_round = by_round, retention = retention, attrition_ids = attrition_ids)
}

