#' 计算每轮参与度、保留率与脱落
#'
#' 专家积极系数即问卷回收率。对 Delphi 更关键的是跨轮保留率（retention）
#' 与脱落（attrition），因为脱落可能改变面板构成。
#'
#' @param study [delphi_study()] 对象。
#'
#' @return 列表：`by_round` 每轮回收率；`retention` 相邻轮保留率；
#'   `attrition_ids` 脱落专家。
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
  by_round <- do.call(rbind, lapply(rounds, function(rnd) {
    ids <- unique(r$expert_id[r$round == rnd])
    data.frame(
      round = rnd,
      n_invited = n_invited,
      n_responded = length(ids),
      response_rate = .pct(length(ids), n_invited),
      stringsAsFactors = FALSE
    )
  }))
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

