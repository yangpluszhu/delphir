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

#' 计算国内文献常用的专家权威系数 Cr
#'
#' \deqn{Cr = (Ca + Cs) / 2}
#' 国内研究常把 Cr ≥ 0.70 视为权威性较好。这不是国际 Delphi（DCAT）的核心要求。
#' 国际方法学更关注专家构成是否完整、是否匿名、是否迭代与反馈、共识是否预先定义。
#'
#' @param panel [delphi_panel()] 或含 `Ca`、`Cs` 的数据框。
#' @param threshold 参考界值，默认 0.70。
#'
#' @return 专家级数据框，含 `Cr` 与 `cr_ok`；属性 `overall` 为面板均数。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 10, n_items = 4, seed = 6)
#' auth <- expert_authority(study$panel)
#' attr(auth, "overall")
#'
#' @export
expert_authority <- function(panel, threshold = 0.70) {
  panel <- .as_df(panel)
  .need_cols(panel, c("expert_id", "Ca", "Cs"), "\u6743\u5a01\u7cfb\u6570\u8ba1\u7b97")
  .stop_if(any(panel$Ca < 0 | panel$Ca > 1 | panel$Cs < 0 | panel$Cs > 1, na.rm = TRUE),
           "Ca\u3001Cs \u5e94\u5728 0 \u4e0e 1 \u4e4b\u95f4\u3002")
  out <- panel[, intersect(c("expert_id", "stakeholder", "Ca", "Cs"), names(panel)), drop = FALSE]
  out$Cr <- (as.numeric(out$Ca) + as.numeric(out$Cs)) / 2
  out$cr_ok <- !is.na(out$Cr) & out$Cr >= threshold
  attr(out, "overall") <- list(
    mean_ca = mean(out$Ca, na.rm = TRUE),
    mean_cs = mean(out$Cs, na.rm = TRUE),
    mean_cr = mean(out$Cr, na.rm = TRUE),
    pct_ok = .pct(sum(out$cr_ok), sum(!is.na(out$Cr))),
    threshold = threshold,
    note = "Cr \u4e0d\u662f\u56fd\u9645 Delphi \u6838\u5fc3\u6307\u6807\uff0c\u4ec5\u4f9b\u56fd\u5185\u53d1\u8868\u8865\u5145\u62a5\u544a\u3002"
  )
  .add_class(out, "delphi_authority")
}
