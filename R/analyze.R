#' 按锁定方案完成 Delphi 全流程分析
#'
#' 主分析入口：参与度、条目汇总、共识/适宜性判定、稳定性、利益相关者分层、
#' 脱落偏倚、权威系数（若有）、意见主题与 Kendall W。
#'
#' @param study [delphi_study()] 对象。
#' @param by_stakeholder 是否做分层，默认在存在多个利益相关者时为 `TRUE`。
#' @param check_dropout 是否比较完成者与脱落者。
#' @param wilcoxon 稳定性分析中是否计算 Wilcoxon，默认 `TRUE`。
#'
#' @return `delphi_result` 列表，主要元素：
#'   `study`, `item_summary`, `classification`, `participation`,
#'   `stability`, `stakeholder`, `dropout`, `authority`,
#'   `comments_summary`, `kendall`, `stopping`。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 16, n_items = 8, n_rounds = 2, seed = 16)
#' res <- analyze_delphi(study)
#' res
#'
#' @export
analyze_delphi <- function(study, by_stakeholder = TRUE, check_dropout = TRUE,
                           wilcoxon = TRUE) {
  study <- .match_study(study)
  validate_delphi(study)
  p <- study$protocol
  participation <- response_summary(study)
  item_summary <- summarize_items(study)

  if (identical(p$scenario, "rand_ucla")) {
    classification <- do.call(rbind, lapply(.unique_sorted(item_summary$round), function(rnd) {
      classify_rand(study, round = rnd, summary = item_summary)
    }))
    rownames(classification) <- NULL
  } else if (identical(p$scenario, "ranking")) {
    classification <- classify_consensus(item_summary, p)
  } else {
    classification <- classify_consensus(item_summary, p)
  }

  stability <- NULL
  stopping <- NULL
  if (length(unique(item_summary$round)) >= 2L) {
    stability <- analyze_stability(
      classification, p,
      ratings = study$ratings,
      wilcoxon = wilcoxon
    )
    stopping <- attr(stability, "stopping")
  }

  stakeholder <- NULL
  n_st <- length(unique(stats::na.omit(study$panel$stakeholder)))
  if (isTRUE(by_stakeholder) && n_st > 1L) {
    stakeholder <- stakeholder_stratify(study)
  }

  dropout <- NULL
  rounds <- .unique_sorted(study$ratings$round)
  if (isTRUE(check_dropout) && length(rounds) >= 2L) {
    dropout <- dropout_bias(study, min(rounds), min(rounds) + 1L)
  }

  authority <- NULL
  pnl <- study$panel
  if (all(c("Ca", "Cs") %in% names(pnl))) {
    authority <- expert_authority(pnl)
  } else if (all(c("familiarity", "practice", "theory", "literature", "intuition") %in% names(pnl))) {
    authority <- score_authority(pnl)
  }

  comments_summary <- summarize_comments(study = study)

  last_round <- max(item_summary$round)
  dims_last <- unique(item_summary$dimension[item_summary$round == last_round])
  prefer <- intersect(c("\u91cd\u8981\u6027", "overall", "\u9002\u5b9c\u6027"), dims_last)
  dim0 <- if (length(prefer)) prefer[1] else dims_last[1]
  kendall <- tryCatch(
    kendall_w(study, round = last_round, dimension = dim0),
    error = function(e) list(error = conditionMessage(e))
  )

  last <- classification[classification$round == last_round, ]
  if ("dimension" %in% names(last) && !is.null(dim0)) {
    last <- last[last$dimension == dim0, ]
  }

  result <- structure(
    list(
      study = study,
      item_summary = item_summary,
      classification = classification,
      participation = participation,
      stability = stability,
      stakeholder = stakeholder,
      dropout = dropout,
      authority = authority,
      comments_summary = comments_summary,
      kendall = kendall,
      stopping = stopping,
      last_round = last_round,
      last_classification = last
    ),
    class = "delphi_result"
  )
  result
}

#' @export
print.delphi_result <- function(x, ...) {
  cat("Delphi \u5206\u6790\u7ed3\u679c\n")
  cat(sprintf("  \u573a\u666f: %s\n", .scenario_label(x$study$protocol$scenario)))
  if (!is.null(x$study$meta$title)) cat(sprintf("  \u9898\u76ee: %s\n", x$study$meta$title))
  cat(sprintf("  \u8f6e\u6b21: %s\n", paste(.unique_sorted(x$item_summary$round), collapse = ", ")))
  if (!is.null(x$participation$by_round)) {
    rr <- x$participation$by_round
    cat("  \u53c2\u4e0e\u5ea6:\n")
    for (i in seq_len(nrow(rr))) {
      cat(sprintf("    \u7b2c %s \u8f6e  %s/%s  (%.1f%%)\n",
                  rr$round[i], rr$n_responded[i], rr$n_invited[i], rr$response_rate[i]))
    }
  }
  if (!is.null(x$last_classification) && "class_zh" %in% names(x$last_classification)) {
    tab <- table(x$last_classification$class_zh)
    cat("  \u672b\u8f6e\u5206\u7c7b:\n")
    for (nm in names(tab)) cat(sprintf("    %s: %s\n", nm, tab[[nm]]))
  }
  if (!is.null(x$stopping)) {
    cat(sprintf("  \u7a33\u5b9a\u6027\u5efa\u8bae\u7ec8\u6b62: %s\n", if (isTRUE(x$stopping$recommend_stop)) "\u662f" else "\u5426"))
  }
  invisible(x)
}

#' @export
summary.delphi_result <- function(object, ...) {
  object$last_classification
}
