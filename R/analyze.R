#' 按锁定方案完成 Delphi 全流程分析
#'
#' 主分析入口：参与度、条目汇总、共识/适宜性判定、稳定性、利益相关者分层、
#' 脱落偏倚、权威系数（若有）、意见主题与 Kendall W。
#'
#' @param study [delphi_study()] 对象。
#' @param by_stakeholder 是否做分层，默认在存在多个利益相关者时为 `TRUE`。
#' @param check_dropout 是否比较完成者与脱落者。
#' @param wilcoxon 稳定性分析中是否计算 Wilcoxon，默认 `TRUE`。
#' @param up_to_round 只分析 `ratings$round <= up_to_round` 的数据（保留历史轮，
#'   不丢弃脱落者上轮评分）。默认 `NULL` 表示研究中已有的全部轮次。
#'
#' @return `delphi_result` 列表，主要元素：
#'   `study`, `item_summary`, `classification`, `participation`,
#'   `stability`, `stakeholder`, `dropout`, `authority`,
#'   `comments_summary`, `kendall`, `stopping`。核心技术要素场景另含
#'   `expert_profile`、`panel_compliance`、`open_round`；此时 `stopping`
#'   来自 [coordination_decision()]，而不是中位数稳定性。
#'   若方案允许第一轮新增条目，另含 `new_item_review`、`item_log`、
#'   `meeting_agenda`、`new_item_audit`。没有编码表时这些审查结果为空，
#'   不会改写已有条目的共识分类。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 16, n_items = 8, n_rounds = 2, seed = 16)
#' res <- analyze_delphi(study)
#' res
#'
#' @export
analyze_delphi <- function(study, by_stakeholder = TRUE, check_dropout = TRUE,
                           wilcoxon = TRUE, up_to_round = NULL) {
  study <- .match_study(study)
  study <- .slice_study_rounds(study, up_to_round)
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
  if (isTRUE(p$new_items$require_iteration)) {
    classification <- mark_iteration(classification, study$items, study$ratings, p)
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
  if (identical(p$scenario, "standard_element")) {
    stopping <- coordination_decision(study)
  }

  stakeholder <- NULL
  n_st <- length(unique(stats::na.omit(study$panel$stakeholder)))
  if (isTRUE(by_stakeholder) && n_st > 1L) {
    stakeholder <- stakeholder_stratify(study)
  }

  dropout <- NULL
  rounds <- .unique_sorted(study$ratings$round)
  if (isTRUE(check_dropout) && length(rounds) >= 2L) {
    drop_list <- lapply(seq_len(length(rounds) - 1L), function(i) {
          d <- suppressWarnings(tryCatch(
            dropout_bias(study, rounds[i], rounds[i + 1L]),
            error = function(e) NULL
          ))
      if (is.null(d) || !nrow(d)) return(NULL)
      d$from_round <- rounds[i]
      d$to_round <- rounds[i + 1L]
      d
    })
    drop_list <- drop_list[!vapply(drop_list, is.null, logical(1))]
    if (length(drop_list)) dropout <- do.call(rbind, drop_list)
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
  prefer <- intersect(c("\u91cd\u8981\u7a0b\u5ea6", "\u91cd\u8981\u6027", "overall", "\u9002\u5b9c\u6027"), dims_last)
  dim0 <- if (length(prefer)) prefer[1] else dims_last[1]
  kendall <- tryCatch(
    kendall_w(study, round = last_round, dimension = dim0),
    error = function(e) list(error = conditionMessage(e))
  )
  profile <- tryCatch(expert_profile(study$panel), error = function(e) NULL)
  compliance <- tryCatch(panel_compliance(study), error = function(e) NULL)
  open_round <- NULL
  if (!is.null(study$comments) && nrow(study$comments) &&
      any(study$comments$round == 1L) && isTRUE(p$open_first)) {
    open_round <- analyze_open_round(study, round = 1L)
  }

  last <- .latest_class_rows(classification)
  if ("dimension" %in% names(last) && !is.null(dim0) && any(last$dimension == dim0)) {
    last <- last[last$dimension == dim0, ]
  }
  screened <- NULL
  if (!is.null(study$concept_review) && nrow(.as_df(study$concept_review))) {
    screened <- screen_new_items(
      suggestions = study$suggestions,
      review = study$concept_review,
      protocol = p,
      items = study$items
    )
  }
  item_log <- NULL
  if (!is.null(screened)) {
    item_log <- item_changelog(study, review = screened$concepts, classification = classification)
  }
  meeting_agenda <- NULL
  new_item_audit <- NULL
  if (!is.null(p$new_items) && isTRUE(p$new_items$allow)) {
    meeting_agenda <- consensus_meeting_agenda(
      classification,
      floor_items = study$floor_items
    )
    if (!is.null(meeting_agenda) && nrow(meeting_agenda)) {
      known <- as.character(study$items$item_text[match(meeting_agenda$item_id, study$items$item_id)])
      empty <- is.na(meeting_agenda$item_text) | !nzchar(meeting_agenda$item_text)
      meeting_agenda$item_text[empty] <- known[empty]
    }
    new_item_audit <- audit_new_items(
      p,
      review = if (is.null(screened)) NULL else screened$concepts,
      classification = classification,
      suggestions = study$suggestions
    )
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
      expert_profile = profile,
      panel_compliance = compliance,
      open_round = open_round,
      new_item_review = if (is.null(screened)) NULL else screened$concepts,
      item_log = item_log,
      meeting_agenda = meeting_agenda,
      new_item_audit = new_item_audit,
      last_round = last_round,
      last_classification = last
    ),
    class = "delphi_result"
  )
  result
}

.slice_study_rounds <- function(study, up_to_round) {
  if (is.null(up_to_round) || !nrow(study$ratings)) return(study)
  k <- as.integer(up_to_round[1])
  .stop_if(is.na(k) || k < 1L, "up_to_round \u5fc5\u987b\u662f\u6b63\u6574\u6570\u3002")
  have <- .unique_sorted(study$ratings$round)
  .stop_if(!any(have <= k), sprintf("\u6570\u636e\u4e2d\u6ca0\u6709 round \u2264 %s \u7684\u8bc4\u5206\u3002", k))
  study$ratings <- study$ratings[study$ratings$round <= k, , drop = FALSE]
  if (!is.null(study$comments) && nrow(study$comments)) {
    study$comments <- study$comments[study$comments$round <= k, , drop = FALSE]
  }
  study
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
    stop_label <- if (identical(x$study$protocol$scenario, "standard_element")) {
      "\u534f\u8c03\u7a0b\u5ea6\u5efa\u8bae\u7ed3\u675f"
    } else {
      "\u7a33\u5b9a\u6027\u5efa\u8bae\u7ec8\u6b62"
    }
    cat(sprintf("  %s: %s\n", stop_label, if (isTRUE(x$stopping$recommend_stop)) "\u662f" else "\u5426"))
  }
  invisible(x)
}

#' @export
summary.delphi_result <- function(object, ...) {
  object$last_classification
}
