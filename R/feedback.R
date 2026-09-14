#' 生成下一轮受控反馈表
#'
#' DCAT 将受控反馈视为 Delphi 的核心特征。每位专家应看到：本人上一轮评分、
#' 专家组 Median / IQR / 高区比例、匿名意见摘要，然后再重新评分。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。
#' @param round 作为反馈来源的轮次，默认已完成的最后一轮。
#' @param dimension 维度。
#' @param include_comments 是否附上匿名意见。
#'
#' @return 数据框，一行对应“一位专家 × 一条目”。可用
#'   `split(..., feedback$expert_id)` 拆成个人反馈单。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 10, n_items = 5, seed = 17)
#' fb <- prepare_feedback(study, round = 1)
#' head(fb)
#'
#' @export
prepare_feedback <- function(study, round = NULL, dimension = NULL,
                             include_comments = TRUE) {
  if (inherits(study, "delphi_result")) {
    result <- study
    study <- study$study
    summary <- result$item_summary
    classification <- result$classification
  } else {
    study <- .match_study(study)
    summary <- summarize_items(study)
    if (study$protocol$scenario == "rand_ucla") {
      classification <- do.call(rbind, lapply(.unique_sorted(summary$round), function(rnd) {
        classify_rand(study, round = rnd, summary = summary)
      }))
    } else {
      classification <- classify_consensus(summary, study$protocol)
    }
  }
  if (is.null(round)) round <- max(study$ratings$round)
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  if (!"stakeholder" %in% names(r) && "stakeholder" %in% names(study$panel)) {
    r$stakeholder <- study$panel$stakeholder[match(r$expert_id, study$panel$expert_id)]
  }
  if (!"stakeholder" %in% names(r)) r$stakeholder <- NA_character_
  if (is.null(dimension)) {
    dimension <- unique(r$dimension[r$round == round])[1]
  }
  r <- r[r$round == round & r$dimension == dimension, ]
  cls <- classification[classification$round == round &
                          classification$dimension == dimension, ]
  items <- study$items
  comment_map <- ""
  if (isTRUE(include_comments) && !is.null(study$comments) && nrow(study$comments)) {
    cc <- study$comments[study$comments$round == round, ]
    comment_map <- tapply(cc$comment, cc$item_id, function(x) {
      paste(utils::head(unique(x), 3L), collapse = " | ")
    })
  }

  out <- merge(
    r[, c("expert_id", "item_id", "score", "unable", "stakeholder")],
    cls[, intersect(c("item_id", "median", "iqr", "q1", "q3", "p_high", "p_low",
                      "median_iqr", "class", "class_zh", "n_valid", "di",
                      "disagreement"), names(cls))],
    by = "item_id",
    all.x = TRUE
  )
  out$item_text <- items$item_text[match(out$item_id, items$item_id)]
  out$own_score <- ifelse(out$unable, "\u65e0\u6cd5\u5224\u65ad", as.character(out$score))
  out$group_feedback <- sprintf(
    "Median = %s\uff1bIQR = %s\uff1b\u9ad8\u533a %.1f%%\uff1b\u4f4e\u533a %.1f%%\uff1b\u5206\u7c7b\uff1a%s",
    ifelse(is.na(out$median), "NA", round(out$median, 1)),
    ifelse(is.na(out$iqr), "NA", round(out$iqr, 1)),
    out$p_high, out$p_low, out$class_zh
  )
  out$anonymous_comments <- ""
  if (length(comment_map)) {
    out$anonymous_comments <- unname(comment_map[out$item_id])
    out$anonymous_comments[is.na(out$anonymous_comments)] <- ""
  }
  out$round_source <- round
  out$dimension <- dimension
  out <- out[order(out$expert_id, out$item_id), ]
  rownames(out) <- NULL
  .add_class(out, "delphi_feedback")
}
