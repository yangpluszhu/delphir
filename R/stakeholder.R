#' 按利益相关者分层汇总
#'
#' 多利益相关者 Delphi 不能只报告总体。总体过线可能掩盖患者与医生的系统性分歧。
#'
#' @param study [delphi_study()] 对象。
#' @param round 轮次，默认最后一轮。
#' @param group 分层变量名，默认 `stakeholder`。
#'
#' @return 数据框，含各组 Median、IQR、高/低区比例；若方案不是 `rand_ucla`，
#'   同时给出组内共识分类。
#'
#' @examples
#' cos <- simulate_cos(n_clinicians = 12, n_patients = 10, n_researchers = 8,
#'                     n_items = 8, seed = 13)
#' st <- stakeholder_stratify(cos)
#' head(st)
#'
#' @export
stakeholder_stratify <- function(study, round = NULL, group = "stakeholder") {
  study <- .match_study(study)
  r <- .ensure_unable(.ensure_dimension(study$ratings))
  if (!group %in% names(r)) {
    if (group %in% names(study$panel)) {
      r[[group]] <- study$panel[[group]][match(r$expert_id, study$panel$expert_id)]
    } else {
      stop(sprintf("\u627e\u4e0d\u5230\u5206\u5c42\u53d8\u91cf %s\u3002", group), call. = FALSE)
    }
  }
  if (is.null(round)) round <- max(r$round)
  r <- r[r$round == round, ]
  p <- study$protocol
  keys <- unique(r[, c("item_id", "dimension", group)])
  names(keys)[3] <- "group_level"
  r$group_level <- r[[group]]
  out <- lapply(seq_len(nrow(keys)), function(i) {
    sub <- r[r$item_id == keys$item_id[i] &
               r$dimension == keys$dimension[i] &
               r$group_level == keys$group_level[i], ]
    key <- data.frame(
      item_id = keys$item_id[i],
      round = round,
      dimension = keys$dimension[i],
      stringsAsFactors = FALSE
    )
    sm <- .summarize_one(sub, key, p)
    sm$group <- group
    sm$group_level <- keys$group_level[i]
    sm
  })
  res <- do.call(rbind, out)
  if (p$scenario == "rand_ucla") {
    extra <- lapply(seq_len(nrow(res)), function(i) {
      sub <- r[r$item_id == res$item_id[i] & r$dimension == res$dimension[i] &
                 r$group_level == res$group_level[i], ]
      rand_disagreement(.valid_scores(sub$score, sub$unable),
                        method = p$rand$disagreement,
                        di_threshold = p$rand$di_threshold)
    })
    res <- cbind(res, do.call(rbind, extra)[, c("di", "disagreement")])
  } else {
    cls <- classify_consensus(res, p)
    res$class <- cls$class
    res$class_zh <- cls$class_zh
  }
  rownames(res) <- NULL
  .add_class(res, "delphi_stakeholder")
}

#' 比较完成者与脱落者在上一轮的评分
#'
#' 用于检查脱落是否可能带来偏倚：若脱落者在关键条目上系统性地更反对，
#' 则最终共识可能被高估。
#'
#' @param study [delphi_study()] 对象。
#' @param from_round,to_round 默认第 1 轮与第 2 轮。
#'
#' @return 条目级数据框，含两组 Median 及 Wilcoxon 秩和 P 值。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 20, n_items = 6, n_rounds = 2, seed = 14)
#' dropout_bias(study)
#'
#' @export
dropout_bias <- function(study, from_round = 1, to_round = 2) {
  study <- .match_study(study)
  r <- .ensure_unable(study$ratings)
  a <- unique(r$expert_id[r$round == from_round])
  b <- unique(r$expert_id[r$round == to_round])
  lost <- setdiff(a, b)
  kept <- intersect(a, b)
  .warn_if(length(lost) == 0L, "\u76f8\u90bb\u8f6e\u65e0\u8131\u843d\uff0c\u65e0\u9700\u6bd4\u8f83\u3002")
  r1 <- r[r$round == from_round & !r$unable, ]
  ids <- unique(r1$item_id)
  dims <- unique(r1$dimension)
  rows <- list()
  for (d in dims) {
    for (id in ids) {
      sub <- r1[r1$item_id == id & r1$dimension == d, ]
      xs_keep <- sub$score[sub$expert_id %in% kept]
      xs_lost <- sub$score[sub$expert_id %in% lost]
      pval <- NA_real_
      if (length(xs_keep) >= 3L && length(xs_lost) >= 3L) {
        pval <- tryCatch(
          stats::wilcox.test(xs_keep, xs_lost, exact = FALSE)$p.value,
          error = function(e) NA_real_
        )
      }
      rows[[length(rows) + 1L]] <- data.frame(
        item_id = id,
        dimension = d,
        n_completer = length(xs_keep),
        n_dropout = length(xs_lost),
        median_completer = .safe_median(xs_keep),
        median_dropout = .safe_median(xs_lost),
        p_high_completer = .pct(sum(.in_range(xs_keep, study$protocol$high_range)), length(xs_keep)),
        p_high_dropout = .pct(sum(.in_range(xs_lost, study$protocol$high_range)), length(xs_lost)),
        wilcoxon_p = pval,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "n_dropout") <- length(lost)
  attr(out, "dropout_ids") <- lost
  .add_class(out, "delphi_dropout")
}
