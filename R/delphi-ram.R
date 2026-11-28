# Modified Delphi combined with RAM ----------------------------------------

.delphi_ram_rules <- function() c("table4", "ipras", "percent")

.delphi_ram_relabel <- function(out, method, percent_high, percent_low) {
  n <- nrow(out)
  if (!n) return(out)
  if (!"median_region" %in% names(out)) out$median_region <- NA_character_
  if (!"class_detail" %in% names(out)) out$class_detail <- NA_character_
  if (!identical(method, "percent")) {
    detail <- as.character(out$class_detail)
    out$consensus <- ifelse(
      out$class == "appropriate", "consensus_endorse",
      ifelse(out$class == "inappropriate", "consensus_reject", "no_consensus")
    )
    out$consensus_zh <- ifelse(
      detail == "appropriate", "\u6b63\u5411\u5171\u8bc6",
      ifelse(detail == "inappropriate", "\u8d1f\u5411\u5171\u8bc6",
      ifelse(detail == "uncertain_equipoise", "\u672a\u8fbe\u6210\u5171\u8bc6\uff08\u4e2d\u4f4d\u6570\u5c45\u4e2d\uff09",
      ifelse(detail == "uncertain_disagreement", "\u672a\u8fbe\u6210\u5171\u8bc6\uff08\u5b58\u5728\u5206\u6b67\uff09",
      ifelse(detail == "uncertain_boundary", "\u672a\u8fbe\u6210\u5171\u8bc6\uff08\u8fb9\u754c\u4e2d\u4f4d\u6570\uff09",
             "\u672a\u8fbe\u6210\u5171\u8bc6"))))
    )
    out$decision_basis <- "appropriateness"
    if (!"modified_ram" %in% names(out)) out$modified_ram <- FALSE
    out$modified_ram <- as.logical(out$modified_ram)
    out$modified_ram[is.na(out$modified_ram)] <- FALSE
    return(out)
  }

  ph <- ifelse(out$n_valid > 0, out$n_high / out$n_valid, NA_real_)
  pl <- ifelse(out$n_valid > 0, out$n_low / out$n_valid, NA_real_)
  both <- !is.na(ph) & !is.na(pl) & pl >= percent_low & ph >= percent_low
  endorse <- !both & out$median_region == "high" & !is.na(ph) & !is.na(pl) &
    ph >= percent_high & pl < percent_low
  reject <- !both & !endorse & out$median_region == "low" & !is.na(ph) & !is.na(pl) &
    pl >= percent_high & ph < percent_low
  mid <- !both & !endorse & !reject & out$median_region == "mid"
  boundary <- !both & !endorse & !reject & !mid & out$median_region == "boundary"
  shortfall <- !both & !endorse & !reject & !mid & !boundary & !is.na(out$median)

  out$class <- ifelse(endorse, "appropriate", ifelse(reject, "inappropriate",
              ifelse(is.na(out$median), NA_character_, "uncertain")))
  out$class_detail <- ifelse(
    endorse, "appropriate",
    ifelse(reject, "inappropriate",
    ifelse(both, "uncertain_disagreement",
    ifelse(mid, "uncertain_equipoise",
    ifelse(boundary, "uncertain_boundary",
    ifelse(shortfall, "uncertain_shortfall", NA_character_)))))
  )
  out$consensus <- ifelse(
    endorse, "consensus_endorse",
    ifelse(reject, "consensus_reject",
    ifelse(both, "disagreement",
    ifelse(is.na(out$median), NA_character_, "no_consensus")))
  )
  out$consensus_zh <- ifelse(
    endorse, "\u8fbe\u6210\u5171\u8bc6\uff08\u8d5e\u540c\uff0cRAM \u4fee\u6539\u7248\uff09",
    ifelse(reject, "\u8fbe\u6210\u5171\u8bc6\uff08\u4e0d\u8d5e\u540c\uff0cRAM \u4fee\u6539\u7248\uff09",
    ifelse(both, "\u672a\u8fbe\u6210\u5171\u8bc6\uff08\u4e24\u7aef\u6bd4\u4f8b\u540c\u65f6\u8fbe\u5230\u4e0b\u9650\uff09",
    ifelse(mid, "\u672a\u8fbe\u6210\u5171\u8bc6\uff08\u4e2d\u4f4d\u6570\u5c45\u4e2d\u6216\u6bd4\u4f8b\u672a\u8fc7\u7ebf\uff09",
    ifelse(boundary, "\u672a\u8fbe\u6210\u5171\u8bc6\uff08\u8fb9\u754c\u4e2d\u4f4d\u6570\uff09",
    ifelse(shortfall, "\u672a\u8fbe\u6210\u5171\u8bc6\uff08\u4e2d\u4f4d\u6570\u5728\u6781\u7aef\u533a\u4f46\u672a\u8fbe\u5230\u9884\u8bbe\u6bd4\u4f8b\uff09",
           NA_character_)))))
  )
  out$class_zh <- ifelse(
    endorse, "\u9002\u5b9c\uff08RAM \u4fee\u6539\u7248\uff09",
    ifelse(reject, "\u4e0d\u9002\u5b9c\uff08RAM \u4fee\u6539\u7248\uff09",
    ifelse(both, "\u4e0d\u786e\u5b9a\uff08\u5b58\u5728\u5206\u6b67\uff0cRAM \u4fee\u6539\u7248\uff09",
    ifelse(mid, "\u4e0d\u786e\u5b9a\uff08\u4e2d\u4f4d\u6570\u5c45\u4e2d\uff0cRAM \u4fee\u6539\u7248\uff09",
    ifelse(boundary, "\u4e0d\u786e\u5b9a\uff08\u8fb9\u754c\u4e2d\u4f4d\u6570\uff0cRAM \u4fee\u6539\u7248\uff09",
    ifelse(shortfall, "\u4e0d\u786e\u5b9a\uff08\u6bd4\u4f8b\u672a\u8fbe\u6807\uff0cRAM \u4fee\u6539\u7248\uff09",
           NA_character_)))))
  )
  out$agreement_status <- ifelse(
    both, "disagreement",
    ifelse(endorse | reject, "agreement",
    ifelse(is.na(out$median), NA_character_, "indeterminate"))
  )
  out$agreement <- endorse | reject
  out$disagreement <- both
  out$modified_ram <- TRUE
  out$decision_basis <- "percent_consensus"
  out
}

.protocol_rules_delphi_ram <- function(p) {
  s <- .rand_settings(p)
  rule <- switch(
    s$rule,
    table4 = "\u624b\u518c\u8868 4\uff1a\u6309\u8be5\u6761\u76ee\u5b9e\u9645\u5e94\u7b54\u4eba\u6570\u67e5\u4e00\u81f4\u4e0e\u5206\u6b67\u9608\u503c\uff08Fitch \u7b49\uff0c2001\uff09\u3002\u9002\u5b9c\u6027\u770b\u4e2d\u4f4d\u6570\u533a\u95f4\u4e0e\u662f\u5426\u5206\u6b67\uff0c\u4e0d\u628a\u66f4\u4e25\u683c\u7684\u201c\u4e00\u81f4\u201d\u5f53\u6210\u9002\u5b9c\u7684\u5fc5\u8981\u6761\u4ef6\u3002",
    ipras = sprintf(
      "IPRAS\uff1aIPR = P70 - P30\uff0cIPRAS = %s + %s \u00d7 |5 - IPR \u4e2d\u5fc3|\u3002IPR \u4e25\u683c\u5927\u4e8e IPRAS \u624d\u7b97\u5206\u6b67\u3002\u5206\u4f4d\u6570 type = %s\u3002\u6807\u51c6\u5e38\u6570\u53ea\u7528\u4e8e 1\u20139 \u5206\u3002",
      s$iprr, s$cfa, s$qtype
    ),
    percent = sprintf(
      "\u81ea\u5b9a\u4e49\u767e\u5206\u6bd4\uff08RAM \u4fee\u6539\u7248\uff09\uff1a\u9ad8\u533a\u6bd4\u4f8b \u2265 %.0f%% \u4e14\u4f4e\u533a\u6bd4\u4f8b < %.0f%%\u3001\u4e2d\u4f4d\u6570\u5728\u9ad8\u533a\uff0c\u4e3a\u8d5e\u540c\u5171\u8bc6\uff1b\u4f4e\u533a\u6bd4\u4f8b \u2265 %.0f%% \u4e14\u9ad8\u533a\u6bd4\u4f8b < %.0f%%\u3001\u4e2d\u4f4d\u6570\u5728\u4f4e\u533a\uff0c\u4e3a\u4e0d\u8d5e\u540c\u5171\u8bc6\uff1b\u4e24\u7aef\u6bd4\u4f8b\u90fd \u2265 %.0f%% \u4e3a\u5206\u6b67\u3002\u4e2d\u4f4d\u6570\u5728\u6781\u7aef\u533a\u4f46\u6bd4\u4f8b\u672a\u8fbe\u6807\uff0c\u4ecd\u662f\u672a\u8fbe\u6210\u5171\u8bc6\u3002\u4e0d\u5957\u7528\u8868 4 \u6216 IPRAS \u4f5c\u4e3b\u5224\u5b9a\u3002",
      100 * s$percent_high, 100 * s$percent_low,
      100 * s$percent_high, 100 * s$percent_low, 100 * s$percent_low
    ),
    s$rule
  )
  mode <- switch(
    s$round2_mode,
    mail = "\u518d\u6b21\u51fd\u8be2\uff0c\u6ca1\u6709\u9762\u5bf9\u9762\u4f1a\u8bae",
    online = "\u7ebf\u4e0a\u4f1a\u8bae\u540e\u72ec\u7acb\u590d\u8bc4",
    in_person = "\u9762\u5bf9\u9762\u4f1a\u8bae\u540e\u72ec\u7acb\u590d\u8bc4",
    s$round2_mode
  )
  paste(c(
    sprintf("- **\u6d41\u7a0b\uff1a** \u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM\u3002\u533f\u540d\u591a\u8f6e\u51fd\u8be2\uff0c\u4e0d\u662f\u7ecf\u5178\u4e24\u8f6e\u9762\u5bf9\u9762\u9002\u5b9c\u6027\u4e13\u5bb6\u7ec4\u3002\u540e\u7eed\u8f6e\u4e3a%s\u3002", mode),
    sprintf("- **\u91cf\u8868\uff1a** %s\u2013%s\u3002%s", p$scale_min, p$scale_max,
            if (identical(s$rule, "percent")) {
              "1 \u8868\u793a\u5f3a\u70c8\u53cd\u5bf9\uff0c9 \u8868\u793a\u5f3a\u70c8\u540c\u610f\u3002"
            } else {
              "1 \u8868\u793a\u9884\u671f\u5371\u5bb3\u8fdc\u5927\u4e8e\u83b7\u76ca\uff0c9 \u8868\u793a\u9884\u671f\u83b7\u76ca\u8fdc\u5927\u4e8e\u5371\u5bb3\u3002"
            }),
    sprintf("- **\u9501\u5b9a\u89c4\u5219\uff1a** %s", rule),
    if (identical(s$rule, "percent")) {
      "- **\u9002\u5b9c\u6027\u5217\uff1a** \u7531\u540c\u4e00\u5957\u767e\u5206\u6bd4\u5224\u5b9a\u6620\u5c04\u800c\u6765\uff0c\u5e76\u6807\u4e3a RAM \u4fee\u6539\u7248\u3002"
    } else {
      "- **\u9002\u5b9c\u6027\uff1a** \u4e2d\u4f4d\u6570 7\u20139 \u4e14\u65e0\u5206\u6b67\u4e3a\u9002\u5b9c\uff1b1\u20133 \u4e14\u65e0\u5206\u6b67\u4e3a\u4e0d\u9002\u5b9c\uff1b\u4e2d\u4f4d\u6570 4\u20136\uff0c\u6216\u4efb\u4f55\u5206\u6b67\uff0c\u4e3a\u4e0d\u786e\u5b9a\u3002\u4e2d\u4f4d\u6570 3.5 \u6216 6.5 \u9ed8\u8ba4\u4e3a\u4e0d\u786e\u5b9a\u3002\u5171\u8bc6\u7528\u8bed\u53ea\u662f\u8fd9\u4e9b\u7ed3\u679c\u7684\u62a5\u544a\u540d\u79f0\u3002\u201c\u65e0\u5206\u6b67\u201d\u4e0d\u7b49\u4e8e\u201c\u4e00\u81f4\u201d\u3002"
    },
    sprintf("- **\u8f6e\u6b21\uff1a** \u6700\u591a %s \u8f6e\u3002\u6bcf\u8f6e\u590d\u8bc4\u5168\u90e8\u6761\u76ee\u3002\u5206\u6b67\u53ef\u4ee5\u662f\u6700\u7ec8\u7ed3\u679c\uff0c\u4e0d\u9760\u52a0\u8f6e\u628a\u5206\u6b67\u6d82\u6389\u3002", p$stability$max_rounds %||% 3L),
    "- **\u4e0d\u53c2\u4e0e\u5224\u5b9a\uff1a** \u4e34\u5e8a\u5171\u8bc6\u7684 high_pct\u3001\u6743\u5a01\u7cfb\u6570\u548c Kendall's W \u53ea\u4f5c\u8865\u5145\u6216\u5bf9\u7167\u3002"
  ), collapse = "\n")
}

.delphi_ram_round_text <- function(round, mode) {
  if (round <= 1L) {
    return("\u7b2c 1 \u8f6e\u72ec\u7acb\u3001\u533f\u540d\u8bc4\u5206\uff0c\u4e0d\u8981\u4e0e\u5176\u4ed6\u4e13\u5bb6\u8ba8\u8bba\u3002")
  }
  tail <- switch(
    mode %||% "mail",
    mail = "\u672c\u8f6e\u662f\u518d\u6b21\u51fd\u8be2\uff0c\u6ca0\u6709\u9762\u5bf9\u9762\u4f1a\u8bae\u3002\u8fd9\u662f\u6539\u826f\u5fb7\u5c14\u83f2\uff0c\u4e0d\u662f\u7ecf\u5178 RAM \u7684\u4f1a\u8bae\u540e\u590d\u8bc4\u3002",
    online = "\u672c\u8f6e\u5728\u7ebf\u4e0a\u4f1a\u8bae\u8ba8\u8bba\u4e4b\u540e\u72ec\u7acb\u590d\u8bc4\u3002\u7ebf\u4e0a\u4f1a\u8bae\u662f\u5bf9\u7ecf\u5178\u9762\u5bf9\u9762\u4f1a\u8bae\u7684\u53d8\u901a\u3002",
    in_person = "\u672c\u8f6e\u5728\u9762\u5bf9\u9762\u4f1a\u8bae\u8ba8\u8bba\u4e4b\u540e\u72ec\u7acb\u590d\u8bc4\u3002\u8ba8\u8bba\u4e0d\u80fd\u4ee3\u66ff\u8bc4\u5206\u3002",
    "\u8bf7\u9605\u8bfb\u53cd\u9988\u540e\u72ec\u7acb\u590d\u8bc4\u3002"
  )
  paste0(
    "\u8bf7\u5148\u9605\u8bfb\u533f\u540d\u53cd\u9988\uff0c\u518d\u5bf9\u5168\u90e8\u6761\u76ee\u72ec\u7acb\u590d\u8bc4\u3002",
    "\u5df2\u5224\u4e3a\u9002\u5b9c\u3001\u4e0d\u9002\u5b9c\u6216\u5df2\u8fbe\u6210\u5171\u8bc6\u7684\u6761\u76ee\u4e5f\u8981\u590d\u8bc4\uff0c\u4e0d\u8981\u5254\u9664\u3002",
    tail
  )
}

#' 按锁定的 RAM 规则判定改良德尔菲的一轮结果
#'
#' 只用于场景 `delphi_ram`。表 4 与 IPRAS 的主判定是适宜性：中位数 7–9 且无分歧
#' 为适宜，1–3 且无分歧为不适宜，中位数 4–6 或任何分歧为不确定。共识列只是这些
#' 结果的报告名称。百分比规则的主判定是预先锁定的比例：中位数落在高区但高区
#' 比例未达标时，必须记为未达成共识。这与 [classify_rand()] 的百分比分类不同，
#' 后者仍只服务于 `rand_ucla`。
#'
#' 「无分歧」不是「一致」。`agreement_status` 另行给出 agreement、
#' indeterminate 或 disagreement。适宜不要求达到更严格的一致。
#'
#' @param study [delphi_study()]，方案须为 `delphi_ram`。
#' @param round 轮次。默认最后一轮。
#' @param method 覆盖锁定规则，仅用于敏感性分析，不改写方案。只能是
#'   `"table4"`、`"ipras"` 或 `"percent"`。
#' @param summary 可选的 [summarize_items()] 结果。
#' @param warn 是否对人数不足 7、7 人沿用或超过 16 人外推发出警告。
#'
#' @return 带类 `delphi_ram_class` 的数据框。含 `class`、`class_detail`、
#'   `class_zh`、`consensus`、`consensus_zh`、`agreement_status`、`decision_basis`。
#'
#' @section 输入输出:
#' **输入**评分长表须含 `expert_id`、`item_id`、`round`、`score`。
#' `unable = TRUE` 的评分不进入分母。
#'
#' **输出**每个条目一行。`action` 为 `keep`（适宜或不适宜）或 `discuss`（不确定
#' 或未达成共识）。`decision_basis` 为 `appropriateness` 或 `percent_consensus`。
#'
#' @section 文献:
#' Fitch 等（2001）表 4 与附录 IPRAS。百分比阈值必须在第 1 轮前写明，
#' 并在报告中标为 RAM 修改版。
#'
#' @examples
#' ram <- simulate_delphi_ram("table4")
#' cls <- classify_delphi_ram(ram, round = 1)
#' table(cls$consensus_zh)
#'
#' pct <- simulate_delphi_ram("percent")
#' table(classify_delphi_ram(pct, round = 1)$consensus_zh)
#'
#' @export
classify_delphi_ram <- function(study, round = NULL, method = NULL, summary = NULL,
                                warn = TRUE) {
  study <- .match_study(study)
  p <- study$protocol
  .stop_if(
    !identical(p$scenario, "delphi_ram"),
    "classify_delphi_ram() \u53ea\u7528\u4e8e\u573a\u666f delphi_ram\u3002"
  )
  settings <- .rand_settings(p)
  if (is.null(method)) {
    method <- settings$rule
  } else {
    method <- match.arg(method, .delphi_ram_rules())
  }
  .stop_if(!method %in% .delphi_ram_rules(), "\u89c4\u5219\u53ea\u80fd\u662f table4\u3001ipras \u6216 percent\u3002")
  if (is.null(summary)) summary <- summarize_items(study)
  if ("dimension" %in% names(summary)) {
    summary <- summary[!grepl("\u5fc5\u8981", summary$dimension), , drop = FALSE]
  }
  .stop_if(!nrow(summary), "\u6ca1\u6709\u53ef\u7528\u4e8e\u5206\u7c7b\u7684\u8bc4\u5206\u3002")
  if (is.null(round)) round <- max(summary$round)
  summary <- summary[summary$round == round, , drop = FALSE]
  .stop_if(!nrow(summary), sprintf("\u7b2c %s \u8f6e\u6ca1\u6709\u8bc4\u5206\u3002", round))

  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[r$round == round & !grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  extra <- lapply(seq_len(nrow(summary)), function(i) {
    sub <- r[r$item_id == summary$item_id[i] & r$dimension == summary$dimension[i], , drop = FALSE]
    xs <- .valid_scores(sub$score, sub$unable)
    rand_disagreement(
      xs, method = method, di_threshold = settings$di_threshold, warn = FALSE,
      low = settings$low, mid = settings$mid, high = settings$high,
      iprr = settings$iprr, cfa = settings$cfa, qtype = settings$qtype,
      percent_high = settings$percent_high, percent_low = settings$percent_low,
      boundary = settings$boundary, scale = settings$scale
    )
  })
  extra <- do.call(rbind, extra)
  if (isTRUE(warn) && any(extra$n_valid < 7L)) {
    warning(
      "\u81f3\u5c11\u4e00\u4e2a\u6761\u76ee\u7684\u6709\u6548\u5e94\u7b54\u4e0d\u8db3 7 \u4eba\u3002\u8868 4 \u9608\u503c\u4ecd\u6309 8\u201310 \u4eba\u6cbf\u7528\uff0c\u5206\u7c7b\u4ec5\u4f9b\u63cf\u8ff0\u3002",
      call. = FALSE
    )
  } else if (isTRUE(warn) && any(extra$n_valid == 7L)) {
    warning("\u5b58\u5728 7 \u4eba\u5e94\u7b54\u7684\u6761\u76ee\uff0c\u5df2\u6cbf\u7528 8\u201310 \u4eba\u9608\u503c\u3002", call. = FALSE)
  }
  if (isTRUE(warn) && any(extra$extrapolated)) {
    warning("\u6709\u6761\u76ee\u5e94\u7b54\u4eba\u6570\u8d85\u8fc7 16\uff0c\u9608\u503c\u4e3a\u5916\u63a8\uff0c\u62a5\u544a\u4e2d\u987b\u6807\u660e\u3002", call. = FALSE)
  }
  keep <- setdiff(names(extra), names(summary))
  out <- cbind(summary, extra[, keep, drop = FALSE])
  out <- .delphi_ram_relabel(out, method, settings$percent_high, settings$percent_low)
  out$action <- ifelse(out$class %in% c("appropriate", "inappropriate"), "keep", "discuss")
  out$action[is.na(out$class)] <- NA_character_
  out$rule_set <- "delphi_ram"
  out$rule_used <- method
  out$exploratory <- !identical(method, settings$rule)
  rownames(out) <- NULL
  .add_class(out, "delphi_ram_class")
}

#' 比较另外两种 RAM 规则，不替换锁定分类
#'
#' 用表 4、IPRAS 和百分比规则重算同一轮，并给出适宜性分类的 Cohen's \eqn{\kappa}。
#' 结果是探索性的。正式结论仍用方案里锁定的那一种规则。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。场景须为 `delphi_ram`。
#' @param round 轮次。默认最后一轮。
#'
#' @return 列表：`by_item` 为各规则分类，`kappa` 为两两一致性，`locked_rule`
#'   为方案规则，`note` 说明不得据此改规则。
#'
#' @section 输入输出:
#' **输入**为已经锁定 `delphi_ram` 方案并含评分的研究。
#'
#' **输出**的 `class` 列是适宜 / 不确定 / 不适宜。百分比规则下，中位数高但比例
#' 未达标会记为不确定，因此可能与表 4 不同。
#'
#' @examples
#' ram <- simulate_delphi_ram("table4")
#' sens <- delphi_ram_sensitivity(ram, round = 1)
#' sens$kappa
#'
#' @export
delphi_ram_sensitivity <- function(study, round = NULL) {
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  .stop_if(!identical(study$protocol$scenario, "delphi_ram"),
           "delphi_ram_sensitivity() \u53ea\u7528\u4e8e\u573a\u666f delphi_ram\u3002")
  locked <- .rand_settings(study$protocol)$rule
  if (is.null(round)) {
    r0 <- .ensure_dimension(.ensure_unable(study$ratings))
    round <- max(r0$round)
  }
  pieces <- list()
  skipped <- character()
  for (rule in .delphi_ram_rules()) {
    one <- tryCatch(
      classify_delphi_ram(study, round = round, method = rule, warn = FALSE),
      error = function(e) {
        skipped <<- c(skipped, paste0(rule, ": ", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(one)) {
      one$rule <- rule
      pieces[[rule]] <- one[, c("item_id", "dimension", "class", "consensus", "class_zh", "consensus_zh")]
    }
  }
  .stop_if(!length(pieces), "\u6ca1\u6709\u4efb\u4f55\u89c4\u5219\u80fd\u5b8c\u6210\u5206\u7c7b\u3002")
  by_item <- pieces[[1]][, c("item_id", "dimension")]
  for (rule in names(pieces)) {
    key <- paste(by_item$item_id, by_item$dimension)
    src <- paste(pieces[[rule]]$item_id, pieces[[rule]]$dimension)
    by_item[[paste0("class_", rule)]] <- pieces[[rule]]$class[match(key, src)]
    by_item[[paste0("consensus_", rule)]] <- pieces[[rule]]$consensus_zh[match(key, src)]
  }
  by_item$locked_rule <- locked
  others <- setdiff(names(pieces), locked)
  if (length(others)) {
    alt <- vapply(others, function(rule) {
      by_item[[paste0("class_", locked)]] == by_item[[paste0("class_", rule)]]
    }, logical(nrow(by_item)))
    if (is.null(dim(alt))) alt <- cbind(alt)
    by_item$matches_locked <- apply(alt, 1, all)
  } else {
    by_item$matches_locked <- TRUE
  }
  pairs <- utils::combn(names(pieces), 2, simplify = FALSE)
  kappa <- do.call(rbind, lapply(pairs, function(pr) {
    data.frame(
      rule_a = pr[1],
      rule_b = pr[2],
      kappa = .cohen_kappa(pieces[[pr[1]]]$class, pieces[[pr[2]]]$class),
      n = nrow(pieces[[pr[1]]]),
      exploratory = TRUE,
      stringsAsFactors = FALSE
    )
  }))
  list(
    by_item = by_item,
    kappa = kappa,
    locked_rule = locked,
    skipped = skipped,
    note = "\u654f\u611f\u6027\u5206\u6790\u4e0d\u66ff\u6362\u5df2\u9501\u5b9a\u89c4\u5219\u3002\u82e5\u4e24\u5957\u89c4\u5219\u7ed3\u8bba\u4e0d\u540c\uff0c\u6b63\u5f0f\u7ed3\u679c\u4ecd\u6309\u7b2c 1 \u8f6e\u524d\u5199\u5165\u65b9\u6848\u7684\u90a3\u4e00\u79cd\u3002"
  )
}

.questionnaire_delphi_ram_round1 <- function(study) {
  p <- study$protocol
  meta <- study$meta
  dims <- .questionnaire_dimensions(study, 1L)
  blocks <- apply(study$items, 1, function(row) {
    likert <- paste(vapply(dims, function(d) .likert_line(p, d), character(1)), collapse = "\n")
    paste0(
      "### ", row[["item_id"]], "\n\n",
      "**", row[["item_text"]], "**\n\n",
      likert, "\n",
      "**\u610f\u89c1\uff08\u53ef\u7a7a\uff0c\u4e0d\u8981\u5199\u59d3\u540d\uff09\uff1a** ________________________________\n"
    )
  })
  paste0(
    "# \u7b2c 1 \u8f6e\u6539\u826f\u5fb7\u5c14\u83f2\uff08\u7ed3\u5408 RAM\uff09\n\n",
    "**\u7814\u7a76\u9898\u76ee\uff1a** ", meta$title %||% "\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM", "\n\n",
    "**\u76ee\u7684\uff1a** ", meta$objective %||% "\u7528\u9884\u5148\u9501\u5b9a\u7684 RAM \u89c4\u5219\u5224\u65ad\u4e00\u81f4\u3001\u5206\u6b67\u4e0e\u5171\u8bc6\u3002", "\n\n",
    "**\u76ee\u6807\u4f7f\u7528\u8005\uff1a** ", meta$users %||% "\u672a\u8bf4\u660e", "\n\n",
    "**\u5730\u7406\u8303\u56f4\uff1a** ", meta$geography %||% "\u672a\u8bf4\u660e", "\n\n",
    "> ", .delphi_ram_round_text(1L, .rand_settings(p)$round2_mode), "\n\n",
    "\u65e0\u6cd5\u5224\u65ad\u8bf7\u52fe\u9009\uff0c\u4e0d\u8981\u586b 5 \u5206\u5145\u6570\u3002\u65e0\u6cd5\u5224\u65ad\u4e0d\u8fdb\u5165\u4e2d\u4f4d\u6570\u548c\u5206\u6b67\u5224\u5b9a\u3002\n\n",
    "## \u5df2\u9501\u5b9a\u7684\u5224\u5b9a\u89c4\u5219\n\n",
    .protocol_rules_delphi_ram(p), "\n\n",
    "**\u4e0d\u786e\u5b9a\u548c\u5206\u6b67\u90fd\u662f\u6b63\u5f0f\u7ed3\u679c\u3002** \u89c4\u5219\u4e0d\u4f1a\u6839\u636e\u7ed3\u679c\u56de\u6539\u3002\n\n",
    "## \u6761\u76ee\n\n",
    paste(blocks, collapse = "\n")
  )
}

.questionnaire_delphi_ram_followup <- function(study, round, expert_ids = NULL) {
  p <- study$protocol
  settings <- .rand_settings(p)
  src <- as.integer(round) - 1L
  cls <- classify_delphi_ram(study, round = src, warn = FALSE)
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[r$round == src & !grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  if (is.null(expert_ids)) expert_ids <- unique(as.character(study$panel$expert_id))
  comments <- study$comments
  docs <- vapply(expert_ids, function(eid) {
    blocks <- vapply(unique(cls$item_id), function(iid) {
      row <- cls[cls$item_id == iid, , drop = FALSE][1, ]
      own <- r[as.character(r$expert_id) == eid & r$item_id == iid, , drop = FALSE]
      own_txt <- if (!nrow(own)) {
        "\u2014"
      } else if (isTRUE(own$unable[1]) || is.na(own$score[1])) {
        "\u65e0\u6cd5\u5224\u65ad"
      } else {
        as.character(own$score[1])
      }
      note <- ""
      if (!is.null(comments) && nrow(comments)) {
        cc <- comments[comments$round == src & comments$item_id == iid, , drop = FALSE]
        if (nrow(cc) && "comment" %in% names(cc)) {
          note <- paste0("**\u533f\u540d\u610f\u89c1\uff1a** ", paste(utils::head(unique(cc$comment), 3), collapse = " | "), "\n\n")
        }
      }
      label <- if ("consensus_zh" %in% names(row) && nzchar(row$consensus_zh[1] %||% "")) {
        row$consensus_zh[1]
      } else {
        row$class_zh[1]
      }
      paste0(
        "### ", iid, "\n\n",
        "**", study$items$item_text[match(iid, study$items$item_id)], "**\n\n",
        "| \u60a8\u7684\u4e0a\u8f6e\u8bc4\u5206 | \u4e2d\u4f4d\u6570 | 1\u20133 | 4\u20136 | 7\u20139 | \u5206\u6b67 | \u5206\u7c7b |\n",
        "|---|---|---|---|---|---|---|\n",
        sprintf("| %s | %s | %s | %s | %s | %s | %s |\n\n",
                own_txt,
                ifelse(is.na(row$median), "\u2014", row$median),
                row$n_low, row$n_mid, row$n_high,
                if (isTRUE(row$disagreement)) "\u662f" else "\u5426",
                label),
        note,
        .likert_line(p, "\u672c\u8f6e\u590d\u8bc4"), "\n",
        "**\u672c\u8f6e\u610f\u89c1\uff08\u53ef\u7a7a\uff09\uff1a** ________________________________\n"
      )
    }, character(1))
    paste0(
      sprintf("# \u7b2c %s \u8f6e\u6539\u826f\u5fb7\u5c14\u83f2\u53cd\u9988\u540e\u590d\u8bc4\n\n", round),
      "**\u7814\u7a76\u9898\u76ee\uff1a** ", study$meta$title %||% "\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM", "\n\n",
      "**\u672c\u5377\u4e13\u5bb6\u7f16\u53f7\uff1a** ", eid, "\n\n",
      .delphi_ram_round_text(round, settings$round2_mode), "\n\n",
      "\u4e0b\u8868\u53ea\u663e\u793a\u7ec4\u5185\u5206\u5e03\u548c\u60a8\u672c\u4eba\u7684\u4e0a\u8f6e\u8bc4\u5206\uff0c\u4e0d\u51fa\u73b0\u5176\u4ed6\u4e13\u5bb6\u59d3\u540d\u3002\n\n",
      paste(blocks, collapse = "\n")
    )
  }, character(1))
  names(docs) <- expert_ids
  docs
}

#' 生成改良德尔菲结合 RAM 的问卷
#'
#' 第 1 轮只说明锁定规则，不展示他人分数。之后每一轮给出本人上轮分数、
#' 1–3 / 4–6 / 7–9 人数、中位数和分歧标记，并要求对全部条目复评。
#'
#' @param study [delphi_study()]，场景须为 `delphi_ram`。
#' @param round 正整数，且不得超过方案 `max_rounds`。
#' @param expert_id 要生成的专家。缺省为全体。
#' @param format `"docx"`、`"xlsx"` 或 `"both"`。
#' @param file 文件或目录。`both` 时用同一主文件名写出两种文件。
#' @param prefill 若研究里已经有该轮分数，是否写入 xlsx。默认否。
#'
#' @return 不可见的写出路径。
#'
#' @section 输入输出:
#' **输入**条目表须含 `item_id`、`item_text`。专家表须有唯一 `expert_id`。
#' 第 2 轮及以后需要上一轮评分。
#'
#' **输出** docx 供发放，xlsx 供回收后用 [read_survey_xlsx()] 读回。
#' 工作表与 [delphi_questionnaire()] 相同：说明、方案、专家、条目、评分、意见。
#' 评分维度在表 4 / IPRAS 下为「适宜性」，在百分比规则下为「同意程度」。
#'
#' @examples
#' \donttest{
#' ram <- simulate_delphi_ram("table4")
#' d <- tempfile("dram-")
#' dir.create(d)
#' delphi_ram_questionnaire(ram, round = 1, format = "both", file = d)
#' }
#'
#' @export
delphi_ram_questionnaire <- function(study, round = 1,
                                     expert_id = NULL,
                                     format = c("both", "docx", "xlsx"),
                                     file = NULL,
                                     prefill = FALSE) {
  format <- match.arg(format)
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  .stop_if(
    !identical(study$protocol$scenario, "delphi_ram"),
    "delphi_ram_questionnaire() \u53ea\u7528\u4e8e\u573a\u666f delphi_ram\u3002"
  )
  round <- as.integer(round)[1]
  .stop_if(is.na(round) || round < 1L, "round \u5fc5\u987b\u662f\u6b63\u6574\u6570\u3002")
  maxr <- study$protocol$stability$max_rounds %||% 3L
  .stop_if(round > maxr, sprintf("round=%s \u8d85\u8fc7\u65b9\u6848 max_rounds=%s\u3002", round, maxr))
  if (round >= 2L) {
    .stop_if(is.null(study$ratings) || !nrow(study$ratings),
             "\u751f\u6210\u53cd\u9988\u8f6e\u95ee\u5377\u9700\u8981\u5df2\u6709\u4e0a\u4e00\u8f6e\u8bc4\u5206\u3002")
    .stop_if(!(round - 1L) %in% study$ratings$round,
             sprintf("\u7f3a\u5c11\u7b2c %s \u8f6e\u8bc4\u5206\u3002", round - 1L))
  }
  ids <- if (is.null(expert_id)) unique(as.character(study$panel$expert_id)) else as.character(expert_id)
  rule <- .rand_settings(study$protocol)$rule
  dim_name <- if (identical(rule, "percent")) "\u540c\u610f\u7a0b\u5ea6" else "\u9002\u5b9c\u6027"
  study2 <- study
  attr(study2, "rand_force_dimension") <- dim_name
  attr(study2, "rand_force_round") <- as.integer(round)
  if (round <= 1L) {
    docs <- .questionnaire_delphi_ram_round1(study2)
    names(docs) <- "round1"
  } else {
    docs <- .questionnaire_delphi_ram_followup(study2, round, ids)
  }
  .write_questionnaires(docs, study2, round, format, file, "all", ids, NULL, FALSE, prefill)
}

#' 根据本轮结果生成下一轮改良德尔菲问卷
#'
#' 等价于 `delphi_ram_questionnaire(result, round = last_round + 1)`。
#' 每一轮都复评全部条目。超过方案 `max_rounds` 时停止。
#'
#' @param result [run_delphi_ram()] 或 [analyze_delphi()] 的结果。
#' @param format `"docx"`、`"xlsx"` 或 `"both"`。
#' @param file 输出路径或目录。
#' @param ... 传给 [delphi_ram_questionnaire()]。
#'
#' @return 见 [delphi_ram_questionnaire()]。
#'
#' @section 输入输出:
#' **输入**须已经完成至少一轮 `delphi_ram` 分析。
#'
#' **输出**为下一轮 docx 和/或 xlsx。不剔除已经适宜或已经达成共识的条目。
#'
#' @examples
#' \donttest{
#' ram <- simulate_delphi_ram("ipras")
#' res <- run_delphi_ram(ram, round = 1)
#' d <- tempfile("dram2-")
#' next_delphi_ram_questionnaire(res, format = "xlsx", file = d)
#' }
#'
#' @export
next_delphi_ram_questionnaire <- function(result, format = "both", file = NULL, ...) {
  .stop_if(!inherits(result, "delphi_result"), "\u8bf7\u63d0\u4f9b analyze_delphi() \u6216 run_delphi_ram() \u7ed3\u679c\u3002")
  .stop_if(
    !identical(result$study$protocol$scenario, "delphi_ram"),
    "next_delphi_ram_questionnaire() \u53ea\u7528\u4e8e\u573a\u666f delphi_ram\u3002"
  )
  nxt <- as.integer(result$last_round) + 1L
  maxr <- result$study$protocol$stability$max_rounds %||% 3L
  .stop_if(nxt > maxr, sprintf("\u5df2\u8fbe\u65b9\u6848\u89c4\u5b9a\u7684\u6700\u591a\u8f6e\u6b21 %s\u3002", maxr))
  delphi_ram_questionnaire(result, round = nxt, format = format, file = file, ...)
}

.delphi_ram_meta <- function(study) {
  p <- study$protocol
  s <- .rand_settings(p)
  list(
    rule = s$rule,
    round_mode = s$round2_mode,
    purpose = s$purpose,
    max_rounds = p$stability$max_rounds %||% 3L,
    percent_high = s$percent_high,
    percent_low = s$percent_low,
    modified_ram = identical(s$rule, "percent") || isTRUE(p$rand$nonstandard_ipras),
    deviation = "\u533f\u540d\u591a\u8f6e\u51fd\u8be2\uff0c\u9ed8\u8ba4\u6700\u591a 3 \u8f6e\uff0c\u4e0d\u662f\u7ecf\u5178 RAM \u7684\u4e24\u8f6e\u9762\u5bf9\u9762\u4f1a\u8bae\u3002\u7b2c 3 \u8f6e\u4e0d\u7528\u6765\u6d88\u9664\u5206\u6b67\u3002",
    supplement = "\u6743\u5a01\u7cfb\u6570\u4e0e Kendall's W \u53ea\u4f5c\u8865\u5145\uff0c\u4e0d\u66ff\u4ee3\u4e2d\u4f4d\u6570\u3001\u5206\u6b67\u548c\u9002\u5b9c\u6027\u6216\u767e\u5206\u6bd4\u5171\u8bc6\u5206\u7c7b\u3002"
  )
}

#' 完成改良德尔菲结合 RAM 的分析，并按需写出报告
#'
#' 对锁定规则做逐轮适宜性与共识分类、稳定性、规则敏感性和补充指标。
#' 临床共识百分比不参与分类。权威系数和 Kendall's W 若能计算，只作为补充。
#'
#' @param study [delphi_study()] 或已有的分析结果。场景须为 `delphi_ram`。
#' @param round `"last"` 或正整数。整数表示只分析到该轮（含此前轮次）。
#' @param report 是否写报告。默认否。
#' @param format `"docx"`、`"html"`、`"word"` 或 `"both"`。选择 html 时仍会另写 docx。
#' @param file 报告路径或目录。交互会话中留空时可弹出文件选择；否则写入临时目录。
#' @param open 是否打开报告。
#' @param ... 传给 [analyze_delphi()]。
#'
#' @return `delphi_result`。附加 `delphi_ram_meta` 与 `sensitivity`。
#'   若写了报告，属性 `report_file` 为路径。
#'
#' @section 输入输出:
#' **输入**为 [delphi_ram_protocol()] 锁定并已读入评分的研究。
#'
#' **输出**分类表同时有适宜性与共识列。报告至少含 docx，写明规则出处、
#' 与经典 RAM 面对面流程的差别、各轮结果、分歧清单和敏感性。
#'
#' @examples
#' ram <- simulate_delphi_ram("table4")
#' res <- run_delphi_ram(ram, report = FALSE)
#' table(res$last_classification$consensus_zh)
#' \donttest{
#' f <- run_delphi_ram(ram, report = TRUE, format = "docx",
#'                     file = tempfile(fileext = ".docx"))
#' attr(f, "report_file")
#' }
#'
#' @export
run_delphi_ram <- function(study, round = "last", report = FALSE,
                           format = c("docx", "html", "word", "both"),
                           file = NULL, open = FALSE, ...) {
  format <- match.arg(format)
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  .stop_if(
    !identical(study$protocol$scenario, "delphi_ram"),
    "run_delphi_ram() \u53ea\u7528\u4e8e\u573a\u666f delphi_ram\u3002\u7ecf\u5178\u9002\u5b9c\u6027\u4e13\u5bb6\u7ec4\u8bf7\u7528 run_rand()\uff0c\u666e\u901a Delphi \u8bf7\u7528 run_delphi()\u3002"
  )
  up <- NULL
  if (!identical(round, "last")) {
    up <- as.integer(round)[1]
    .stop_if(is.na(up) || up < 1L, "round \u5fc5\u987b\u662f \"last\" \u6216\u6b63\u6574\u6570\u3002")
  }
  result <- analyze_delphi(study, up_to_round = up, ...)
  result$delphi_ram_meta <- .delphi_ram_meta(result$study)
  result$sensitivity <- tryCatch(
    delphi_ram_sensitivity(result, round = result$last_round),
    error = function(e) list(error = conditionMessage(e))
  )
  if (!isTRUE(report)) return(result)
  if (identical(format, "html")) format <- "both"
  if (is.null(file)) {
    if (isTRUE(interactive())) {
      message("\u8bf7\u9009\u62e9\u62a5\u544a\u4fdd\u5b58\u4f4d\u7f6e\uff08docx\uff09\u3002")
      file <- tryCatch(utils::file.choose(), error = function(e) NULL)
    }
    if (is.null(file) || !nzchar(file[1])) {
      file <- tempfile("delphi-ram-report-", fileext = ".docx")
      message("\u672a\u6307\u5b9a file\uff0c\u62a5\u544a\u5199\u5165: ", file)
    }
  }
  files <- delphi_report(result, file = file, format = format, template = "delphi_ram", open = open)
  attr(result, "report_file") <- files
  result
}

.delphi_ram_panel <- function(n, prefix) {
  data.frame(
    expert_id = sprintf("%s%02d", prefix, seq_len(n)),
    stakeholder = rep(c("\u4e34\u5e8a", "\u65b9\u6cd5\u5b66"), length.out = n),
    specialty = rep(c("\u5fc3\u5185\u79d1", "\u5168\u79d1", "\u62a4\u7406", "\u836f\u5b66", "\u7edf\u8ba1", "\u65b9\u6cd5\u5b66"), length.out = n),
    title = "\u59d4\u5458",
    years = 8 + seq_len(n),
    invited = TRUE,
    stringsAsFactors = FALSE
  )
}

.delphi_ram_stack <- function(panel, item_id, rounds, dimension) {
  do.call(rbind, lapply(seq_along(rounds), function(k) {
    data.frame(
      expert_id = panel$expert_id,
      item_id = item_id,
      round = k,
      score = rounds[[k]],
      dimension = dimension,
      unable = FALSE,
      stakeholder = panel$stakeholder,
      stringsAsFactors = FALSE
    )
  }))
}

.delphi_ram_items <- function(ids, texts) {
  data.frame(
    item_id = ids,
    item_text = texts,
    domain = "\u63a8\u8350\u610f\u89c1",
    evidence_level = rep(c("B", "C", "B", "C", "A", "C"), length.out = length(ids)),
    stringsAsFactors = FALSE
  )
}

#' 生成改良德尔菲结合 RAM 的教学数据
#'
#' 分数事先排好，使适宜、不适宜、中位数居中、分歧都会出现。百分比规则另外
#' 包含一条中位数已在高区、但高区比例未达到 70% 的条目。表 4 与 IPRAS
#' 使用同一套 9 人分数，其中一条在两种规则下结论不同。百分比样本为 12 人。
#'
#' @param rule `"table4"`、`"ipras"` 或 `"percent"`。
#'
#' @return `delphi_study`。三轮评分。数据为教学模拟，不是真实指南。
#'
#' @section 输入输出:
#' **输入**为规则名称。
#'
#' **输出**可直接交给 [classify_delphi_ram()]、[delphi_ram_questionnaire()]
#' 和 [run_delphi_ram()]。安装后的固定副本见 `delphi_ram_table4_demo`、
#' `delphi_ram_ipras_demo` 与 `delphi_ram_percent_demo`。
#'
#' @examples
#' tab <- simulate_delphi_ram("table4")
#' tab$protocol$rand$rule
#' table(classify_delphi_ram(tab, round = 1)$class_detail)
#'
#' ip <- simulate_delphi_ram("ipras")
#' classify_delphi_ram(ip, round = 1)$class_detail[
#'   classify_delphi_ram(ip, round = 1)$item_id == "ST05"
#' ]
#'
#' pct <- simulate_delphi_ram("percent")
#' classify_delphi_ram(pct, round = 1)[
#'   c("item_id", "median", "consensus_zh")
#' ]
#'
#' @export
simulate_delphi_ram <- function(rule = c("table4", "ipras", "percent")) {
  rule <- match.arg(rule)
  if (identical(rule, "percent")) {
    panel <- .delphi_ram_panel(12L, "Q")
    dimension <- "\u540c\u610f\u7a0b\u5ea6"
    texts <- c(
      "\u5efa\u8bae\u5728\u542f\u52a8\u6297\u51dd\u524d\u5b8c\u6210\u57fa\u7ebf\u51fa\u8840\u98ce\u9669\u8bc4\u4f30\u3002",
      "\u5efa\u8bae\u5bf9\u6240\u6709\u62e9\u671f\u624b\u672f\u4e00\u5f8b\u505c\u7528\u963f\u53f8\u5339\u6797\uff0c\u4e0d\u5206\u5371\u9669\u3002",
      "\u5efa\u8bae\u628a\u672f\u524d\u68c0\u67e5\u7f29\u51cf\u4e3a\u56e2\u961f\u81ea\u884c\u51b3\u5b9a\uff0c\u4e0d\u5199\u5165\u63a8\u8350\u3002",
      "\u5efa\u8bae\u540c\u65f6\u628a\u8be5\u9879\u63aa\u65bd\u5199\u6210\u5fc5\u987b\u505a\u548c\u4e0d\u5e94\u8be5\u505a\u3002",
      "\u5efa\u8bae\u5728\u8bc1\u636e\u4e0d\u8db3\u65f6\u4ecd\u5199\u51fa\u5f3a\u63a8\u8350\u3002",
      "\u5efa\u8bae\u5411\u60a3\u8005\u8bf4\u660e\u6297\u51dd\u7684\u83b7\u76ca\u4e0e\u51fa\u8840\u98ce\u9669\u540e\u518d\u51b3\u5b9a\u3002"
    )
    bank <- list(
      PC01 = list(
        c(7, 8, 8, 8, 8, 9, 9, 9, 8, 7, 8, 9),
        c(8, 8, 8, 8, 8, 9, 9, 9, 8, 8, 8, 9),
        c(8, 8, 8, 8, 8, 9, 9, 9, 8, 8, 8, 9)
      ),
      PC02 = list(
        c(1, 1, 2, 2, 2, 3, 1, 2, 2, 3, 1, 2),
        c(1, 1, 2, 2, 2, 2, 1, 2, 2, 3, 1, 2),
        c(1, 1, 2, 2, 2, 2, 1, 2, 2, 3, 1, 2)
      ),
      PC03 = list(
        c(4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 4),
        c(4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 4),
        c(4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 5, 4)
      ),
      PC04 = list(
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 2, 8),
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 2, 8),
        c(1, 2, 2, 4, 5, 6, 7, 8, 9, 9, 3, 8)
      ),
      PC05 = list(
        c(6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9),
        c(6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9),
        c(6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 9)
      ),
      PC06 = list(
        c(6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9),
        c(6, 6, 6, 7, 7, 8, 8, 8, 9, 9, 8, 7),
        c(6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 8, 8)
      )
    )
    objective <- "\u7528\u9884\u5148\u9501\u5b9a\u7684 70/15 \u767e\u5206\u6bd4\u89c4\u5219\u5224\u65ad\u63a8\u8350\u610f\u89c1\u662f\u5426\u8fbe\u6210\u5171\u8bc6\u3002\u8fd9\u662f RAM \u4fee\u6539\u7248\uff0c\u4e0d\u662f\u7ecf\u5178\u9002\u5b9c\u6027\u8bc4\u4ef7\u3002"
  } else {
    panel <- .delphi_ram_panel(9L, "E")
    dimension <- "\u9002\u5b9c\u6027"
    texts <- c(
      "\u5bf9\u9ad8\u5371\u51fa\u8840\u60a3\u8005\uff0c\u542f\u52a8\u6297\u51dd\u524d\u5b8c\u6210\u57fa\u7ebf\u5b9e\u9a8c\u5ba4\u68c0\u67e5\u662f\u5426\u9002\u5b9c\u3002",
      "\u5bf9\u4f4e\u5371\u62e9\u671f\u624b\u672f\u4e00\u5f8b\u505c\u7528\u963f\u53f8\u5339\u6797\u662f\u5426\u9002\u5b9c\u3002",
      "\u628a\u672f\u524d\u68c0\u67e5\u5b8c\u5168\u4ea4\u7ed9\u4e2a\u4eba\u7ecf\u9a8c\u3001\u4e0d\u5199\u5165\u5171\u8bc6\u662f\u5426\u9002\u5b9c\u3002",
      "\u5728\u83b7\u76ca\u4e0e\u5371\u5bb3\u90fd\u4e0d\u6e05\u695a\u65f6\u4ecd\u5199\u6210\u5f3a\u63a8\u8350\u662f\u5426\u9002\u5b9c\u3002",
      "\u5bf9\u4e2d\u5371\u60a3\u8005\u4e8e\u672f\u524d\u505c\u7528\u4e00\u79cd\u6297\u8840\u5c0f\u677f\u836f\u662f\u5426\u9002\u5b9c\u3002",
      "\u5728\u591a\u6570\u4e13\u5bb6\u8d5e\u540c\u4f46\u4ecd\u6709\u4e00\u5c0f\u90e8\u5206\u4eba\u4fdd\u7559\u610f\u89c1\u65f6\uff0c\u662f\u5426\u628a\u8be5\u6761\u76ee\u5199\u6210\u5df2\u8fbe\u767e\u5206\u6bd4\u5171\u8bc6\u3002"
    )
    bank <- list(
      ST01 = list(
        c(8, 8, 9, 7, 8, 9, 8, 7, 9),
        c(8, 8, 9, 8, 8, 9, 8, 8, 9),
        c(8, 8, 9, 8, 8, 9, 8, 8, 9)
      ),
      ST02 = list(
        c(1, 2, 2, 1, 3, 2, 1, 2, 2),
        c(1, 2, 2, 1, 2, 2, 1, 2, 2),
        c(1, 1, 2, 1, 2, 2, 1, 2, 2)
      ),
      ST03 = list(
        c(4, 5, 5, 5, 5, 5, 5, 6, 6),
        c(4, 4, 5, 5, 5, 5, 5, 6, 6),
        c(5, 5, 5, 5, 5, 5, 6, 6, 4)
      ),
      ST04 = list(
        c(1, 2, 2, 3, 5, 7, 8, 9, 9),
        c(1, 2, 2, 3, 5, 7, 8, 9, 9),
        c(1, 2, 2, 3, 5, 7, 8, 9, 8)
      ),
      ST05 = list(
        c(2, 3, 3, 7, 8, 8, 8, 9, 9),
        c(2, 3, 3, 7, 8, 8, 8, 9, 9),
        c(2, 3, 3, 7, 8, 8, 8, 9, 9)
      ),
      ST06 = list(
        c(6, 6, 6, 7, 7, 7, 8, 8, 9),
        c(6, 6, 6, 7, 7, 7, 8, 8, 9),
        c(6, 6, 7, 7, 7, 7, 8, 8, 9)
      )
    )
    objective <- if (identical(rule, "ipras")) {
      "\u7528 IPRAS \u5224\u5b9a\u5206\u6b67\uff0c\u518d\u6309\u4e2d\u4f4d\u6570\u533a\u95f4\u7ed9\u51fa\u9002\u5b9c\u6027\u3002\u6d41\u7a0b\u662f\u533f\u540d\u4e09\u8f6e\u51fd\u8be2\uff0c\u4e0d\u662f\u7ecf\u5178\u4e24\u8f6e\u9762\u5bf9\u9762\u4e13\u5bb6\u7ec4\u3002"
    } else {
      "\u7528\u624b\u518c\u8868 4 \u5224\u5b9a\u5206\u6b67\uff0c\u518d\u6309\u4e2d\u4f4d\u6570\u533a\u95f4\u7ed9\u51fa\u9002\u5b9c\u6027\u3002\u6d41\u7a0b\u662f\u533f\u540d\u4e09\u8f6e\u51fd\u8be2\uff0c\u4e0d\u662f\u7ecf\u5178\u4e24\u8f6e\u9762\u5bf9\u9762\u4e13\u5bb6\u7ec4\u3002"
    }
  }
  ids <- names(bank)
  rows <- lapply(ids, function(id) .delphi_ram_stack(panel, id, bank[[id]], dimension))
  protocol <- delphi_ram_protocol(rule)
  meta <- list(
    title = sprintf("\u56f4\u672f\u671f\u6297\u51dd\u6539\u826f\u5fb7\u5c14\u83f2\uff08\u6559\u5b66\u6a21\u62df\uff0c%s\uff09", rule),
    objective = objective,
    users = "\u56f4\u672f\u671f\u5de5\u4f5c\u7ec4",
    geography = "\u6559\u5b66\u793a\u4f8b",
    evidence_note = "\u6761\u76ee\u6765\u81ea\u6a21\u62df\u7684\u8bc1\u636e\u6458\u8981\uff0c\u4e0d\u662f GRADE \u63a8\u8350\u3002",
    anonymity = "\u4e09\u8f6e\u5747\u4e3a\u533f\u540d\u51fd\u8be2\u3002\u53cd\u9988\u53ea\u663e\u793a\u7ec4\u5185\u5206\u5e03\u548c\u672c\u4eba\u4e0a\u8f6e\u8bc4\u5206\u3002",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\u3002"
  )
  comments <- data.frame(
    expert_id = panel$expert_id[1],
    item_id = ids[4],
    round = 1L,
    comment = "\u4e24\u7aef\u610f\u89c1\u5dee\u8ddd\u5927\uff0c\u5efa\u8bae\u4fdd\u7559\u5206\u6b67\u800c\u4e0d\u662f\u518d\u6295\u4e00\u8f6e\u5f3a\u5236\u5408\u5e76\u3002",
    theme = "\u5206\u6b67",
    stringsAsFactors = FALSE
  )
  delphi_study(
    protocol = protocol,
    panel = panel,
    items = .delphi_ram_items(ids, texts),
    ratings = do.call(rbind, rows),
    comments = comments,
    meta = meta,
    validate = TRUE
  )
}
