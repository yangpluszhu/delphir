# 第一轮新增条目：编码审查、双通道、迭代与审计轨迹
# 软件不把自由文本自动改写成条目。研究组完成编码后，这里只执行事先锁定的规则。

#' 整理第一轮结构化新增建议
#'
#' 一行是一名专家提出的一条建议。同一专家把同一概念写了多次，
#' 后续审查只计 1 人。本函数不做归并，归并后的 `concept_id` 由研究组填写。
#'
#' @param data 数据框。必填 `expert_id`、`raw_name`。建议同时有
#'   `meaning`（含义）、`rationale`（理由）、`distinction`（与现有条目的区别）、
#'   `evidence_note`（文献、指南、访谈或临床依据）、`concept_id`（编码后的概念）、
#'   `round`、`slot`。
#'
#' @return 带类 `"delphi_suggestions"` 的数据框。空名称的行会被去掉。
#'
#' @examples
#' delphi_suggestions(data.frame(
#'   expert_id = c("E1", "E1"),
#'   raw_name = c("睡眠质量", "睡眠质量"),
#'   concept_id = c("C1", "C1"),
#'   stringsAsFactors = FALSE
#' ))
#'
#' @export
delphi_suggestions <- function(data) {
  data <- .as_df(data)
  .need_cols(data, c("expert_id", "raw_name"), "\u65b0\u589e\u5efa\u8bae\u8868")
  data$expert_id <- as.character(data$expert_id)
  data$raw_name <- trimws(as.character(data$raw_name))
  data <- data[!is.na(data$raw_name) & nzchar(data$raw_name), , drop = FALSE]
  if (!nrow(data)) {
    data <- data.frame(
      suggestion_id = character(),
      expert_id = character(),
      round = integer(),
      slot = integer(),
      raw_name = character(),
      meaning = character(),
      rationale = character(),
      distinction = character(),
      evidence_note = character(),
      concept_id = character(),
      stringsAsFactors = FALSE
    )
    return(.add_class(data, "delphi_suggestions"))
  }
  if (!"suggestion_id" %in% names(data) || all(!nzchar(as.character(data$suggestion_id)))) {
    data$suggestion_id <- sprintf("S%03d", seq_len(nrow(data)))
  }
  data$suggestion_id <- as.character(data$suggestion_id)
  if (!"round" %in% names(data) || all(is.na(data$round))) data$round <- 1L
  data$round <- as.integer(data$round)
  if (!"slot" %in% names(data)) data$slot <- NA_integer_
  data$slot <- as.integer(data$slot)
  for (nm in c("meaning", "rationale", "distinction", "evidence_note", "concept_id")) {
    if (!nm %in% names(data)) data[[nm]] <- NA_character_
    data[[nm]] <- as.character(data[[nm]])
  }
  .add_class(data, "delphi_suggestions")
}

#' 按五问和双通道审查新增概念
#'
#' 研究组事先把自由文本归并成概念，并完成八类编码。本函数按锁定方案回答：
#' 是否在范围内、是否已被覆盖、是否为独立构念、是否有足够支持、能否形成
#' 单一可评价条目。至少 2 名专家独立提出走通道 A；仅 1 名但有外部证据走通道 B。
#' 范围外即使人数达标也不进入下一轮。同一人重复书写只计 1 人。
#'
#' 两名编码者不一致且没有第三人裁决时，决定为 `pending`，不会自动纳入。
#'
#' @param suggestions [delphi_suggestions()] 或数据框。可为空；为空时使用
#'   审查表里的 `n_independent`。
#' @param review 一行一个概念。必填 `concept_id`、`standardized_name`、`construct`。
#'   `construct` 取：`new_item`、`duplicate`、`hierarchy`、`modification`、
#'   `instrument`、`timepoint`、`out_of_scope`、`unevaluable`
#'   （或对应中文：新条目、重复、上下位、修改、测量工具、测量时间点、范围外、无法评价）。
#'   修改类还要 `modification_type`：`wording` 或 `substantive`。
#'   可选：`in_scope`、`covered_by`、`external_support`、`external_note`、
#'   `coder_a`、`coder_b`、`adjudicator`、`adjudicated_construct`、
#'   `proposed_item_id`、`proposed_text`、`domain`、`parent_item_id`、`n_independent`。
#' @param protocol [delphi_protocol()]。
#' @param items 当前候选条目，可省略。用于避免新编号与已有编号冲突。
#' @param enter_round 通过审查的条目从哪一轮开始评分，默认 2。
#'
#' @return 列表：`concepts`（含决定与理由）、`admitted`（可交给
#'   [update_delphi_study()] 的新条目）、`wording`、`disposition_complete`。
#'
#' @examples
#' protocol <- preset_protocol("comet_cos")
#' suggestions <- delphi_suggestions(data.frame(
#'   expert_id = c("E1", "E1", "E3", "E2", "E4"),
#'   raw_name = c("睡眠", "还是睡眠", "睡眠质量", "住院费用", "费用"),
#'   concept_id = c("C1", "C1", "C1", "C2", "C2"),
#'   stringsAsFactors = FALSE
#' ))
#' review <- data.frame(
#'   concept_id = c("C1", "C2", "C3"),
#'   standardized_name = c("睡眠质量", "医疗费用", "患者总体改善感受"),
#'   construct = c("new_item", "out_of_scope", "new_item"),
#'   in_scope = c(TRUE, FALSE, TRUE),
#'   external_support = c(FALSE, FALSE, TRUE),
#'   n_independent = c(NA, NA, 1),
#'   stringsAsFactors = FALSE
#' )
#' screen <- screen_new_items(suggestions, review, protocol)
#' screen$concepts[, c("concept_id", "n_independent", "channel", "decision")]
#'
#' @export
screen_new_items <- function(suggestions = NULL, review, protocol,
                             items = NULL, enter_round = 2L) {
  .stop_if(!inherits(protocol, "delphi_protocol"), "protocol \u5fc5\u987b\u662f delphi_protocol\u3002")
  ni <- protocol$new_items
  .stop_if(is.null(ni), "\u65b9\u6848\u6ca1\u6709 new_items\u3002\u8bf7\u7528\u5f53\u524d\u7248\u672c\u91cd\u65b0\u9501\u5b9a\u65b9\u6848\u3002")
  review <- .prepare_concept_review(review)
  if (!is.null(suggestions) && !inherits(suggestions, "delphi_suggestions")) {
    suggestions <- delphi_suggestions(suggestions)
  }
  enter_round <- as.integer(enter_round)[1]
  .stop_if(is.na(enter_round) || enter_round < 2L, "enter_round \u5e94\u4e3a\u4e0b\u4e00\u8f6e\uff0c\u901a\u5e38\u662f 2\u3002")
  existing <- if (is.null(items)) character() else as.character(items$item_id)

  out <- lapply(seq_len(nrow(review)), function(i) {
    .screen_one_concept(review[i, , drop = FALSE], suggestions, ni, enter_round, existing)
  })
  concepts <- do.call(rbind, lapply(out, `[[`, "row"))
  rownames(concepts) <- NULL
  admitted_rows <- lapply(out, `[[`, "item")
  admitted_rows <- admitted_rows[!vapply(admitted_rows, is.null, logical(1))]
  if (length(admitted_rows)) {
    admitted <- do.call(rbind, admitted_rows)
    rownames(admitted) <- NULL
    .stop_if(anyDuplicated(admitted$item_id) > 0L, "\u901a\u8fc7\u5ba1\u67e5\u7684\u65b0\u7f16\u53f7\u6709\u91cd\u590d\u3002")
    overlap <- intersect(admitted$item_id, existing)
    if (length(overlap)) {
      concepts$already_in_pool <- concepts$proposed_item_id %in% overlap
    }
    admitted <- delphi_items(admitted)
  } else {
    admitted <- delphi_items(data.frame(
      item_id = character(), item_text = character(), stringsAsFactors = FALSE
    ))
  }
  pending <- concepts$decision == "pending"
  list(
    concepts = concepts,
    admitted = admitted,
    wording = concepts[concepts$decision == "wording", , drop = FALSE],
    disposition_complete = !any(pending) && all(nzchar(concepts$decision)),
    n_admitted = sum(concepts$decision == "admit"),
    n_concepts = nrow(concepts),
    note = paste0(
      "\u5ba1\u67e5\u53ea\u6267\u884c\u9501\u5b9a\u89c4\u5219\uff0c\u4e0d\u628a\u53e5\u5b50\u81ea\u52a8\u6539\u6210\u6761\u76ee\u3002",
      "\u901a\u9053 A\uff1a\u81f3\u5c11 ", ni$min_independent, " \u540d\u4e13\u5bb6\u72ec\u7acb\u63d0\u51fa\uff1b",
      "\u901a\u9053 B\uff1a1 \u540d\u4e13\u5bb6\u63d0\u51fa\u4e14\u6709\u5916\u90e8\u8bc1\u636e\u3002",
      "\u672a\u8fdb\u5165\u4e0b\u4e00\u8f6e\u7684\u5efa\u8bae\u4ecd\u987b\u4fdd\u7559\u7406\u7531\u3002"
    )
  )
}

#' 区分文字修改和实质性修改
#'
#' 只改术语、不改变评价构念时，保留原编号，不重新计算评分轮次。
#' 改变了评价构念时，生成新版本，从 `enter_round` 起作为新条目评分；
#' 原条目标记为被替代，不再进入后续问卷。
#'
#' @param items [delphi_items()] 或数据框。
#' @param changes 数据框。必填 `item_id`、`modification`（`wording` 或 `substantive`）。
#'   实质性修改还要 `new_text`。可选 `new_item_id`、`note`。
#' @param enter_round 实质性新版本进入评分的轮次。
#'
#' @return 列表：`items` 为更新后的条目表，`log` 记录是否重置评分轮次。
#'
#' @examples
#' items <- delphi_items(data.frame(
#'   item_id = c("A", "B"),
#'   item_text = c("\u8fd0\u52a8\u8010\u91cf", "\u4e2d\u533b\u75c7\u72b6\u6539\u5584"),
#'   introduced_round = c(1L, 1L),
#'   stringsAsFactors = FALSE
#' ))
#' changed <- classify_modification(
#'   items,
#'   data.frame(
#'     item_id = c("A", "B"),
#'     modification = c("wording", "substantive"),
#'     new_text = c("\u8fd0\u52a8\u80fd\u529b", "\u6c14\u865a\u8840\u7600\u8bc1\u8bc1\u5019\u79ef\u5206\u6539\u5584"),
#'     new_item_id = c(NA, "B2"),
#'     stringsAsFactors = FALSE
#'   ),
#'   enter_round = 3
#' )
#' changed$log
#'
#' @export
classify_modification <- function(items, changes, enter_round = 2L) {
  if (!inherits(items, "delphi_items")) items <- delphi_items(items)
  changes <- .as_df(changes)
  .need_cols(changes, c("item_id", "modification"), "\u4fee\u6539\u8bb0\u5f55")
  enter_round <- as.integer(enter_round)[1]
  if (!"new_text" %in% names(changes)) changes$new_text <- NA_character_
  if (!"new_item_id" %in% names(changes)) changes$new_item_id <- NA_character_
  if (!"note" %in% names(changes)) changes$note <- NA_character_
  if (!"introduced_round" %in% names(items)) items$introduced_round <- 1L
  if (!"origin" %in% names(items)) items$origin <- "initial"
  if (!"modification" %in% names(items)) items$modification <- "none"
  if (!"parent_item_id" %in% names(items)) items$parent_item_id <- NA_character_
  if (!"previous_text" %in% names(items)) items$previous_text <- NA_character_
  if (!"superseded_by" %in% names(items)) items$superseded_by <- NA_character_
  if (!"wording_note" %in% names(items)) items$wording_note <- NA_character_

  logs <- vector("list", nrow(changes))
  for (i in seq_len(nrow(changes))) {
    id <- as.character(changes$item_id[i])
    hit <- which(items$item_id == id)
    .stop_if(!length(hit), sprintf("\u627e\u4e0d\u5230\u6761\u76ee %s\u3002", id))
    kind <- .canon_modification(changes$modification[i])
    note <- as.character(changes$note[i])
    new_text <- as.character(changes$new_text[i])
    if (identical(kind, "wording")) {
      if (!is.na(new_text) && nzchar(new_text) && !identical(new_text, items$item_text[hit[1]])) {
        items$previous_text[hit[1]] <- items$item_text[hit[1]]
        items$item_text[hit[1]] <- new_text
      }
      items$modification[hit[1]] <- "wording"
      items$wording_note[hit[1]] <- note
      logs[[i]] <- data.frame(
        item_id = id,
        modification = "wording",
        new_item_id = id,
        reset_scoring = FALSE,
        note = "\u53ea\u89c4\u8303\u7528\u8bed\uff0c\u4e0d\u6539\u53d8\u6784\u5ff5\uff0c\u4e0d\u91cd\u65b0\u8ba1\u7b97\u8bc4\u5206\u8f6e\u6b21\u3002",
        stringsAsFactors = FALSE
      )
    } else {
      new_id <- as.character(changes$new_item_id[i])
      if (is.na(new_id) || !nzchar(new_id)) new_id <- paste0(id, "-R", enter_round)
      .stop_if(new_id %in% items$item_id, sprintf("\u65b0\u7f16\u53f7 %s \u5df2\u5b58\u5728\u3002", new_id))
      .stop_if(is.na(new_text) || !nzchar(new_text), "\u5b9e\u8d28\u6027\u4fee\u6539\u5fc5\u987b\u63d0\u4f9b new_text\u3002")
      row <- items[hit[1], , drop = FALSE]
      row$item_id <- new_id
      row$previous_text <- items$item_text[hit[1]]
      row$item_text <- new_text
      row$introduced_round <- enter_round
      row$origin <- "substantive"
      row$modification <- "substantive"
      row$parent_item_id <- id
      row$superseded_by <- NA_character_
      row$wording_note <- note
      items <- .rbind_fill(items, row)
      items$superseded_by[items$item_id == id] <- new_id
      logs[[i]] <- data.frame(
        item_id = id,
        modification = "substantive",
        new_item_id = new_id,
        reset_scoring = TRUE,
        note = "\u6539\u53d8\u4e86\u8bc4\u4ef7\u6784\u5ff5\uff0c\u6309\u65b0\u6761\u76ee\u91cd\u65b0\u8ba1\u7b97\u8bc4\u5206\u8f6e\u6b21\u3002",
        stringsAsFactors = FALSE
      )
    }
  }
  items <- delphi_items(items)
  list(items = items, log = do.call(rbind, logs))
}

#' 只评过一次的新条目标为暂时共识
#'
#' 原条目若已有两轮评分，分类保持不变。新进入的条目、或实质性修改后的新版本，
#' 在只完成一轮评分时，即使已经达到纳入线，`action` 也改为 `rescore`，
#' 中文分类前加「暂时」。共识类别代码本身不改，以免打乱稳定性比较。
#'
#' @param classification [classify_consensus()] 的结果。
#' @param items 条目表，可含 `introduced_round`、`origin`、`modification`。
#' @param ratings 评分长表。
#' @param protocol 含 `new_items$require_iteration` 的方案。未要求迭代时原样返回。
#'
#' @return 增加 `provisional` 与 `n_scoring_rounds` 的分类表。
#'
#' @examples
#' protocol <- preset_protocol("comet_cos")
#' items <- delphi_items(data.frame(
#'   item_id = "NEW-01",
#'   item_text = "\u7761\u7720\u8d28\u91cf",
#'   introduced_round = 2L,
#'   origin = "new",
#'   stringsAsFactors = FALSE
#' ))
#' ratings <- delphi_ratings(data.frame(
#'   expert_id = sprintf("E%02d", 1:10),
#'   item_id = "NEW-01",
#'   round = 2L,
#'   score = c(rep(8, 8), 5, 2),
#'   dimension = "\u91cd\u8981\u6027",
#'   stringsAsFactors = FALSE
#' ))
#' study <- delphi_study(
#'   protocol, data.frame(expert_id = sprintf("E%02d", 1:10)),
#'   items, ratings, validate = FALSE
#' )
#' sm <- summarize_items(study)
#' cls <- classify_consensus(sm, protocol)
#' marked <- mark_iteration(cls, items, ratings, protocol)
#' marked$provisional
#' marked$action
#'
#' @export
mark_iteration <- function(classification, items, ratings, protocol = NULL) {
  if (is.null(protocol) || !isTRUE(protocol$new_items$require_iteration)) {
    return(classification)
  }
  if (is.null(classification) || !nrow(classification)) return(classification)
  ratings <- .ensure_unable(.ensure_dimension(.as_df(ratings)))
  items <- .as_df(items)
  if (!"provisional" %in% names(classification)) {
    classification$provisional <- FALSE
  }
  if (!"n_scoring_rounds" %in% names(classification)) {
    classification$n_scoring_rounds <- NA_integer_
  }
  keep_classes <- c("consensus_in", "high_consensus", "basic_consensus", "keep", "retain")
  meta <- .item_clock(items)
  global_first <- if (nrow(ratings)) min(ratings$round, na.rm = TRUE) else 1L
  for (i in seq_len(nrow(classification))) {
    id <- as.character(classification$item_id[i])
    rnd <- if ("round" %in% names(classification)) as.integer(classification$round[i]) else global_first
    dim <- if ("dimension" %in% names(classification)) as.character(classification$dimension[i]) else NULL
    sub <- ratings[ratings$item_id == id & ratings$round <= rnd, , drop = FALSE]
    if (!is.null(dim) && nzchar(dim) && "dimension" %in% names(sub)) {
      sub <- sub[sub$dimension == dim, , drop = FALSE]
    }
    clock <- meta$introduced[[id]]
    if (is.null(clock) || length(clock) != 1L || is.na(clock)) clock <- 1L
    org <- meta$origin[[id]]
    if (is.null(org) || length(org) != 1L || is.na(org) || !nzchar(org)) org <- "initial"
    md <- meta$modification[[id]]
    if (is.null(md) || length(md) != 1L || is.na(md) || !nzchar(md)) md <- "none"
    sub <- sub[sub$round >= clock, , drop = FALSE]
    scored <- unique(sub$round[!sub$unable & !is.na(sub$score)])
    n_scored <- length(scored)
    classification$n_scoring_rounds[i] <- n_scored
    seen <- ratings$round[ratings$item_id == id]
    first_obs <- if (length(seen)) min(seen, na.rm = TRUE) else rnd
    is_newish <- org %in% c("new", "substantive") ||
      clock > global_first ||
      identical(md, "substantive") ||
      first_obs > global_first
    cls <- classification$class[i]
    if (is_newish && !is.na(cls) && cls %in% keep_classes && n_scored < 2L) {
      classification$provisional[i] <- TRUE
      classification$action[i] <- "rescore"
      zh <- as.character(classification$class_zh[i])
      if (!is.na(zh) && nzchar(zh) && !startsWith(zh, "\u6682\u65f6")) {
        classification$class_zh[i] <- paste0("\u6682\u65f6", zh)
      }
    } else {
      classification$provisional[i] <- FALSE
    }
  }
  classification
}

#' 条目变更日志
#'
#' 汇总初始条目、纳入的新条目，以及未进入下一轮的建议。
#' 用于回答：谁提出、与哪条重复、为什么删除或新增、哪一轮加入、最终结果。
#'
#' @param study [delphi_study()]。
#' @param review [screen_new_items()] 的 `concepts`。缺省时不写建议去向。
#' @param classification 可选，用于填写各轮结果。
#'
#' @return 数据框。每行一条目或一条未纳入的建议。
#'
#' @examples
#' protocol <- preset_protocol("comet_cos")
#' suggestions <- delphi_suggestions(data.frame(
#'   expert_id = c("E1", "E2"),
#'   raw_name = c("\u8d39\u7528", "\u8d39\u7528"),
#'   concept_id = "C2",
#'   stringsAsFactors = FALSE
#' ))
#' review <- data.frame(
#'   concept_id = "C2",
#'   standardized_name = "\u533b\u7597\u8d39\u7528",
#'   construct = "out_of_scope",
#'   in_scope = FALSE,
#'   stringsAsFactors = FALSE
#' )
#' items <- delphi_items(data.frame(
#'   item_id = "CHF-01", item_text = "\u5168\u56e0\u6b7b\u4ea1", stringsAsFactors = FALSE
#' ))
#' study <- delphi_study(
#'   protocol, data.frame(expert_id = "E1"), items,
#'   validate = FALSE, suggestions = suggestions, concept_review = review
#' )
#' screen <- screen_new_items(suggestions, review, protocol, items)
#' item_changelog(study, screen$concepts)
#'
#' @export
item_changelog <- function(study, review = NULL, classification = NULL) {
  study <- .match_study(study)
  items <- .as_df(study$items)
  if (is.null(review) && !is.null(study$concept_review) &&
      "decision" %in% names(study$concept_review)) {
    review <- study$concept_review
  }
  blank <- "\u2014"
  rows <- lapply(seq_len(nrow(items)), function(i) {
    id <- as.character(items$item_id[i])
    intro <- if ("introduced_round" %in% names(items)) items$introduced_round[i] else 1L
    if (is.na(intro)) intro <- 1L
    origin <- if ("origin" %in% names(items)) as.character(items$origin[i]) else "initial"
    src <- if ("source_type" %in% names(items) && !is.na(items$source_type[i]) &&
               nzchar(items$source_type[i])) {
      as.character(items$source_type[i])
    } else if (!is.na(origin) && origin %in% c("new", "substantive")) {
      "\u7b2c 1 \u8f6e\u4e13\u5bb6\u65b0\u589e"
    } else {
      "\u521d\u59cb\u5019\u9009\u6c60"
    }
    text <- as.character(items$item_text[i])
    data.frame(
      item_id = id,
      source = src,
      text_r1 = if (intro <= 1L) text else blank,
      suggestion = blank,
      decision_r1 = if (!is.na(origin) && origin %in% c("new", "substantive")) "\u65b0\u589e" else "\u4fdd\u7559",
      text_scored = text,
      result_r2 = .round_result_cell(classification, id, 2L),
      result_r3 = .round_result_cell(classification, id, 3L),
      final = .final_result_cell(classification, id),
      reason = if ("source_note" %in% names(items) && !is.na(items$source_note[i])) {
        as.character(items$source_note[i])
      } else {
        blank
      },
      stringsAsFactors = FALSE
    )
  })
  log <- if (length(rows)) do.call(rbind, rows) else .empty_changelog()
  if (!is.null(review) && nrow(review)) {
    review <- .as_df(review)
    for (i in seq_len(nrow(review))) {
      dec <- if ("decision" %in% names(review)) as.character(review$decision[i]) else ""
      dest <- NA_character_
      if ("proposed_item_id" %in% names(review)) dest <- as.character(review$proposed_item_id[i])
      mapped <- NA_character_
      if ("mapped_item_id" %in% names(review)) mapped <- as.character(review$mapped_item_id[i])
      if ("covered_by" %in% names(review) && (is.na(mapped) || !nzchar(mapped))) {
        mapped <- as.character(review$covered_by[i])
      }
      label <- if ("standardized_name" %in% names(review)) as.character(review$standardized_name[i]) else review$concept_id[i]
      n_ind <- if ("n_independent" %in% names(review)) review$n_independent[i] else NA
      channel <- if ("channel" %in% names(review)) as.character(review$channel[i]) else ""
      reason <- if ("reason" %in% names(review)) as.character(review$reason[i]) else ""
      decision_zh <- if ("decision_zh" %in% names(review)) as.character(review$decision_zh[i]) else dec
      suggestion <- sprintf("%s\uff08\u72ec\u7acb\u63d0\u51fa %s \u4eba\uff09", label, ifelse(is.na(n_ind), "?", n_ind))
      if (identical(dec, "admit") && !is.na(dest) && nzchar(dest) && dest %in% log$item_id) {
        j <- match(dest, log$item_id)
        log$suggestion[j] <- suggestion
        log$decision_r1[j] <- sprintf("\u65b0\u589e\uff08\u901a\u9053%s\uff09", channel)
        log$reason[j] <- reason
        log$source[j] <- sprintf("%s\u540d\u4e13\u5bb6\uff1b\u901a\u9053%s", ifelse(is.na(n_ind), "?", n_ind), channel)
      } else if (!is.na(mapped) && nzchar(mapped) && mapped %in% log$item_id && !identical(dec, "admit")) {
        j <- match(mapped, log$item_id)
        extra <- sprintf("%s\uff1a%s", label, decision_zh)
        if (identical(log$suggestion[j], blank)) log$suggestion[j] <- extra else log$suggestion[j] <- paste(log$suggestion[j], extra, sep = "\uff1b")
        log <- rbind(log, .suggestion_log_row(review[i, , drop = FALSE], suggestion, decision_zh, reason))
      } else {
        log <- rbind(log, .suggestion_log_row(review[i, , drop = FALSE], suggestion, decision_zh, reason))
      }
    }
  }
  rownames(log) <- NULL
  names(log) <- c(
    "\u6761\u76ee", "\u6765\u6e90", "\u7b2c1\u8f6e\u7248\u672c", "\u4e13\u5bb6\u5efa\u8bae", "\u7b2c1\u8f6e\u5904\u7406",
    "\u8fdb\u5165\u8bc4\u5206\u7684\u7248\u672c", "\u7b2c2\u8f6e\u7ed3\u679c", "\u7b2c3\u8f6e\u7ed3\u679c", "\u6700\u7ec8", "\u7406\u7531"
  )
  log
}

#' 共识会议分组
#'
#' A 类为已完成迭代且明确纳入，原则上直接进入。
#' B 类为明确排除，原则上不再翻案。
#' C 类为未达预设阈值，才是会议重点。
#' 只评过一次的暂时共识继续书面迭代，不在会上直接纳入。
#' `floor_items` 中从未匿名评分的临时提案标记为不得当场拍板。
#'
#' @param classification 分类表。默认取最后一轮；若各条目最后出现的轮次不同，
#'   每个条目用它自己的最新一轮。
#' @param round 若指定，只用该轮。
#' @param dimension 维度。缺省时优先「重要性」。
#' @param floor_items 会上临时提出、尚未评分的条目。至少含 `item_text`。
#'
#' @return 数据框，含 `band`：`A`、`B`、`C`、`iterate`、`floor`。
#'
#' @examples
#' protocol <- preset_protocol("comet_cos")
#' items <- data.frame(
#'   item_id = c("A", "B"),
#'   item_text = c("\u6761\u76ee\u7532", "\u6761\u76ee\u4e59"),
#'   stringsAsFactors = FALSE
#' )
#' ratings <- delphi_ratings(data.frame(
#'   expert_id = rep(sprintf("E%02d", 1:10), 2),
#'   item_id = rep(c("A", "B"), each = 10),
#'   round = 2L,
#'   score = c(rep(8, 10), rep(2, 8), 8, 5),
#'   dimension = "\u91cd\u8981\u6027",
#'   stringsAsFactors = FALSE
#' ))
#' study <- delphi_study(
#'   protocol, data.frame(expert_id = sprintf("E%02d", 1:10)),
#'   items, ratings, validate = FALSE
#' )
#' sm <- summarize_items(study)
#' cls <- classify_consensus(sm, protocol)
#' consensus_meeting_agenda(cls, floor_items = data.frame(
#'   item_text = "\u7167\u62a4\u8005\u8d1f\u62c5", stringsAsFactors = FALSE
#' ))
#'
#' @export
consensus_meeting_agenda <- function(classification, round = NULL, dimension = NULL,
                                     floor_items = NULL) {
  .stop_if(is.null(classification) || !nrow(classification), "\u6ca1\u6709\u5206\u7c7b\u7ed3\u679c\u3002")
  cls <- classification
  if (!is.null(round)) {
    cls <- cls[cls$round == as.integer(round), , drop = FALSE]
  } else if ("round" %in% names(cls)) {
    cls <- .latest_class_rows(cls)
  }
  if (!is.null(dimension) && "dimension" %in% names(cls)) {
    cls <- cls[cls$dimension == dimension, , drop = FALSE]
  } else if ("dimension" %in% names(cls)) {
    prefer <- intersect(
      c("\u91cd\u8981\u6027", "\u91cd\u8981\u7a0b\u5ea6", "overall", "\u9002\u5b9c\u6027"),
      unique(as.character(cls$dimension))
    )
    if (length(prefer)) cls <- cls[cls$dimension == prefer[1], , drop = FALSE]
  }
  .stop_if(!nrow(cls), "\u6307\u5b9a\u8f6e\u6b21\u6216\u7ef4\u5ea6\u4e0b\u6ca1\u6709\u6761\u76ee\u3002")
  prov <- if ("provisional" %in% names(cls)) isTRUE(cls$provisional) | cls$provisional %in% TRUE else rep(FALSE, nrow(cls))
  prov[is.na(prov)] <- FALSE
  in_cls <- cls$class %in% c("consensus_in", "high_consensus", "basic_consensus", "keep", "retain")
  out_cls <- cls$class %in% c("consensus_out", "reject", "not_retain") |
    (if ("action" %in% names(cls)) cls$action %in% "drop" else FALSE)
  band <- rep("C", nrow(cls))
  band[in_cls & !prov] <- "A"
  band[out_cls & !prov] <- "B"
  band[prov] <- "iterate"
  note <- ifelse(
    band == "A",
    "\u5df2\u5b8c\u6210\u8fed\u4ee3\u4e14\u8fbe\u5230\u7eb3\u5165\u6807\u51c6\u3002\u539f\u5219\u4e0a\u76f4\u63a5\u8fdb\u5165\uff0c\u4f1a\u8bae\u4e0d\u91cd\u65b0\u5426\u51b3\u3002",
    ifelse(
      band == "B",
      "\u5df2\u660e\u786e\u6392\u9664\u3002\u539f\u5219\u4e0a\u4e0d\u518d\u7ffb\u6848\u3002",
      ifelse(
        band == "iterate",
        "\u53ea\u5b8c\u6210\u4e00\u6b21\u8bc4\u5206\u3002\u5373\u4f7f\u5206\u6570\u8fc7\u7ebf\uff0c\u4e5f\u53ea\u662f\u6682\u65f6\u5171\u8bc6\uff0c\u987b\u7ee7\u7eed\u4e66\u9762\u8fed\u4ee3\uff0c\u4e0d\u80fd\u5728\u4f1a\u4e0a\u76f4\u63a5\u7eb3\u5165\u3002",
        "\u672a\u8fbe\u5230\u9884\u8bbe\u5171\u8bc6\u9608\u503c\u3002\u8fd9\u662f\u4f1a\u8bae\u8981\u8ba8\u8bba\u7684\u90e8\u5206\u3002\u4e0d\u8981\u5199\u6210\u4e13\u5bb6\u8ba4\u4e3a\u4e0d\u91cd\u8981\u3002"
      )
    )
  )
  agenda <- data.frame(
    item_id = as.character(cls$item_id),
    item_text = if ("item_text" %in% names(cls)) as.character(cls$item_text) else NA_character_,
    class = as.character(cls$class),
    class_zh = as.character(cls$class_zh),
    band = band,
    note = note,
    stringsAsFactors = FALSE
  )
  if (!is.null(floor_items) && nrow(.as_df(floor_items))) {
    floor_items <- .as_df(floor_items)
    .need_cols(floor_items, "item_text", "\u4f1a\u4e0a\u4e34\u65f6\u63d0\u6848")
    fid <- if ("item_id" %in% names(floor_items)) as.character(floor_items$item_id) else paste0("FLOOR-", seq_len(nrow(floor_items)))
    extra <- data.frame(
      item_id = fid,
      item_text = as.character(floor_items$item_text),
      class = "not_scored",
      class_zh = "\u672a\u7ecf\u533f\u540d\u8bc4\u5206",
      band = "floor",
      note = paste0(
        "\u4f1a\u4e0a\u4e34\u65f6\u63d0\u51fa\u3002\u4e13\u5bb6\u6ca1\u6709\u63d0\u524d\u770b\u5230\uff0c\u6ca1\u6709\u533f\u540d\u8bc4\u5206\uff0c\u4e5f\u6ca1\u6709\u53d7\u63a7\u53cd\u9988\u3002",
        "\u4e0d\u5f97\u5f53\u573a\u62cd\u677f\u3002\u82e5\u786e\u5b9e\u91cd\u8981\uff0c\u5e94\u53e6\u5f00\u4e00\u8f6e\u4e66\u9762\u533f\u540d\u8bc4\u4ef7\u3002"
      ),
      stringsAsFactors = FALSE
    )
    agenda <- rbind(agenda, extra)
  }
  rownames(agenda) <- NULL
  agenda
}

#' 核对新增条目过程是否符合预先规定
#'
#' 对照六条原则：允许新增、不是提到就加入、规则事先锁定、双通道、
#' 新条目必须迭代、建议都有去向。并标出几种不建议的做法，
#' 例如只按人数、第二轮高分后直接纳入、用均数代替共识。
#'
#' @param protocol [delphi_protocol()]。
#' @param review 可选，[screen_new_items()] 的 `concepts`。
#' @param classification 可选分类表。
#' @param suggestions 可选建议表，用于核对人数是否来自独立专家。
#'
#' @return 数据框：`check`、`status`（通过 / 警告 / 不通过 / 未执行 / 不适用）、`detail`。
#'
#' @examples
#' audit_new_items(preset_protocol("comet_cos"))
#' audit_new_items(preset_protocol("rand_ucla"))
#'
#' @export
audit_new_items <- function(protocol, review = NULL, classification = NULL,
                            suggestions = NULL) {
  .stop_if(!inherits(protocol, "delphi_protocol"), "protocol \u5fc5\u987b\u662f delphi_protocol\u3002")
  ni <- protocol$new_items
  rows <- list()
  add <- function(check, status, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(
      check = check, status = status, detail = detail, stringsAsFactors = FALSE
    )
  }
  if (is.null(ni) || !isTRUE(ni$allow)) {
    add(
      "\u5141\u8bb8\u65b0\u589e", "\u4e0d\u9002\u7528",
      "\u672c\u65b9\u6848\u672a\u5f00\u542f\u7b2c\u4e00\u8f6e\u8865\u5145\u6761\u76ee\u3002\u82e5\u786e\u5b9e\u4e0d\u8bbe\u5f00\u653e\u9898\uff0c\u5e94\u5728\u65b9\u6848\u4e2d\u5199\u660e\u7406\u7531\u3002\u521d\u59cb\u6761\u76ee\u6c60\u4e0d\u80fd\u9ed8\u8ba4\u5df2\u7ecf\u5b8c\u6574\u3002"
    )
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  add("\u5141\u8bb8\u65b0\u589e", "\u901a\u8fc7", "\u65b9\u6848\u5141\u8bb8\u7b2c 1 \u8f6e\u7ed3\u6784\u5316\u8865\u5145\u9057\u6f0f\u6761\u76ee\u3002\u521d\u59cb\u6761\u76ee\u6c60\u4e0d\u5047\u5b9a\u5df2\u7ecf\u5b8c\u6574\u3002")
  add(
    "\u89c4\u5219\u4e8b\u5148\u9501\u5b9a",
    if (isTRUE(protocol$locked)) "\u901a\u8fc7" else "\u8b66\u544a",
    if (isTRUE(protocol$locked)) {
      "\u65b0\u589e\u89c4\u5219\u4e0e\u5171\u8bc6\u9608\u503c\u90fd\u5728\u89c1\u5230\u7ed3\u679c\u4e4b\u524d\u9501\u5b9a\u3002\u5206\u6790\u51fd\u6570\u4e0d\u4f1a\u56de\u6539\u3002"
    } else {
      "\u65b9\u6848\u672a\u9501\u5b9a\u3002\u5e94\u5728\u53d1\u653e\u7b2c 1 \u8f6e\u524d\u9501\u5b9a\u3002"
    }
  )
  if (isTRUE(ni$allow_single_with_evidence) && ni$min_independent <= 2L) {
    add(
      "\u53cc\u901a\u9053", "\u901a\u8fc7",
      sprintf("\u4e3b\u901a\u9053\u4e3a\u81f3\u5c11 %s \u540d\u4e13\u5bb6\u72ec\u7acb\u63d0\u51fa\uff1b\u53e6\u4fdd\u7559 1 \u4eba\u63d0\u51fa\u4e14\u6709\u5916\u90e8\u8bc1\u636e\u7684\u5ba1\u67e5\u901a\u9053\u3002\u63d0\u51fa\u6b21\u6570\u4e0d\u7b49\u4e8e\u4e34\u5e8a\u91cd\u8981\u6027\u3002", ni$min_independent)
    )
  } else if (!isTRUE(ni$allow_single_with_evidence)) {
    add("\u53cc\u901a\u9053", "\u8b66\u544a", "\u53ea\u6709\u4eba\u6570\u95e8\u69db\uff0c\u6ca1\u6709\u5355\u4eba\u63d0\u51fa\u52a0\u5916\u90e8\u8bc1\u636e\u7684\u4f8b\u5916\u3002\u53ef\u80fd\u6f0f\u6389\u60a3\u8005\u91cd\u8981\u6216\u5b89\u5168\u6027\u7ed3\u5c40\u3002")
  } else {
    add("\u53cc\u901a\u9053", "\u8b66\u544a", sprintf("\u72ec\u7acb\u4eba\u6570\u95e8\u69db\u4e3a %s\uff0c\u9ad8\u4e8e\u5e38\u7528\u7684 2 \u4eba\u793a\u4f8b\u3002\u65b0\u589e\u9636\u6bb5\u662f\u4e3a\u4e86\u9632\u6b62\u9057\u6f0f\uff0c\u4e0d\u662f\u63d0\u524d\u505a\u5171\u8bc6\u3002", ni$min_independent))
  }
  if (ni$min_independent >= 3L) {
    add("\u907f\u514d\u8fc7\u4e25\u4eba\u6570\u95e8\u69db", "\u8b66\u544a", "\u4e0d\u5efa\u8bae\u8981\u6c42\u4e09\u5206\u4e4b\u4e8c\u4e13\u5bb6\u90fd\u63d0\u5230\u624d\u5141\u8bb8\u65b0\u589e\u3002")
  } else {
    add("\u907f\u514d\u8fc7\u4e25\u4eba\u6570\u95e8\u69db", "\u901a\u8fc7", "\u6ca1\u6709\u628a\u65b0\u589e\u95e8\u69db\u8bbe\u6210\u4e09\u5206\u4e4b\u4e8c\u591a\u6570\u3002")
  }
  add(
    "\u5fc5\u987b\u8fed\u4ee3",
    if (isTRUE(ni$require_iteration)) "\u901a\u8fc7" else "\u8b66\u544a",
    if (isTRUE(ni$require_iteration)) {
      "\u65b0\u6761\u76ee\u81f3\u5c11\u4e24\u8f6e\u8bc4\u5206\u540e\u624d\u80fd\u6700\u7ec8\u7eb3\u5165\u3002\u9996\u6b21\u9ad8\u5206\u53ea\u8bb0\u4e3a\u6682\u65f6\u5171\u8bc6\u3002"
    } else {
      "\u672a\u8981\u6c42\u8fed\u4ee3\u3002\u7b2c\u4e8c\u8f6e\u624d\u9996\u6b21\u51fa\u73b0\u7684\u6761\u76ee\uff0c\u4e0d\u5e94\u56e0\u4e3a\u8fd9\u4e00\u8f6e\u5206\u6570\u9ad8\u5c31\u76f4\u63a5\u7eb3\u5165\u3002"
    }
  )
  if (is.null(review) || !nrow(.as_df(review))) {
    add("\u6bcf\u6761\u5efa\u8bae\u6709\u53bb\u5411", "\u672a\u6267\u884c", "\u8fd8\u6ca1\u6709\u7f16\u7801\u540e\u7684\u6982\u5ff5\u5ba1\u67e5\u8868\u3002\u8f6f\u4ef6\u4e0d\u4f1a\u628a\u53e5\u5b50\u81ea\u52a8\u6539\u6210\u6761\u76ee\u3002")
    add("\u4e0d\u662f\u63d0\u5230\u5c31\u52a0\u5165", "\u672a\u6267\u884c", "\u6ca1\u6709\u5ba1\u67e5\u8868\uff0c\u65e0\u6cd5\u6838\u5bf9\u662f\u5426\u628a\u5168\u90e8\u5efa\u8bae\u81ea\u52a8\u52a0\u5165\u3002")
    add("\u8303\u56f4\u4f18\u5148\u4e8e\u4eba\u6570", "\u672a\u6267\u884c", "\u6709\u4e86\u5ba1\u67e5\u8868\u4e4b\u540e\uff0c\u624d\u80fd\u6838\u5bf9\u8d85\u51fa\u8303\u56f4\u7684\u5efa\u8bae\u6709\u6ca1\u6709\u88ab\u4eba\u6570\u5e26\u8fdb\u6765\u3002")
  } else {
    review <- .as_df(review)
    dec <- as.character(review$decision)
    complete <- !is.na(dec) & nzchar(dec) & dec != "pending"
    add(
      "\u6bcf\u6761\u5efa\u8bae\u6709\u53bb\u5411",
      if (all(complete)) "\u901a\u8fc7" else "\u8b66\u544a",
      "\u6bcf\u4e2a\u5f52\u5e76\u540e\u7684\u6982\u5ff5\u90fd\u5e94\u6709\u7eb3\u5165\u3001\u6392\u9664\u3001\u5408\u5e76\u3001\u8f6c\u9636\u6bb5\u6216\u5f85\u88c1\u51b3\u7684\u7ed3\u8bba\uff0c\u5e76\u5199\u660e\u7406\u7531\u3002"
    )
    if ("coder_a" %in% names(review) && any(dec == "pending")) {
      add("\u53cc\u4eba\u7f16\u7801", "\u8b66\u544a", "\u5b58\u5728\u7f16\u7801\u4e0d\u4e00\u81f4\u4e14\u672a\u7ecf\u7b2c\u4e09\u4eba\u88c1\u51b3\u7684\u6982\u5ff5\u3002")
    } else {
      add("\u53cc\u4eba\u7f16\u7801", "\u901a\u8fc7", "\u6ca1\u6709\u672a\u88c1\u51b3\u7684\u7f16\u7801\u5206\u6b67\u3002\u7f16\u7801\u4ecd\u7531\u7814\u7a76\u7ec4\u5b8c\u6210\u3002")
    }
    n_admit <- sum(dec == "admit", na.rm = TRUE)
    if (nrow(review) > 1L && n_admit == nrow(review)) {
      add("\u4e0d\u662f\u63d0\u5230\u5c31\u52a0\u5165", "\u8b66\u544a", "\u5168\u90e8\u6982\u5ff5\u90fd\u88ab\u7eb3\u5165\u3002\u8bf7\u786e\u8ba4\u8303\u56f4\u3001\u91cd\u590d\u548c\u6d4b\u91cf\u5de5\u5177\u5ba1\u67e5\u786e\u5b9e\u6267\u884c\u8fc7\u3002")
    } else {
      add("\u4e0d\u662f\u63d0\u5230\u5c31\u52a0\u5165", "\u901a\u8fc7", sprintf("%s \u4e2a\u6982\u5ff5\u4e2d\u6709 %s \u4e2a\u8fdb\u5165\u4e0b\u4e00\u8f6e\uff0c\u5176\u4f59\u6709\u660e\u786e\u53bb\u5411\u3002", nrow(review), n_admit))
    }
    leaked <- rep(FALSE, nrow(review))
    if ("in_scope" %in% names(review)) {
      leaked <- leaked | (dec == "admit" & !.as_bool(review$in_scope, default = TRUE))
    }
    if ("construct" %in% names(review)) {
      leaked <- leaked | (dec == "admit" & review$construct %in% c("out_of_scope", "instrument", "timepoint", "unevaluable"))
    }
    add(
      "\u8303\u56f4\u4f18\u5148\u4e8e\u4eba\u6570",
      if (any(leaked)) "\u4e0d\u901a\u8fc7" else "\u901a\u8fc7",
      if (any(leaked)) "\u6709\u8d85\u51fa\u8303\u56f4\u3001\u6d4b\u91cf\u5de5\u5177\u6216\u65f6\u95f4\u70b9\u4ecd\u88ab\u7eb3\u5165\u3002" else "\u4eba\u6570\u8fbe\u6807\u4e5f\u4e0d\u80fd\u8d8a\u8fc7\u7814\u7a76\u8303\u56f4\uff0c\u4e5f\u4e0d\u80fd\u628a\u6d4b\u91cf\u5de5\u5177\u6216\u65f6\u95f4\u70b9\u5f53\u6210\u65b0\u7ed3\u5c40\u3002"
    )
  }
  if (!is.null(classification) && nrow(classification) && "provisional" %in% names(classification)) {
    bad <- classification$provisional %in% TRUE & classification$action %in% "keep"
    add(
      "\u9996\u6b21\u8bc4\u5206\u4e0d\u5f97\u6700\u7ec8\u7eb3\u5165",
      if (any(bad, na.rm = TRUE)) "\u4e0d\u901a\u8fc7" else "\u901a\u8fc7",
      if (any(bad, na.rm = TRUE)) "\u5b58\u5728\u53ea\u8bc4\u8fc7\u4e00\u6b21\u5374\u5efa\u8bae\u4fdd\u7559\u7684\u6761\u76ee\u3002" else "\u6682\u65f6\u5171\u8bc6\u7684\u52a8\u4f5c\u662f\u518d\u8bc4\uff0c\u4e0d\u662f\u7eb3\u5165\u3002"
    )
  }
  if (protocol$scenario %in% c("comet_cos", "clinical_consensus", "definition")) {
    add(
      "\u4e0d\u7528\u5747\u6570\u4ee3\u66ff\u5171\u8bc6", "\u901a\u8fc7",
      "\u6761\u76ee\u5224\u5b9a\u4f7f\u7528\u4e2d\u4f4d\u6570\u3001\u56db\u5206\u4f4d\u8ddd\u548c\u9ad8/\u4f4e\u533a\u6bd4\u4f8b\u3002\u5747\u6570\u3001\u53d8\u5f02\u7cfb\u6570\u3001Kendall \u534f\u8c03\u7cfb\u6570\u548c\u6743\u5a01\u7cfb\u6570\u53ea\u63cf\u8ff0\u6574\u4f53\u60c5\u51b5\uff0c\u4e0d\u51b3\u5b9a\u5355\u6761\u53bb\u7559\u3002"
    )
  }
  if (!is.null(suggestions) && nrow(.as_df(suggestions)) && "concept_id" %in% names(suggestions)) {
    add(
      "\u72ec\u7acb\u63d0\u51fa", "\u901a\u8fc7",
      "\u4eba\u6570\u6309\u4e13\u5bb6\u7f16\u53f7\u53bb\u91cd\u3002\u540c\u4e00\u4e2a\u4eba\u628a\u540c\u4e00\u6982\u5ff5\u5199\u4e09\u6b21\uff0c\u4ecd\u7136\u53ea\u7b97 1 \u4eba\u3002"
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.prepare_concept_review <- function(review) {
  review <- .as_df(review)
  .stop_if(is.null(review) || !nrow(review), "\u5ba1\u67e5\u8868\u4e0d\u80fd\u4e3a\u7a7a\u3002")
  .need_cols(review, c("concept_id", "standardized_name", "construct"), "\u6982\u5ff5\u5ba1\u67e5\u8868")
  review$concept_id <- as.character(review$concept_id)
  review$standardized_name <- as.character(review$standardized_name)
  .stop_if(anyDuplicated(review$concept_id) > 0L, "concept_id \u5fc5\u987b\u552f\u4e00\u3002")
  review$construct_raw <- as.character(review$construct)
  review$construct <- .canon_construct(review$construct)
  optional <- c(
    "in_scope", "covered_by", "external_support", "external_note",
    "coder_a", "coder_b", "adjudicator", "adjudicated_construct",
    "proposed_item_id", "proposed_text", "domain", "parent_item_id",
    "mapped_item_id", "modification_type", "n_independent", "evaluable"
  )
  for (nm in optional) if (!nm %in% names(review)) review[[nm]] <- NA
  review
}

.screen_one_concept <- function(row, suggestions, ni, enter_round, existing) {
  resolved <- .resolve_coder_conflict(row)
  construct <- resolved$construct
  n_info <- .independent_count(suggestions, row)
  n <- n_info$n
  channel <- "none"
  decision <- NA_character_
  reason <- NA_character_
  q <- list(q1 = NA, q2 = NA, q3 = NA, q4 = NA, q5 = NA)
  mod_type <- .canon_modification(row$modification_type, allow_na = TRUE)
  if (identical(construct, "wording")) mod_type <- "wording"
  if (identical(construct, "substantive")) {
    mod_type <- "substantive"
    construct <- "modification"
  }
  in_scope <- if (is.na(row$in_scope) || !nzchar(as.character(row$in_scope))) {
    !identical(construct, "out_of_scope")
  } else {
    .as_bool(row$in_scope, default = TRUE)
  }
  if (identical(construct, "out_of_scope")) in_scope <- FALSE
  external <- .as_bool(row$external_support, default = FALSE)
  covered <- .nz_chr(row$covered_by)
  name <- .nz_chr(row$standardized_name)
  evaluable <- if (is.na(row$evaluable) || !nzchar(as.character(row$evaluable))) TRUE else .as_bool(row$evaluable, TRUE)

  if (isTRUE(resolved$pending)) {
    decision <- "pending"
    reason <- "\u4e24\u540d\u7f16\u7801\u8005\u4e0d\u4e00\u81f4\uff0c\u4e14\u6ca1\u6709\u7b2c\u4e09\u4eba\u88c1\u51b3\u3002\u5728\u88c1\u51b3\u524d\u4e0d\u8fdb\u5165\u4e0b\u4e00\u8f6e\u3002"
  } else if (!in_scope) {
    q$q1 <- FALSE
    decision <- "exclude"
    reason <- "\u8d85\u51fa\u672c\u7814\u7a76\u9884\u8bbe\u8303\u56f4\u3002\u5373\u4f7f\u6709\u591a\u540d\u4e13\u5bb6\u63d0\u51fa\uff0c\u4eba\u6570\u4e5f\u4e0d\u80fd\u51cc\u9a7e\u4e8e\u8303\u56f4\u3002"
  } else {
    q$q1 <- TRUE
    if (construct %in% c("duplicate", "hierarchy") || (nzchar(covered) && construct == "new_item")) {
      q$q2 <- FALSE
      decision <- "merge"
      reason <- sprintf(
        "\u5df2\u88ab\u539f\u6761\u76ee%s\u8986\u76d6\uff0c\u5408\u5e76\u800c\u4e0d\u65b0\u589e\u3002",
        if (nzchar(covered)) paste0("\u300c", covered, "\u300d") else ""
      )
    } else if (identical(construct, "instrument")) {
      q$q2 <- TRUE
      q$q3 <- FALSE
      decision <- "redirect"
      reason <- "\u8fd9\u662f\u6d4b\u91cf\u5de5\u5177\uff0c\u4e0d\u662f\u65b0\u7684\u7ed3\u5c40\u6216\u8bc4\u4ef7\u6784\u5ff5\u3002\u8f6c\u5165\u540e\u7eed\u6d4b\u91cf\u5de5\u5177\u9074\u9009\uff0c\u4e0d\u8fdb\u5165\u4e0b\u4e00\u8f6e\u8bc4\u5206\u3002"
    } else if (identical(construct, "timepoint")) {
      q$q2 <- TRUE
      q$q3 <- FALSE
      decision <- "redirect"
      reason <- "\u8fd9\u662f\u6d4b\u91cf\u65f6\u95f4\u70b9\u6216\u65f6\u95f4\u7a97\uff0c\u4e0d\u662f\u72ec\u7acb\u7684\u65b0\u7ed3\u5c40\u3002\u8bb0\u5728\u76f8\u5e94\u6761\u76ee\u4e0b\uff0c\u4e0d\u5355\u72ec\u8fdb\u5165\u4e0b\u4e00\u8f6e\u3002"
    } else if (identical(construct, "unevaluable") || !evaluable || !nzchar(name)) {
      q$q2 <- TRUE
      q$q3 <- TRUE
      q$q5 <- FALSE
      decision <- "exclude"
      reason <- "\u4e0d\u80fd\u5f62\u6210\u6e05\u6670\u3001\u5355\u4e00\u3001\u53ef\u89c2\u5bdf\u6216\u53ef\u6d4b\u91cf\u7684\u6761\u76ee\u3002"
    } else if (identical(construct, "modification") && identical(mod_type, "wording")) {
      q$q2 <- TRUE
      q$q3 <- FALSE
      decision <- "wording"
      reason <- "\u53ea\u662f\u6587\u5b57\u89c4\u8303\uff0c\u6ca1\u6709\u6539\u53d8\u8bc4\u4ef7\u6784\u5ff5\u3002\u4fdd\u7559\u539f\u7f16\u53f7\uff0c\u4e0d\u4f5c\u4e3a\u65b0\u6761\u76ee\uff0c\u4e5f\u4e0d\u91cd\u65b0\u8ba1\u7b97\u8bc4\u5206\u8f6e\u6b21\u3002"
    } else if (identical(construct, "modification") && !mod_type %in% c("wording", "substantive")) {
      decision <- "pending"
      reason <- "\u539f\u6761\u76ee\u4fee\u6539\u5fc5\u987b\u5199\u660e\u662f\u6587\u5b57\u4fee\u6539\uff08wording\uff09\u8fd8\u662f\u5b9e\u8d28\u6027\u4fee\u6539\uff08substantive\uff09\u3002"
    } else if (construct %in% c("new_item", "modification")) {
      q$q2 <- TRUE
      q$q3 <- TRUE
      q$q5 <- TRUE
      if (!is.na(n) && n >= ni$min_independent) {
        q$q4 <- TRUE
        decision <- "admit"
        channel <- "A"
        reason <- sprintf(
          "\u81f3\u5c11 %s \u540d\u4e13\u5bb6\u72ec\u7acb\u63d0\u51fa\uff0c\u5e76\u901a\u8fc7\u8303\u56f4\u3001\u72ec\u7acb\u6027\u548c\u53ef\u8bc4\u4ef7\u6027\u5ba1\u67e5\u3002",
          ni$min_independent
        )
      } else if (!is.na(n) && n == 1L && external && isTRUE(ni$allow_single_with_evidence)) {
        q$q4 <- TRUE
        decision <- "admit"
        channel <- "B"
        reason <- "\u4ec5 1 \u540d\u4e13\u5bb6\u63d0\u51fa\uff0c\u4f46\u6709\u6587\u732e\u3001\u8bbf\u8c08\u3001\u6307\u5357\u6216\u91cd\u8981\u4e34\u5e8a\u4f9d\u636e\u3002\u63d0\u51fa\u9891\u7387\u4e0d\u7b49\u4e8e\u91cd\u8981\u6027\uff0c\u6545\u8d70\u4f8b\u5916\u901a\u9053\u3002"
      } else if (!isTRUE(ni$allow)) {
        q$q4 <- FALSE
        decision <- "exclude"
        reason <- "\u65b9\u6848\u672a\u5141\u8bb8\u65b0\u589e\u6761\u76ee\u3002"
      } else {
        q$q4 <- FALSE
        decision <- "exclude"
        reason <- "\u72ec\u7acb\u63d0\u51fa\u4eba\u6570\u4e0d\u8db3\uff0c\u4e14\u6ca1\u6709\u53ef\u9760\u7684\u5916\u90e8\u8bc1\u636e\u3002\u65b0\u589e\u9636\u6bb5\u4e0d\u662f\u5171\u8bc6\u8868\u51b3\uff0c\u4f46\u4ecd\u9700\u8981\u8fbe\u5230\u9884\u8bbe\u7684\u652f\u6301\u6761\u4ef6\u3002"
      }
      if (identical(decision, "admit") && identical(mod_type, "substantive")) {
        reason <- paste0(reason, "\u8be5\u4fee\u6539\u6539\u53d8\u4e86\u539f\u6761\u76ee\u542b\u4e49\uff0c\u6309\u65b0\u7248\u672c\u91cd\u65b0\u8bc4\u5206\u3002")
      }
    } else {
      decision <- "pending"
      reason <- sprintf("\u65e0\u6cd5\u8bc6\u522b\u7684\u7f16\u7801\uff1a%s\u3002", construct)
    }
  }

  if (identical(decision, "admit")) {
    extra <- .nz_chr(row$external_note)
    if (nzchar(extra)) reason <- paste0(reason, extra)
  }
  decision_zh <- c(
    admit = "\u8fdb\u5165\u4e0b\u4e00\u8f6e",
    exclude = "\u4e0d\u8fdb\u5165",
    merge = "\u5408\u5e76\uff0c\u4e0d\u65b0\u589e",
    redirect = "\u8f6c\u5165\u76f8\u5e94\u7814\u7a76\u9636\u6bb5",
    wording = "\u4ec5\u6587\u5b57\u4fee\u6539",
    pending = "\u5f85\u88c1\u51b3"
  )
  proposed <- .nz_chr(row$proposed_item_id)
  if (!nzchar(proposed) && identical(decision, "admit")) {
    proposed <- paste0("NEW-", row$concept_id)
  }
  text <- .nz_chr(row$proposed_text)
  if (!nzchar(text)) text <- name
  item_row <- NULL
  if (identical(decision, "admit")) {
    origin <- if (identical(mod_type, "substantive")) "substantive" else "new"
    item_row <- data.frame(
      item_id = proposed,
      item_text = text,
      domain = .blank_to_na(row$domain),
      introduced_round = enter_round,
      origin = origin,
      modification = if (identical(mod_type, "substantive")) "substantive" else "none",
      parent_item_id = .blank_to_na(row$parent_item_id),
      source_type = "\u7b2c 1 \u8f6e\u4e13\u5bb6\u65b0\u589e",
      source_note = sprintf("\u901a\u9053%s\uff1b\u72ec\u7acb\u63d0\u51fa %s \u4eba\u3002%s", channel, n, reason),
      stringsAsFactors = FALSE
    )
    if (proposed %in% existing) {
      reason <- paste0(reason, "\u8be5\u7f16\u53f7\u5df2\u5728\u6761\u76ee\u6c60\u4e2d\uff0c\u4e0d\u518d\u91cd\u590d\u8ffd\u52a0\u3002")
      item_row <- NULL
    }
  }
  data.frame(
    concept_id = row$concept_id,
    standardized_name = name,
    construct = if (identical(mod_type, "substantive")) "substantive" else construct,
    n_independent = n,
    count_note = n_info$note,
    in_scope = in_scope,
    covered_by = if (nzchar(covered)) covered else NA_character_,
    external_support = external,
    channel = channel,
    decision = decision,
    decision_zh = unname(ifelse(decision %in% names(decision_zh), decision_zh[decision], decision)),
    reason = reason,
    q1_in_scope = q$q1,
    q2_not_covered = q$q2,
    q3_independent = q$q3,
    q4_support = q$q4,
    q5_evaluable = q$q5,
    proposed_item_id = if (nzchar(proposed)) proposed else NA_character_,
    mapped_item_id = .blank_to_na(row$mapped_item_id),
    parent_item_id = .blank_to_na(row$parent_item_id),
    coder_a = .blank_to_na(row$coder_a),
    coder_b = .blank_to_na(row$coder_b),
    adjudicator = .blank_to_na(row$adjudicator),
    stringsAsFactors = FALSE
  ) -> concept_row
  # data.frame() inside -> loses item. Return via list from a cleaner structure.
  list(row = concept_row, item = item_row)
}

.resolve_coder_conflict <- function(row) {
  construct <- as.character(row$construct)
  a <- .nz_chr(row$coder_a)
  b <- .nz_chr(row$coder_b)
  if (!nzchar(a) || !nzchar(b) || identical(.canon_construct(a), .canon_construct(b))) {
    return(list(construct = construct, pending = FALSE))
  }
  adj_c <- .nz_chr(row$adjudicated_construct)
  adjudicator <- .nz_chr(row$adjudicator)
  if (nzchar(adj_c)) {
    return(list(construct = .canon_construct(adj_c), pending = FALSE))
  }
  if (nzchar(adjudicator) && nzchar(construct)) {
    return(list(construct = construct, pending = FALSE))
  }
  list(construct = construct, pending = TRUE)
}

.independent_count <- function(suggestions, row) {
  cid <- as.character(row$concept_id)
  stated <- suppressWarnings(as.integer(row$n_independent))
  if (is.null(suggestions) || !nrow(suggestions) || !"concept_id" %in% names(suggestions)) {
    return(list(n = if (length(stated) && !is.na(stated)) stated else NA_integer_, note = "\u4f7f\u7528\u5ba1\u67e5\u8868\u4e2d\u7684\u4eba\u6570"))
  }
  sub <- suggestions[!is.na(suggestions$concept_id) & as.character(suggestions$concept_id) == cid, , drop = FALSE]
  if (!nrow(sub)) {
    if (length(stated) == 1L && !is.na(stated)) {
      return(list(n = stated, note = "\u8be5\u6982\u5ff5\u6ca0\u6709\u9010\u6761\u5efa\u8bae\uff0c\u4f7f\u7528\u5ba1\u67e5\u8868\u4e2d\u7684\u4eba\u6570"))
    }
    return(list(n = 0L, note = "\u6ca0\u6709\u5bf9\u5e94\u5efa\u8bae"))
  }
  n <- length(unique(sub$expert_id))
  note <- "\u6309\u5efa\u8bae\u8868\u7684\u4e13\u5bb6\u7f16\u53f7\u53bb\u91cd"
  if (length(stated) == 1L && !is.na(stated) && !identical(stated, n)) {
    note <- sprintf("\u5ba1\u67e5\u8868\u5199 %s \u4eba\uff0c\u6309\u72ec\u7acb\u4e13\u5bb6\u8ba1\u4e3a %s \u4eba\uff0c\u91c7\u7528\u540e\u8005\u3002", stated, n)
  }
  if (nrow(sub) > n) {
    note <- paste0(note, "\u3002\u540c\u4e00\u4e13\u5bb6\u7684\u91cd\u590d\u4e66\u5199\u5df2\u5408\u5e76\u3002")
  }
  list(n = as.integer(n), note = note)
}

.canon_construct <- function(x) {
  raw <- trimws(as.character(x))
  key <- c(
    new_item = "new_item", "\u65b0\u6761\u76ee" = "new_item", "\u65b0\u6307\u6807" = "new_item",
    duplicate = "duplicate", "\u91cd\u590d" = "duplicate", "\u91cd\u590d\u6761\u76ee" = "duplicate",
    hierarchy = "hierarchy", "\u4e0a\u4e0b\u4f4d" = "hierarchy",
    "\u4e0a\u4f4d/\u4e0b\u4f4d\u6982\u5ff5" = "hierarchy", "\u4e0a\u4f4d\u6982\u5ff5" = "hierarchy",
    "\u4e0b\u4f4d\u6982\u5ff5" = "hierarchy",
    modification = "modification", "\u4fee\u6539" = "modification",
    "\u539f\u6761\u76ee\u4fee\u6539\u5efa\u8bae" = "modification",
    wording = "wording", "\u63aa\u8f9e\u4fee\u6539" = "wording", "\u6587\u5b57\u4fee\u6539" = "wording",
    substantive = "substantive", "\u5b9e\u8d28\u6027\u4fee\u6539" = "substantive",
    instrument = "instrument", "\u6d4b\u91cf\u5de5\u5177" = "instrument",
    timepoint = "timepoint", "\u6d4b\u91cf\u65f6\u95f4\u70b9" = "timepoint",
    "\u65f6\u95f4\u7a97" = "timepoint", "\u6d4b\u91cf\u65f6\u70b9" = "timepoint",
    out_of_scope = "out_of_scope", "\u8303\u56f4\u5916" = "out_of_scope",
    "\u8303\u56f4\u5916\u9879\u76ee" = "out_of_scope",
    unevaluable = "unevaluable", "\u65e0\u6cd5\u8bc4\u4ef7" = "unevaluable",
    "\u65e0\u6cd5\u5f62\u6210\u53ef\u8bc4\u4ef7\u6982\u5ff5" = "unevaluable"
  )
  out <- unname(key[raw])
  bad <- is.na(out) & nzchar(raw) & !is.na(raw)
  .stop_if(any(bad), sprintf("\u65e0\u6cd5\u8bc6\u522b\u7684\u7f16\u7801: %s", paste(unique(raw[bad]), collapse = ", ")))
  out[is.na(out)] <- ""
  out
}

.canon_modification <- function(x, allow_na = FALSE) {
  raw <- trimws(as.character(x))
  if (length(raw) != 1L) raw <- raw[1]
  if (is.na(raw) || !nzchar(raw)) {
    if (allow_na) return(NA_character_)
    stop("modification \u4e0d\u80fd\u4e3a\u7a7a\u3002", call. = FALSE)
  }
  key <- c(
    wording = "wording", "\u6587\u5b57\u4fee\u6539" = "wording", "\u63aa\u8f9e\u4fee\u6539" = "wording",
    substantive = "substantive", "\u5b9e\u8d28\u6027\u4fee\u6539" = "substantive",
    none = "none", "\u65e0" = "none"
  )
  hit <- unname(key[raw])
  .stop_if(is.na(hit), sprintf("modification \u53ea\u80fd\u662f wording \u6216 substantive\uff0c\u6536\u5230 %s\u3002", raw))
  hit
}

.item_clock <- function(items) {
  ids <- as.character(items$item_id)
  introduced <- if ("introduced_round" %in% names(items)) {
    x <- suppressWarnings(as.integer(items$introduced_round))
    x[is.na(x)] <- 1L
    x
  } else {
    rep(1L, length(ids))
  }
  origin <- if ("origin" %in% names(items)) as.character(items$origin) else rep("initial", length(ids))
  origin[is.na(origin) | !nzchar(origin)] <- "initial"
  modification <- if ("modification" %in% names(items)) as.character(items$modification) else rep("none", length(ids))
  modification[is.na(modification) | !nzchar(modification)] <- "none"
  list(
    introduced = stats::setNames(introduced, ids),
    origin = stats::setNames(origin, ids),
    modification = stats::setNames(modification, ids)
  )
}

.as_bool <- function(x, default = FALSE) {
  if (is.logical(x)) {
    x[is.na(x)] <- default
    return(as.logical(x))
  }
  y <- tolower(trimws(as.character(x)))
  out <- y %in% c("true", "t", "1", "yes", "y", "\u662f", "\u6709")
  empty <- is.na(x) | is.na(y) | !nzchar(y) | y %in% c("na", "null")
  out[empty] <- default
  out
}

.nz_chr <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x)) return("")
  x
}

.blank_to_na <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(x)) NA_character_ else x
}

.active_item_ids <- function(items) {
  if (is.null(items) || !nrow(items)) return(character())
  ids <- as.character(items$item_id)
  if ("superseded_by" %in% names(items)) {
    gone <- !is.na(items$superseded_by) & nzchar(as.character(items$superseded_by))
    ids <- ids[!gone]
  }
  ids
}

.entering_item_ids <- function(items, round) {
  if (is.null(items) || !nrow(items) || !"introduced_round" %in% names(items)) {
    return(character())
  }
  intro <- suppressWarnings(as.integer(items$introduced_round))
  ids <- as.character(items$item_id)
  keep <- !is.na(intro) & intro == as.integer(round)
  if ("superseded_by" %in% names(items)) {
    gone <- !is.na(items$superseded_by) & nzchar(as.character(items$superseded_by))
    keep <- keep & !gone
  }
  ids[keep]
}

.items_for_round <- function(study, cls, items, round) {
  all_ids <- .active_item_ids(study$items)
  round <- as.integer(round)
  if (identical(study$protocol$scenario, "delphi_ram")) return(all_ids)
  if (round <= 1L || identical(items, "all") || is.null(cls) || !nrow(cls)) {
    return(all_ids)
  }
  src_round <- round - 1L
  dim0 <- tryCatch(.primary_dimension(study, round), error = function(e) NULL)
  new_ids <- .entering_item_ids(study$items, round)
  if (identical(items, "iteration")) {
    keep <- .iteration_item_ids(cls, src_round, dim0)
  } else {
    keep <- .unresolved_item_ids(cls, src_round, "unresolved", dim0)
  }
  unique(c(intersect(all_ids, keep), intersect(all_ids, new_ids)))
}

.iteration_item_ids <- function(cls, round, dimension = NULL) {
  sub <- cls
  if (is.null(sub) || !nrow(sub)) return(character())
  if ("round" %in% names(sub)) sub <- sub[sub$round == round, , drop = FALSE]
  if (!is.null(dimension) && "dimension" %in% names(sub) && nrow(sub)) {
    hit <- sub[sub$dimension == dimension, , drop = FALSE]
    if (nrow(hit)) sub <- hit
  }
  if (!nrow(sub)) return(character())
  flag <- rep(FALSE, nrow(sub))
  if ("provisional" %in% names(sub)) flag <- flag | sub$provisional %in% TRUE
  if ("action" %in% names(sub)) flag <- flag | sub$action %in% c("rescore", "discuss")
  if ("class" %in% names(sub)) {
    flag <- flag | sub$class %in% c("borderline", "no_consensus", "polarized", "uncertain")
  }
  if ("action" %in% names(sub)) {
    done <- sub$action %in% c("keep", "drop")
    if ("provisional" %in% names(sub)) done <- done & !sub$provisional %in% TRUE
    flag <- flag & !done
  }
  if ("modification" %in% names(sub)) flag <- flag | sub$modification %in% "substantive"
  unique(as.character(sub$item_id[flag]))
}

.latest_class_rows <- function(cls) {
  if (!"round" %in% names(cls)) return(cls)
  cls <- cls[order(cls$round), , drop = FALSE]
  dim <- if ("dimension" %in% names(cls)) as.character(cls$dimension) else rep("", nrow(cls))
  key <- paste(cls$item_id, dim, sep = "||")
  cls[!duplicated(key, fromLast = TRUE), , drop = FALSE]
}

.round_result_cell <- function(classification, item_id, round) {
  if (is.null(classification) || !nrow(classification) || !"round" %in% names(classification)) {
    return("\u2014")
  }
  sub <- classification[classification$item_id == item_id & classification$round == round, , drop = FALSE]
  if (!nrow(sub)) return("\u2014")
  if ("dimension" %in% names(sub)) {
    prefer <- intersect(
      c("\u91cd\u8981\u6027", "\u91cd\u8981\u7a0b\u5ea6", "overall"),
      unique(as.character(sub$dimension))
    )
    if (length(prefer)) sub <- sub[sub$dimension == prefer[1], , drop = FALSE]
  }
  if (!nrow(sub)) return("\u2014")
  ph <- if ("p_high" %in% names(sub)) sprintf("\uff1b7\u20139\u5206 %.1f%%", sub$p_high[1]) else ""
  pl <- if ("p_low" %in% names(sub)) sprintf("\uff1b1\u20133\u5206 %.1f%%", sub$p_low[1]) else ""
  paste0(sub$class_zh[1], ph, pl)
}

.final_result_cell <- function(classification, item_id) {
  if (is.null(classification) || !nrow(classification)) return("\u2014")
  sub <- classification[classification$item_id == item_id, , drop = FALSE]
  if (!nrow(sub)) return("\u2014")
  sub <- .latest_class_rows(sub)
  if ("dimension" %in% names(sub)) {
    prefer <- intersect(
      c("\u91cd\u8981\u6027", "\u91cd\u8981\u7a0b\u5ea6", "overall"),
      unique(as.character(sub$dimension))
    )
    if (length(prefer)) sub <- sub[sub$dimension == prefer[1], , drop = FALSE]
  }
  if (!nrow(sub)) return("\u2014")
  as.character(sub$class_zh[1])
}

.empty_changelog <- function() {
  data.frame(
    item_id = character(), source = character(), text_r1 = character(),
    suggestion = character(), decision_r1 = character(), text_scored = character(),
    result_r2 = character(), result_r3 = character(), final = character(),
    reason = character(), stringsAsFactors = FALSE
  )
}

.suggestion_log_row <- function(row, suggestion, decision_zh, reason) {
  id <- if ("concept_id" %in% names(row)) as.character(row$concept_id) else "SUG"
  data.frame(
    item_id = id,
    source = "\u7b2c 1 \u8f6e\u5f00\u653e\u5efa\u8bae",
    text_r1 = "\u2014",
    suggestion = suggestion,
    decision_r1 = decision_zh,
    text_scored = "\u2014",
    result_r2 = "\u2014",
    result_r3 = "\u2014",
    final = "\u672a\u8fdb\u5165\u8bc4\u5206",
    reason = reason,
    stringsAsFactors = FALSE
  )
}
