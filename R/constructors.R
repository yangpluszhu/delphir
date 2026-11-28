#' 整理专家面板表
#'
#' 将专家特征整理为标准数据框。Delphi 采用目的抽样，本函数不要求随机样本，
#' 但要求每位专家有唯一 `expert_id`。
#'
#' @param data 数据框。必填列：`expert_id`。建议列：`stakeholder`、`specialty`、
#'   `region`、`title`、`years`。国内发表可选问卷原始勾选 `familiarity`、
#'   `practice`、`theory`、`literature`、`intuition`，或已赋分的 `Ca`（判断依据）、
#'   `Cs`（熟悉程度）。Ca/Cs 按专家填一次，不要按条目重复。可选 `invited`
#'   （是否受邀，逻辑值）。
#'
#' @return 带类 `"delphi_panel"` 的数据框。
#'
#' @section 输入输出:
#' **输入**一行一位专家。`expert_id` 必须唯一。
#'
#' **输出**原表加上类标记；若缺 `invited` 则默认全部为 `TRUE`。
#'
#' @examples
#' panel <- delphi_panel(data.frame(
#'   expert_id = paste0("E", 1:3),
#'   stakeholder = c("临床", "方法学", "临床"),
#'   years = c(12, 8, 20)
#' ))
#' panel
#'
#' @export
delphi_panel <- function(data) {
  data <- .as_df(data)
  .need_cols(data, "expert_id", "\u4e13\u5bb6\u8868")
  data$expert_id <- as.character(data$expert_id)
  .stop_if(anyDuplicated(data$expert_id) > 0, "expert_id \u5fc5\u987b\u552f\u4e00\u3002")
  if (!"invited" %in% names(data)) data$invited <- TRUE
  if (!"stakeholder" %in% names(data)) data$stakeholder <- NA_character_
  data$stakeholder <- as.character(data$stakeholder)
  .add_class(data, "delphi_panel")
}

#' 整理共识条目/结局/指征表
#'
#' @param data 数据框。必填列：`item_id`、`item_text`。建议列：`domain`。
#'   COS 可加 `outcome_domain`；RAND/UCLA 可加 `indication`、`procedure`、
#'   `evidence_level`。
#'
#' @return 带类 `"delphi_items"` 的数据框。
#'
#' @examples
#' items <- delphi_items(data.frame(
#'   item_id = c("I1", "I2"),
#'   item_text = c("应预先规定缺失数据处理策略。", "应报告脱落原因。"),
#'   domain = c("统计", "实施")
#' ))
#' items
#'
#' @export
delphi_items <- function(data) {
  data <- .as_df(data)
  .need_cols(data, c("item_id", "item_text"), "\u6761\u76ee\u6807")
  data$item_id <- as.character(data$item_id)
  data$item_text <- as.character(data$item_text)
  .stop_if(anyDuplicated(data$item_id) > 0, "item_id \u5fc5\u987b\u552f\u4e00\u3002")
  if (!"domain" %in% names(data)) data$domain <- NA_character_
  .add_class(data, "delphi_items")
}

#' 整理长表评分
#'
#' 一行表示一位专家在某一轮、某一条目、某一维度上的一次评分。
#'
#' @param data 数据框。必填列：`expert_id`、`item_id`、`round`、`score`。
#'   可选：`dimension`（如重要性/可行性，缺省为 `"overall"`）、
#'   `unable`（逻辑值，超出专业领域）、`stakeholder`。
#'   `score` 为 Likert 整数；无法判断时应为 `NA` 且 `unable = TRUE`。
#'
#' @return 带类 `"delphi_ratings"` 的数据框。
#'
#' @section 方法学要点:
#' “无法判断 / 超出本人专业领域”不得强迫计入 1–9 分母，应单独报告。
#'
#' @examples
#' ratings <- delphi_ratings(data.frame(
#'   expert_id = c("E1", "E2", "E1"),
#'   item_id = c("I1", "I1", "I1"),
#'   round = c(1, 1, 2),
#'   score = c(8, NA, 9),
#'   unable = c(FALSE, TRUE, FALSE)
#' ))
#' ratings
#'
#' @export
delphi_ratings <- function(data) {
  data <- .as_df(data)
  .need_cols(data, c("expert_id", "item_id", "round", "score"), "\u8bc4\u5206\u8868")
  data$expert_id <- as.character(data$expert_id)
  data$item_id <- as.character(data$item_id)
  data$round <- as.integer(data$round)
  data$score <- as.numeric(data$score)
  data <- .ensure_dimension(data)
  data <- .ensure_unable(data)
  key <- paste(data$expert_id, data$item_id, data$round, data$dimension, sep = "|")
  .stop_if(anyDuplicated(key) > 0, "\u540c\u4e00\u4e13\u5bb6-\u6761\u76ee-\u8f6e\u6b21-\u7ef4\u5ea6\u4e0d\u80fd\u6709\u91cd\u590d\u8bc4\u5206\u3002")
  .add_class(data, "delphi_ratings")
}

#' 整理自由文本意见
#'
#' @param data 数据框。必填列：`expert_id`、`item_id`、`round`、`comment`。
#'   可选：`theme`（方法学组预先编码的主题，如支持/条件性支持/表述问题/反对/新增）。
#'
#' @return 带类 `"delphi_comments"` 的数据框。
#'
#' @examples
#' delphi_comments(data.frame(
#'   expert_id = "E1",
#'   item_id = "I1",
#'   round = 1,
#'   comment = "仅适用于高风险干预性研究。",
#'   theme = "条件性支持"
#' ))
#'
#' @export
delphi_comments <- function(data) {
  data <- .as_df(data)
  .need_cols(data, c("expert_id", "item_id", "round", "comment"), "\u610f\u89c1\u8868")
  data$expert_id <- as.character(data$expert_id)
  data$item_id <- as.character(data$item_id)
  data$round <- as.integer(data$round)
  data$comment <- as.character(data$comment)
  if (!"theme" %in% names(data)) data$theme <- NA_character_
  data <- data[!is.na(data$comment) & nzchar(trimws(data$comment)), , drop = FALSE]
  .add_class(data, "delphi_comments")
}

#' 组装 Delphi 研究对象
#'
#' 把方案、专家、条目、评分（及可选意见）装入单一 S3 对象，供后续分析使用。
#' 第一轮发放前若尚无评分，请用 [delphi_blueprint()]。
#'
#' @param protocol [delphi_protocol()] 对象。
#' @param panel [delphi_panel()] 或数据框。
#' @param items [delphi_items()] 或数据框。
#' @param ratings [delphi_ratings()] 或数据框。可省略（空表）以表示尚未回收。
#' @param comments [delphi_comments()] 或数据框，可省略。
#' @param meta 命名列表。建议字段：`title`、`objective`、`users`、`geography`、
#'   `evidence_note`、`pilot_tested`、`anonymity`、`ethics`。
#' @param validate 是否立即调用 [validate_delphi()]，默认 `TRUE`。无评分时自动跳过。
#'
#' @return `delphi_study` 对象。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 12, n_items = 6, n_rounds = 2, seed = 1)
#' study
#'
#' @export
delphi_study <- function(protocol, panel, items, ratings = NULL, comments = NULL,
                         meta = list(), validate = TRUE) {
  .stop_if(!inherits(protocol, "delphi_protocol"), "protocol \u5fc5\u987b\u662f delphi_protocol \u5bf9\u8c61\u3002")
  if (!inherits(panel, "delphi_panel")) panel <- delphi_panel(panel)
  if (!inherits(items, "delphi_items")) items <- delphi_items(items)
  if (is.null(ratings)) ratings <- .empty_ratings()
  if (!inherits(ratings, "delphi_ratings")) ratings <- delphi_ratings(ratings)
  if (!is.null(comments) && !inherits(comments, "delphi_comments")) {
    comments <- delphi_comments(comments)
  }
  study <- structure(
    list(
      protocol = protocol,
      panel = panel,
      items = items,
      ratings = ratings,
      comments = comments,
      meta = as.list(meta)
    ),
    class = "delphi_study"
  )
  if (isTRUE(validate) && nrow(ratings) > 0L) validate_delphi(study)
  study
}

#' 第一轮发放前的研究规格（尚无评分）
#'
#' 把已锁定的方案、专家面板和条目组装为 [delphi_study()]，评分表为空。
#' 供 [delphi_questionnaire()] 生成第 1 轮 docx/xlsx 问卷。
#'
#' @param protocol [delphi_protocol()] 对象。
#' @param panel [delphi_panel()] 或数据框。
#' @param items [delphi_items()] 或数据框。
#' @param meta 命名列表，见 [delphi_study()]。
#'
#' @return `delphi_study` 对象，`ratings` 为空。
#'
#' @examples
#' bp <- delphi_blueprint(
#'   protocol = preset_protocol("clinical_consensus"),
#'   panel = data.frame(expert_id = c("E01", "E02"), stakeholder = c("临床", "方法学")),
#'   items = data.frame(item_id = "C01", item_text = "应预先规定缺失数据处理策略。"),
#'   meta = list(title = "示例", objective = "教学", users = "研究者", geography = "中国大陆")
#' )
#' nrow(bp$ratings)
#'
#' @export
delphi_blueprint <- function(protocol, panel, items, meta = list()) {
  delphi_study(
    protocol = protocol,
    panel = panel,
    items = items,
    ratings = .empty_ratings(),
    comments = NULL,
    meta = meta,
    validate = FALSE
  )
}

#' 把新一轮回收结果并入研究（不覆盖历史轮）
#'
#' 按 `expert_id` × `item_id` × `round` × `dimension` 追加评分与意见。
#' 若新数据与已有轮次键重复则报错，避免把第 1 轮覆盖成第 2 轮。
#' 脱落专家的上轮评分必须保留，不要从 `study$ratings` 中删除。
#'
#' @param study [delphi_study()] 对象。
#' @param ratings 新评分（通常只含新一轮）。
#' @param comments 新意见，可省略。
#' @param panel 若提供，按 `expert_id` 更新专家表（用于写入 Ca/Cs），不删除原行。
#' @param items 若提供，按 `item_id` 追加尚未存在的条目（第 1 轮新增条目）。
#' @param validate 合并后是否 [validate_delphi()]，默认 `TRUE`。
#'
#' @return 更新后的 `delphi_study`。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 3)
#' extra <- study$ratings
#' extra$round <- 2L
#' study2 <- update_delphi_study(study, ratings = extra)
#' unique(study2$ratings$round)
#'
#' @export
update_delphi_study <- function(study, ratings = NULL, comments = NULL,
                                panel = NULL, items = NULL, validate = TRUE) {
  study <- .match_study(study)
  if (!is.null(panel)) {
    if (!inherits(panel, "delphi_panel")) panel <- delphi_panel(panel)
    old <- study$panel
    extra_ids <- setdiff(panel$expert_id, old$expert_id)
    if (length(extra_ids)) {
      study$panel <- .rbind_fill(old, panel[panel$expert_id %in% extra_ids, , drop = FALSE])
    }
    common <- intersect(panel$expert_id, study$panel$expert_id)
    if (length(common)) {
      for (nm in setdiff(names(panel), "expert_id")) {
        if (!nm %in% names(study$panel)) study$panel[[nm]] <- NA
        idx <- match(common, study$panel$expert_id)
        src <- match(common, panel$expert_id)
        study$panel[[nm]][idx] <- panel[[nm]][src]
      }
    }
    if (!inherits(study$panel, "delphi_panel")) study$panel <- delphi_panel(study$panel)
  }
  if (!is.null(items)) {
    if (!inherits(items, "delphi_items")) items <- delphi_items(items)
    extra <- items[!items$item_id %in% study$items$item_id, , drop = FALSE]
    if (nrow(extra)) {
      study$items <- .rbind_fill(study$items, extra)
      study$items <- delphi_items(study$items)
    }
  }
  if (!is.null(ratings)) {
    if (!inherits(ratings, "delphi_ratings")) ratings <- delphi_ratings(ratings)
    key_old <- .rating_key(study$ratings)
    key_new <- .rating_key(ratings)
    overlap <- intersect(key_new, key_old)
    .stop_if(
      length(overlap) > 0L,
      sprintf(
        "\u65b0\u8bc4\u5206\u4e0e\u5df2\u6709\u8f6e\u6b21\u91cd\u590d %s \u6761\uff0c\u62d2\u7edd\u8986\u76d6\u3002\u8bf7\u53ea\u4f20\u5165\u65b0\u4e00\u8f6e\u3002",
        length(overlap)
      )
    )
    study$ratings <- delphi_ratings(.rbind_fill(study$ratings, ratings))
  }
  if (!is.null(comments) && nrow(.as_df(comments))) {
    if (!inherits(comments, "delphi_comments")) comments <- delphi_comments(comments)
    if (is.null(study$comments) || !nrow(study$comments)) {
      study$comments <- comments
    } else {
      study$comments <- delphi_comments(.rbind_fill(study$comments, comments))
    }
  }
  if (isTRUE(validate) && nrow(study$ratings) > 0L) validate_delphi(study)
  study
}

.empty_ratings <- function() {
  delphi_ratings(data.frame(
    expert_id = character(),
    item_id = character(),
    round = integer(),
    score = numeric(),
    dimension = character(),
    unable = logical(),
    stringsAsFactors = FALSE
  ))
}

.rating_key <- function(ratings) {
  if (is.null(ratings) || nrow(ratings) == 0L) return(character())
  ratings <- .ensure_dimension(.ensure_unable(ratings))
  paste(ratings$expert_id, ratings$item_id, ratings$round, ratings$dimension, sep = "|")
}

.rbind_fill <- function(a, b) {
  a <- .as_df(a)
  b <- .as_df(b)
  if (is.null(a) || nrow(a) == 0L) return(b)
  if (is.null(b) || nrow(b) == 0L) return(a)
  nms <- union(names(a), names(b))
  for (nm in nms) {
    if (!nm %in% names(a)) a[[nm]] <- NA
    if (!nm %in% names(b)) b[[nm]] <- NA
  }
  rbind(a[nms], b[nms])
}

#' @export
print.delphi_study <- function(x, ...) {
  cat("Delphi \u7814\u7a76\n")
  if (!is.null(x$meta$title)) cat(sprintf("  \u9898\u76ee: %s\n", x$meta$title))
  cat(sprintf("  \u573a\u666f: %s\n", .scenario_label(x$protocol$scenario)))
  rnd <- if (is.null(x$ratings) || nrow(x$ratings) == 0L) {
    "\u5c1a\u672a\u8bc4\u5206"
  } else {
    paste(.unique_sorted(x$ratings$round), collapse = ", ")
  }
  cat(sprintf("  \u4e13\u5bb6: %s \u4eba\uff1b\u6761\u76ee: %s\uff1b\u8f6e\u6b21: %s\n",
              nrow(x$panel), nrow(x$items), rnd))
  cat(sprintf("  \u8bc4\u5206\u884c\u6570: %s\n", if (is.null(x$ratings)) 0L else nrow(x$ratings)))
  invisible(x)
}

#' @export
summary.delphi_study <- function(object, ...) {
  if (is.null(object$ratings) || nrow(object$ratings) == 0L) {
    return(list(
      protocol = object$protocol,
      n_experts = nrow(object$panel),
      n_items = nrow(object$items),
      rounds = integer(),
      participation = NULL
    ))
  }
  part <- response_summary(object)
  list(
    protocol = object$protocol,
    n_experts = nrow(object$panel),
    n_items = nrow(object$items),
    rounds = .unique_sorted(object$ratings$round),
    participation = part$by_round
  )
}

#' @export
summary.delphi_study <- function(object, ...) {
  part <- response_summary(object)
  list(
    protocol = object$protocol,
    n_experts = nrow(object$panel),
    n_items = nrow(object$items),
    rounds = .unique_sorted(object$ratings$round),
    participation = part$by_round
  )
}
