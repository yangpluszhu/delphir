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
#'
#' @param protocol [delphi_protocol()] 对象。
#' @param panel [delphi_panel()] 或数据框。
#' @param items [delphi_items()] 或数据框。
#' @param ratings [delphi_ratings()] 或数据框。
#' @param comments [delphi_comments()] 或数据框，可省略。
#' @param meta 命名列表。建议字段：`title`、`objective`、`users`、`geography`、
#'   `evidence_note`、`pilot_tested`、`anonymity`、`ethics`。
#' @param validate 是否立即调用 [validate_delphi()]，默认 `TRUE`。
#'
#' @return `delphi_study` 对象。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 12, n_items = 6, n_rounds = 2, seed = 1)
#' study
#'
#' @export
delphi_study <- function(protocol, panel, items, ratings, comments = NULL,
                         meta = list(), validate = TRUE) {
  .stop_if(!inherits(protocol, "delphi_protocol"), "protocol \u5fc5\u987b\u662f delphi_protocol \u5bf9\u8c61\u3002")
  if (!inherits(panel, "delphi_panel")) panel <- delphi_panel(panel)
  if (!inherits(items, "delphi_items")) items <- delphi_items(items)
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
  if (isTRUE(validate)) validate_delphi(study)
  study
}

#' @export
print.delphi_study <- function(x, ...) {
  cat("Delphi \u7814\u7a76\n")
  if (!is.null(x$meta$title)) cat(sprintf("  \u9898\u76ee: %s\n", x$meta$title))
  cat(sprintf("  \u573a\u666f: %s\n", .scenario_label(x$protocol$scenario)))
  cat(sprintf("  \u4e13\u5bb6: %s \u4eba\uff1b\u6761\u76ee: %s\uff1b\u8f6e\u6b21: %s\n",
              nrow(x$panel), nrow(x$items),
              paste(.unique_sorted(x$ratings$round), collapse = ", ")))
  cat(sprintf("  \u8bc4\u5206\u884c\u6570: %s\n", nrow(x$ratings)))
  invisible(x)
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
