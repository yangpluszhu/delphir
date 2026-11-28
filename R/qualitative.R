#' 汇总自由文本意见（主题编码计频）
#'
#' 对方法学组预先编码的 `theme` 做计数，并给出代表性摘录。
#' 本函数不做自动 NLP 或大模型推断；定性分析应由研究组完成编码后再汇总。
#'
#' @param comments [delphi_comments()] 或数据框。
#' @param study 可选，从中取出 comments。
#' @param round 轮次，默认全部。
#'
#' @return 列表：`by_theme` 主题频次；`by_item` 条目×主题；`examples` 摘录。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 16, n_items = 8, seed = 15)
#' q <- summarize_comments(study = study)
#' q$by_theme
#'
#' @export
summarize_comments <- function(comments = NULL, study = NULL, round = NULL) {
  if (is.null(comments)) {
    study <- .match_study(study)
    comments <- study$comments
  }
  if (is.null(comments) || nrow(comments) == 0L) {
    return(list(
      by_theme = data.frame(theme = character(), n = integer()),
      by_item = data.frame(),
      examples = data.frame(),
      note = "\u65e0\u81ea\u7531\u6587\u672c\u610f\u89c1\u3002"
    ))
  }
  comments <- .as_df(comments)
  if (!is.null(round)) comments <- comments[comments$round %in% round, ]
  if (!"theme" %in% names(comments) || all(is.na(comments$theme))) {
    comments$theme <- "\u672a\u7f16\u7801"
  }
  by_theme <- as.data.frame(table(comments$theme), stringsAsFactors = FALSE)
  names(by_theme) <- c("theme", "n")
  by_item <- as.data.frame(table(comments$item_id, comments$theme),
                           stringsAsFactors = FALSE)
  names(by_item) <- c("item_id", "theme", "n")
  by_item <- by_item[by_item$n > 0, ]
  examples <- comments
  if (nrow(examples) > 30L) {
    examples <- examples[order(examples$item_id), ][seq_len(30L), ]
  }
  list(
    by_theme = by_theme[order(-by_theme$n), ],
    by_item = by_item,
    examples = examples,
    note = "\u4e3b\u9898\u5e94\u7531\u65b9\u6cd5\u5b66\u7ec4\u9884\u5148\u7f16\u7801\u3002ACCORD \u8981\u6c42\u8bf4\u660e\u81ea\u7531\u6587\u672c\u5982\u4f55\u8fdb\u884c\u5b9a\u6027\u5904\u7406\u3002"
  )
}
