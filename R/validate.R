#' 校验 Delphi 研究对象
#'
#' 检查 ID 对齐、量表范围、轮次连续性、重复评分，以及阈值与量表是否匹配。
#'
#' @param study [delphi_study()] 对象。
#' @param error 发现问题时是报错（默认）还是仅返回问题表。
#'
#' @return 若通过，不可见地返回 `study`；若 `error = FALSE`，返回问题数据框。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 10, n_items = 5, seed = 2)
#' validate_delphi(study)
#'
#' @export
validate_delphi <- function(study, error = TRUE) {
  .stop_if(!inherits(study, "delphi_study"), "\u9700\u8981 delphi_study \u5bf9\u8c61\u3002")
  issues <- data.frame(
    level = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  add <- function(level, message) {
    issues <<- rbind(issues, data.frame(level = level, message = message,
                                        stringsAsFactors = FALSE))
  }

  p <- study$protocol
  r <- study$ratings
  panel_ids <- study$panel$expert_id
  item_ids <- study$items$item_id

  unknown_e <- setdiff(unique(r$expert_id), panel_ids)
  if (length(unknown_e)) add("error", sprintf("\u8bc4\u5206\u8868\u4e2d\u6709\u4e13\u5bb6\u4e0d\u5728\u9762\u677f: %s", paste(unknown_e, collapse = ", ")))
  unknown_i <- setdiff(unique(r$item_id), item_ids)
  if (length(unknown_i)) add("error", sprintf("\u8bc4\u5206\u8868\u4e2d\u6709\u6761\u76ee\u4e0d\u5728\u6761\u76ee\u6807: %s", paste(unknown_i, collapse = ", ")))

  bad <- !is.na(r$score) & (r$score < p$scale_min | r$score > p$scale_max)
  if (any(bad)) add("error", sprintf("\u6709 %s \u6761\u8bc4\u5206\u8d85\u51fa\u91cf\u8868\u8303\u56f4 %s\u2013%s\u3002", sum(bad), p$scale_min, p$scale_max))

  non_int <- !is.na(r$score) & abs(r$score - round(r$score)) > 1e-8
  if (any(non_int)) add("warning", "\u5b58\u5728\u975e\u6574\u6570\u8bc4\u5206\uff1bLikert \u901a\u5e38\u5e94\u4e3a\u6574\u6570\u3002")

  rounds <- .unique_sorted(r$round)
  if (length(rounds) && !identical(as.integer(rounds), seq.int(min(rounds), max(rounds)))) {
    add("warning", "\u8f6e\u6b21\u7f16\u53f7\u4e0d\u8fde\u7eed\u3002")
  }
  if (length(rounds) < 2L) {
    add("warning", "\u76ee\u524d\u53ea\u6709\u4e00\u8f6e\u8bc4\u5206\u3002DCAT \u5c06\u8fed\u4ee3\u89c6\u4e3a Delphi \u6838\u5fc3\u7279\u5f81\uff0c\u81f3\u5c11\u5e94\u6709\u4e24\u8f6e\u76f8\u540c\u91cf\u8868\u3002")
  }

  if (!is.null(study$comments)) {
    c_e <- setdiff(unique(study$comments$expert_id), panel_ids)
    c_i <- setdiff(unique(study$comments$item_id), item_ids)
    if (length(c_e)) add("warning", "\u610f\u89c1\u8868\u4e2d\u6709\u4e13\u5bb6\u4e0d\u5728\u9762\u677f\u3002")
    if (length(c_i)) add("warning", "\u610f\u89c1\u8868\u4e2d\u6709\u6761\u76ee\u4e0d\u5728\u6761\u76ee\u6807\u3002")
  }

  if (p$high_range[2] > p$scale_max || p$low_range[1] < p$scale_min) {
    add("error", "\u5171\u8bc6\u533a\u95f4\u4e0e\u91cf\u8868\u7aef\u70b9\u4e0d\u5339\u914d\u3002")
  }
  if (identical(p$scenario, "standard_element")) {
    comp <- tryCatch(panel_compliance(study), error = function(e) NULL)
    if (!is.null(comp)) {
      bad <- comp[!comp$ok & comp$level == "warn", , drop = FALSE]
      if (nrow(bad)) {
        for (i in seq_len(nrow(bad))) add("warning", bad$message[i])
      }
    }
  }

  errs <- issues$message[issues$level == "error"]
  warns <- issues$message[issues$level == "warning"]
  if (length(warns)) {
    for (w in warns) warning(w, call. = FALSE)
  }
  if (length(errs) && isTRUE(error)) {
    stop(paste(c("validate_delphi \u672a\u901a\u8fc7:", errs), collapse = "\n  - "), call. = FALSE)
  }
  if (!isTRUE(error)) return(issues)
  invisible(study)
}
