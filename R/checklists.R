.accord_items <- function() {
  data.frame(
    id = c(
      "T1",
      "I1", "I2", "I3",
      paste0("M", 1:21),
      paste0("R", 1:5),
      "D1", "D2",
      "O1", "O2", "O3"
    ),
    section = c(
      "\u9898\u76ee",
      rep("\u5f15\u8a00", 3),
      rep("\u65b9\u6cd5", 21),
      rep("\u7ed3\u679c", 5),
      rep("\u8ba8\u8bba", 2),
      rep("\u5176\u4ed6", 3)
    ),
    item = c(
      "\u5728\u9898\u76ee\u4e2d\u6807\u660e\u8fd9\u662f\u5171\u8bc6\u7814\u7a76\uff0c\u5e76\u8bf4\u660e\u6240\u7528\u65b9\u6cd5\uff08\u5982 Delphi\u3001RAND/UCLA\uff09\u3002",
      "\u89e3\u91ca\u4e3a\u4f55\u9700\u8981\u91c7\u7528\u5171\u8bc6\u65b9\u6cd5\uff0c\u800c\u4e0d\u662f\u4ec5\u4f9d\u8d56\u5df2\u6709\u8bc1\u636e\u3002",
      "\u8bf4\u660e\u5171\u8bc6\u8fc7\u7a0b\u7684\u76ee\u7684\u4e0e\u5177\u4f53\u76ee\u6807\u3002",
      "\u8bf4\u660e\u76ee\u6807\u4f7f\u7528\u8005\u3001\u9002\u7528\u8303\u56f4\u4e0e\u5730\u7406\u8303\u56f4\u3002",
      "\u662f\u5426\u9884\u5148\u6ce8\u518c\u6216\u516c\u5f00\u65b9\u6848\u3002",
      "\u53c2\u4e0e\u8005\uff08\u4e13\u5bb6\uff09\u7684\u7eb3\u5165\u6807\u51c6\u3002",
      "\u5982\u4f55\u8bc6\u522b\u3001\u9080\u8bf7\u53c2\u4e0e\u8005\u3002",
      "\u9080\u8bf7\u4eba\u6570\u53ca\u5176\u7406\u7531\u3002",
      "\u60a3\u8005\u4e0e\u516c\u4f17\u53c2\u4e0e\uff08PPI\uff09\u3002",
      "\u6307\u5bfc\u59d4\u5458\u4f1a/\u65b9\u6cd5\u5b66\u7ec4\u7684\u7ec4\u6210\u4e0e\u804c\u8d23\u3002",
      "\u5229\u76ca\u51b2\u7a81\u7684\u8bc6\u522b\u4e0e\u7ba1\u7406\u3002",
      "\u6761\u76ee\u6216\u9648\u8ff0\u5982\u4f55\u751f\u6210\uff08\u8bc1\u636e\u7efc\u5408\uff09\u3002",
      "\u6536\u96c6\u610f\u89c1\u5e76\u5f62\u6210\u5171\u8bc6\u7684\u5177\u4f53\u65b9\u6cd5\u6b65\u9aa4\u3002",
      "\u8ba1\u5212\u8f6e\u6b21\u6570\u4e0e\u7ec8\u6b62\u89c4\u5219\u3002",
      "\u5171\u8bc6\u7684\u9884\u5148\u5b9a\u4e49\u3002",
      "\u672a\u8fbe\u5171\u8bc6\u6216\u5206\u6b67\u7684\u5904\u7406\u65b9\u5f0f\u3002",
      "\u533f\u540d\u7684\u7a0b\u5ea6\u4e0e\u5b9e\u73b0\u65b9\u5f0f\u3002",
      "\u8f6e\u6b21\u4e4b\u95f4\u7684\u53d7\u63a7\u53cd\u9988\u5185\u5bb9\u3002",
      "\u6761\u76ee\u5728\u8f6e\u6b21\u95f4\u5982\u4f55\u4fee\u6539\u3002",
      "\u6570\u636e\u6536\u96c6\u65b9\u5f0f\u4e0e\u8f6f\u4ef6\u3002",
      "\u662f\u5426\u53ca\u5982\u4f55\u8fdb\u884c\u9884\u6d4b\u8bd5\u3002",
      "\u81ea\u7531\u6587\u672c\u7684\u5b9a\u6027\u5206\u6790\u65b9\u6cd5\u3002",
      "\u5b9a\u91cf\u5206\u6790\u65b9\u6cd5\u548c\u6307\u6807\u3002",
      "\u7f3a\u5931\u6570\u636e\uff08\u542b\u65e0\u6cd5\u5224\u65ad\uff09\u7684\u5904\u7406\u3002",
      "\u4f26\u7406\u5ba1\u67e5\u4e0e\u77e5\u60c5\u540c\u610f\u3002",
      "\u5404\u8f6e\u53c2\u4e0e\u4eba\u6570\u4e0e\u6d41\u5931\u3002",
      "\u53c2\u4e0e\u8005\u7279\u5f81\u3002",
      "\u5404\u8f6e\u5b9a\u91cf\u7ed3\u679c\u3002",
      "\u5b9a\u6027\u7ed3\u679c\u3002",
      "\u8fc7\u7a0b\u4e2d\u5bf9\u6761\u76ee\u7684\u4fee\u6539\u3002",
      "\u5171\u8bc6\u8fc7\u7a0b\u7684\u5c40\u9650\u6027\u3002",
      "\u7ed3\u679c\u5e94\u5982\u4f55\u4f7f\u7528\uff1b\u5171\u8bc6\u4e0d\u7b49\u4e8e\u9ad8\u8d28\u91cf\u8bc1\u636e\u3002",
      "\u8d44\u52a9\u6765\u6e90\u3002",
      "\u4f5c\u8005\u5229\u76ca\u51b2\u7a81\u3002",
      "\u65b9\u6848\u3001\u6570\u636e\u4e0e\u6750\u6599\u7684\u53ef\u5f97\u6027\u3002"
    ),
    stringsAsFactors = FALSE
  )
}

.dcat_items <- function() {
  data.frame(
    id = paste0("DCAT", 1:16),
    core = c(TRUE, TRUE, TRUE, TRUE, rep(FALSE, 12)),
    item = c(
      "\u533f\u540d\uff1a\u8bc4\u5206\u5728\u6536\u96c6\u4e0e\u62a5\u544a\u65f6\u662f\u5426\u9002\u5f53\u4fdd\u6301\u533f\u540d\u3002",
      "\u8fed\u4ee3\uff1a\u662f\u5426\u81f3\u5c11\u4e24\u8f6e\u4f7f\u7528\u76f8\u540c\u8bc4\u5206\u534f\u8bae\uff0c\u5e76\u5141\u8bb8\u4fee\u6539\u539f\u5224\u65ad\u3002",
      "\u7edf\u8ba1\u6c47\u603b\uff1a\u662f\u5426\u7528\u9002\u5f53\u7684\u96c6\u4e2d\u4e0e\u79bb\u6563\u6307\u6807\u4ee5\u53ca\u9884\u5148\u5b9a\u4e49\u7684\u5171\u8bc6\u89c4\u5219\u3002",
      "\u53d7\u63a7\u53cd\u9988\uff1a\u662f\u5426\u63d0\u4f9b\u7ec4\u7ed3\u679c\u53ca\u4e2a\u6027\u5316\u7684\u672c\u4eba\u4e0a\u4e00\u8f6e\u8bc4\u5206\u3002",
      "\u662f\u5426\u5145\u5206\u8bba\u8bc1\u9009\u62e9 Delphi \u7684\u7406\u7531\u3002",
      "Delphi \u7c7b\u578b\u662f\u5426\u4e0e\u7814\u7a76\u95ee\u9898\u5339\u914d\u3002",
      "\u4e13\u5bb6\u51c6\u5165\u6807\u51c6\u4e0e\u6784\u6210\u662f\u5426\u9002\u5f53\u3002",
      "\u662f\u5426\u62a5\u544a\u5e76\u5c3d\u529b\u964d\u4f4e\u8131\u843d\u3002",
      "\u7b2c\u4e00\u8f6e\u524d\u662f\u5426\u5b8c\u6210\u5e76\u5206\u4eab\u9884\u5907\u5de5\u4f5c\uff08\u8bc1\u636e/\u5f00\u653e\u9898\uff09\u3002",
      "\u8bbe\u8ba1\u9009\u62e9\uff08\u8f6e\u6b21\u3001\u505c\u6b62\u89c4\u5219\u3001\u8ba8\u8bba\u4f1a\uff09\u662f\u5426\u5408\u7406\u4e14\u4e0d\u5f3a\u8feb\u5171\u8bc6\u3002",
      "\u8c03\u67e5\u95ee\u9898\u662f\u5426\u6e05\u695a\u3001\u65e0\u8bf1\u5bfc\u3001\u65e0\u590d\u5408\u63d0\u95ee\u3002",
      "\u5e94\u7b54\u5c3a\u5ea6\u662f\u5426\u5408\u7406\u5e76\u5141\u8bb8\u6587\u5b57\u8bf4\u660e\u3002",
      "\u662f\u5426\u9884\u6d4b\u8bd5\u3002",
      "\u5206\u6790\u662f\u5426\u9884\u5148\u89c4\u5b9a\u4e14\u5b8c\u6574\uff08\u542b\u7f3a\u5931\uff09\u3002",
      "\u662f\u5426\u6309 ACCORD \u7b49\u89c4\u8303\u900f\u660e\u62a5\u544a\u3002",
      "\u4f26\u7406\u95ee\u9898\u662f\u5426\u5f97\u5230\u5904\u7406\u3002"
    ),
    stringsAsFactors = FALSE
  )
}

.credes_items <- function() {
  data.frame(
    id = paste0("CR", 1:12),
    item = c(
      "\u8bf4\u660e\u4e3a\u4f55\u9009\u62e9 Delphi\uff0c\u4ee5\u53ca\u76ee\u7684\u662f\u63a2\u7d22\u3001\u5171\u8bc6\u8fd8\u662f\u653f\u7b56\u6f84\u6e05\u3002",
      "\u8bf4\u660e\u4e13\u5bb6\u9009\u62e9\u6807\u51c6\u3001\u9080\u8bf7\u8fc7\u7a0b\u4e0e\u5229\u76ca\u51b2\u7a81\u3002",
      "\u8bf4\u660e\u9884\u5907\u5de5\u4f5c\u5982\u4f55\u5f62\u6210\u8c03\u67e5\u5185\u5bb9\u3002",
      "\u9884\u5148\u5b9a\u4e49\u5171\u8bc6\u3001\u8f6e\u6b21\u548c\u7ec8\u6b62\u6807\u51c6\u3002",
      "\u8bf4\u660e\u533f\u540d\u3001\u53cd\u9988\u4e0e\u95ee\u5377\u7ed3\u6784\u3002",
      "\u8bf4\u660e\u9884\u6d4b\u8bd5\u3002",
      "\u5206\u522b\u62a5\u544a\u6bcf\u4e00\u8f6e\u7684\u5b9a\u91cf\u7ed3\u679c\u3002",
      "\u62a5\u544a\u5b9a\u6027\u610f\u89c1\u7684\u5904\u7406\u3002",
      "\u62a5\u544a\u53c2\u4e0e/\u8131\u843d\u3002",
      "\u8ba8\u8bba\u7a33\u5b9a\u6027\u4e0e\u672a\u8fbe\u5171\u8bc6\u7684\u6761\u76ee\u3002",
      "\u8ba8\u8bba\u65b9\u6cd5\u5b66\u5c40\u9650\u3002",
      "\u63d0\u4f9b\u53ef\u91cd\u590d\u7684\u65b9\u6cd5\u5b66\u7ec6\u8282\u3002"
    ),
    stringsAsFactors = FALSE
  )
}

.auto_status <- function(result, which = "accord") {
  study <- result$study
  meta <- study$meta
  has <- function(nm) !is.null(meta[[nm]]) && !identical(meta[[nm]], "")
  p <- study$protocol
  if (which == "accord") {
    yes <- c(
      T1 = TRUE,
      I1 = has("objective"),
      I2 = has("objective"),
      I3 = has("users") || has("geography"),
      M1 = FALSE,
      M2 = TRUE,
      M3 = FALSE,
      M4 = TRUE,
      M5 = FALSE,
      M6 = FALSE,
      M7 = FALSE,
      M8 = has("evidence_note"),
      M9 = TRUE,
      M10 = TRUE,
      M11 = TRUE,
      M12 = TRUE,
      M13 = has("anonymity"),
      M14 = TRUE,
      M15 = FALSE,
      M16 = TRUE,
      M17 = isTRUE(meta$pilot_tested),
      M18 = !is.null(study$comments),
      M19 = TRUE,
      M20 = TRUE,
      M21 = has("ethics"),
      R1 = TRUE,
      R2 = TRUE,
      R3 = TRUE,
      R4 = !is.null(study$comments),
      R5 = FALSE,
      D1 = FALSE,
      D2 = TRUE,
      O1 = FALSE,
      O2 = FALSE,
      O3 = FALSE
    )
    ids <- .accord_items()$id
    unname(ifelse(ids %in% names(yes) & yes[ids], "Yes",
                  ifelse(ids %in% names(yes), "Unsure", "Unsure")))
  } else if (which == "dcat") {
    c(
      "Yes", "Yes", "Yes", "Yes",
      if (has("objective")) "Yes" else "Unsure",
      "Yes",
      "Yes",
      "Yes",
      if (has("evidence_note")) "Yes" else "Unsure",
      "Yes",
      "Unsure",
      "Yes",
      if (isTRUE(meta$pilot_tested)) "Yes" else "Unsure",
      "Yes",
      "Yes",
      if (has("ethics")) "Yes" else "Unsure"
    )
  } else {
    c(
      if (has("objective")) "Yes" else "Unsure",
      "Yes",
      if (has("evidence_note")) "Yes" else "Unsure",
      "Yes", "Yes",
      if (isTRUE(meta$pilot_tested)) "Yes" else "Unsure",
      "Yes",
      if (!is.null(study$comments)) "Yes" else "Unsure",
      "Yes", "Yes", "Unsure", "Yes"
    )
  }
}

#' ACCORD 报告清单对照
#'
#' ACCORD（2024）是生物医学共识方法的通用报告规范，共 35 条。
#' 能从分析对象推断的条目标 Yes，需作者补充的标 Unsure。
#'
#' @param result [analyze_delphi()] 结果；若为空则只返回清单正文。
#'
#' @return 数据框：`id`, `section`, `item`, `status`, `note`。
#'
#' @examples
#' accord_checklist()[, c("id", "section")]
#'
#' @export
accord_checklist <- function(result = NULL) {
  d <- .accord_items()
  d$status <- if (is.null(result)) "Unsure" else .auto_status(result, "accord")
  d$note <- ifelse(d$status == "Unsure", "\u9700\u5728\u6587\u7a3f\u4e2d\u7531\u4f5c\u8005\u8865\u5145", "\u53ef\u7531 delphir \u5bf9\u8c61\u652f\u6301")
  .add_class(d, "delphi_checklist")
}

#' DCAT 关键评价清单
#'
#' DCAT（BMJ 2025）含 4 个核心项（匿名、迭代、统计汇总、受控反馈）和 12 个附加项。
#' 核心项“否”视为关键缺陷。
#'
#' @param result 可选 [analyze_delphi()] 结果。
#'
#' @return 数据框。
#'
#' @examples
#' dcat_checklist()
#'
#' @export
dcat_checklist <- function(result = NULL) {
  d <- .dcat_items()
  d$status <- if (is.null(result)) "Unsure" else .auto_status(result, "dcat")
  d$note <- ifelse(d$core, "\u6838\u5fc3\u9879", "\u9644\u52a0\u9879")
  .add_class(d, "delphi_checklist")
}

#' CREDES 报告要点对照
#'
#' CREDES 是较早的 Delphi 方法与报告框架，适合作为 ACCORD 的补充。
#'
#' @param result 可选分析结果。
#'
#' @return 数据框。
#'
#' @examples
#' credes_checklist()
#'
#' @export
credes_checklist <- function(result = NULL) {
  d <- .credes_items()
  d$status <- if (is.null(result)) "Unsure" else .auto_status(result, "credes")
  d$note <- ifelse(d$status == "Unsure", "\u9700\u4f5c\u8005\u8865\u5145", "\u53ef\u7531\u5bf9\u8c61\u652f\u6301")
  .add_class(d, "delphi_checklist")
}
