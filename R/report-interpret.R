#' 报告中的扩展指标解读
#'
#' 供五套报告模板调用。数字只描述专家组、离散程度和评分者一致性，
#' 不替换锁定规则，也不给 Likert 加权。
#'
#' @param result [analyze_delphi()] 的结果。
#'
#' @return 无。以 Markdown 写入当前报告。
#'
#' @export
report_extended_section <- function(result) {
  .stop_if(!inherits(result, "delphi_result"), "\u9700\u8981 delphi_result\u3002")
  cat("\n\n# \u6269\u5c55\u6307\u6807\uff08\u8865\u5145\uff0c\u4e0d\u53c2\u4e0e\u5206\u7c7b\uff09\n\n")
  cat(paste0(
    "\u4e0b\u9762\u8fd9\u4e9b\u6570\u63cf\u8ff0\u4e13\u5bb6\u662f\u5426\u56de\u5377\u3001\u5bf9\u4e3b\u9898\u662f\u5426\u81ea\u8bc4\u6709\u628a\u63e1\u3001",
    "\u6574\u4f53\u6392\u5e8f\u662f\u5426\u63a5\u8fd1\uff0c\u4ee5\u53ca\u8bc4\u5206\u4f5c\u4e3a\u6d4b\u91cf\u662f\u5426\u7a33\u3002",
    "\u5b83\u4eec\u4e0d\u51b3\u5b9a\u67d0\u4e00\u6761\u8fdb\u4e0d\u8fdb\u6807\u51c6\uff0c\u4e5f\u4e0d\u7ed9\u5206\u6570\u52a0\u6743\u3002",
    "\u6b63\u5f0f\u7ed3\u8bba\u4ecd\u662f\u9501\u5b9a\u89c4\u5219\u4e0b\u7684\u5206\u7c7b\u3002\n\n"
  ))
  part <- result$participation$by_round
  if (is.data.frame(part) && nrow(part)) {
    cat(paste0(
      "**\u79ef\u6781\u7cfb\u6570**\u5c31\u662f\u95ee\u5377\u56de\u6536\u7387\uff1a\u8fd9\u4e00\u8f6e\u6709\u591a\u5c11\u53d7\u9080\u4e13\u5bb6\u628a\u95ee\u5377\u5bc4\u56de\u6765\u4e86\u3002",
      "\u5b83\u8bf4\u660e\u4e13\u5bb6\u613f\u4e0d\u613f\u610f\u53c2\u52a0\uff0c\u4e0d\u8bf4\u660e\u610f\u89c1\u662f\u5426\u4e00\u81f4\u3002",
      "\u8fbe\u5230\u53c2\u8003\u7ebf\u53ea\u8868\u793a\u56de\u5377\u6bd4\u4f8b\u4e0d\u4f4e\u4e8e\u65b9\u6848\u91cc\u5199\u597d\u7684\u53c2\u8003\u503c\uff0c\u4e0d\u662f\u5408\u683c\u7ed3\u8bba\uff0c",
      "\u4e5f\u4e0d\u80fd\u9760\u5220\u6389\u672a\u56de\u590d\u7684\u4eba\u6765\u628a\u6bd4\u4f8b\u505a\u9ad8\u3002\n\n"
    ))
    .report_print_table(part, "\u5404\u8f6e\u79ef\u6781\u7cfb\u6570\uff08\u56de\u6536\u7387\uff09", digits = 1)
  }
  .report_authority_block(result)
  .report_dispersion_block(result)
  .report_kendall_block(result)
  .report_reliability_block(result)
  invisible(result)
}

#' 报告中的轮次比较解读
#'
#' @param result [analyze_delphi()] 的结果。
#' @return 无。以 Markdown 写入当前报告。
#' @export
report_rounds_section <- function(result) {
  .stop_if(!inherits(result, "delphi_result"), "\u9700\u8981 delphi_result\u3002")
  cmp <- result$round_comparison
  cat("\n\n# \u8f6e\u6b21\u95f4\u6bd4\u8f83\n\n")
  cat(paste0(
    "\u6761\u76ee\u7ea7\u6bd4\u8f83\u76f8\u90bb\u8f6e\u7684\u4e2d\u4f4d\u6570\u3001IPR\uff08\u7b2c 70 \u4e0e\u7b2c 30 \u767e\u5206\u4f4d\u6570\u4e4b\u5dee\uff09\u548c\u5206\u7c7b\u3002",
    "`changed` \u4e3a TRUE \u8868\u793a\u8fd9\u4e00\u6761\u7684\u5206\u7c7b\u53d8\u4e86\u3002",
    "\u7ec4\u5c42\u9762\u770b\u8fbe\u6210\u5171\u8bc6\u6761\u76ee\u6bd4\u4f8b\u3001\u5e73\u5747 IPR \u548c Kendall's W \u600e\u4e48\u53d8\u3002",
    "IPR \u5e73\u5747\u4e0b\u964d\u8868\u793a\u5206\u6570\u66f4\u96c6\u4e2d\uff0c\u4e0a\u5347\u8868\u793a\u66f4\u5206\u6563\uff1b",
    "W \u5347\u9ad8\u8868\u793a\u6392\u5e8f\u66f4\u63a5\u8fd1\u3002\u8fd9\u4e9b\u90fd\u662f\u63cf\u8ff0\uff0c\u4e0d\u662f\u505c\u6b62\u89c4\u5219\u3002\n\n"
  ))
  if (is.null(cmp) || (is.null(cmp$group) && is.null(cmp$items))) {
    msg <- if (is.null(cmp)) {
      "\u672a\u8ba1\u7b97\u8f6e\u6b21\u6bd4\u8f83\u3002"
    } else {
      cmp$note %||% cmp$error %||% "\u672a\u8ba1\u7b97\u8f6e\u6b21\u6bd4\u8f83\u3002"
    }
    cat(msg, "\n\n")
    return(invisible(result))
  }
  if (!is.null(cmp$consensus_definition) && !is.na(cmp$consensus_definition)) {
    cat(sprintf("\u672c\u573a\u666f\u7684\u7ec4\u5c42\u9762\u53e3\u5f84\uff1a%s\u3002\n\n", cmp$consensus_definition))
  }
  if (is.data.frame(cmp$group) && nrow(cmp$group)) {
    .report_print_table(cmp$group, "\u5404\u8f6e\u7ec4\u5c42\u9762\uff1a\u5171\u8bc6\u6bd4\u4f8b\u3001\u5e73\u5747 IPR \u4e0e Kendall's W", digits = 3)
  }
  if (is.data.frame(cmp$group_change) && nrow(cmp$group_change)) {
    .report_print_table(cmp$group_change, "\u76f8\u90bb\u8f6e\u7ec4\u5c42\u9762\u53d8\u5316", digits = 3)
    cat(paste0(
      "\u600e\u4e48\u8bfb\uff1a\u5171\u8bc6\u6bd4\u4f8b\u4e0a\u5347\uff0c\u53ea\u8bf4\u660e\u6309\u5f53\u524d\u53e3\u5f84\u7b97\u8fdb\u5165\u8be5\u7c7b\u7684\u6761\u76ee\u53d8\u591a\u4e86\uff1b",
      "\u5e73\u5747 IPR \u53d8\u5c0f\uff0c\u8bf4\u660e\u6574\u4f53\u66f4\u6536\u655b\u3002",
      "\u4e0d\u8981\u628a\u6bd4\u4f8b\u5347\u9ad8\u5199\u6210\u5df2\u7ecf\u53ef\u4ee5\u653e\u5bbd\u754c\u503c\u3002\n\n"
    ))
  }
  items <- cmp$items
  if (is.data.frame(items) && nrow(items) && "changed" %in% names(items)) {
    keep <- intersect(c(
      "item_id", "dimension", "from_round", "to_round",
      "median_from", "median_to", "ipr_from", "ipr_to",
      "class_zh_from", "class_zh_to", "changed", "wilcoxon_p"
    ), names(items))
    .report_print_table(items[, keep, drop = FALSE], "\u6761\u76ee\u5728\u76f8\u90bb\u8f6e\u662f\u5426\u8f6c\u53d8", digits = 3)
  } else if (is.data.frame(items) && nrow(items)) {
    .report_print_table(utils::head(items, 30), "\u8f6e\u6b21\u660e\u7ec6\uff08\u622a\u53d6\uff09", digits = 3)
  }
  cat(cmp$note %||% "", "\n\n")
  cat(paste0(
    "Wilcoxon \u7b26\u53f7\u79e9\u68c0\u9a8c\u53ea\u63cf\u8ff0\u540c\u4e00\u6279\u4e13\u5bb6\u524d\u540e\u5206\u6570\u662f\u5426\u6574\u4f53\u79fb\u52a8\u3002",
    "**\u5b83\u662f\u63a2\u7d22\u6027\u7684\u3002** P \u503c\u5c0f\u4e0d\u8868\u793a\u5df2\u7ecf\u8fbe\u6210\u9002\u5b9c\u6216\u5171\u8bc6\uff0c",
    "P \u503c\u5927\u4e5f\u4e0d\u8868\u793a\u610f\u89c1\u5df2\u7ecf\u7a33\u5b9a\u6216\u5e94\u8be5\u505c\u6b62\u3002",
    "\u5206\u7c7b\u662f\u5426\u8f6c\u53d8\u770b `changed`\uff0c\u4e0d\u770b\u8fd9\u4e2a P \u503c\u3002\n\n"
  ))
  invisible(result)
}

#' 报告中的敏感性分析解读
#'
#' @param result [analyze_delphi()] 的结果。
#' @return 无。以 Markdown 写入当前报告。
#' @export
report_sensitivity_section <- function(result) {
  .stop_if(!inherits(result, "delphi_result"), "\u9700\u8981 delphi_result\u3002")
  sens <- result$sensitivity
  cat("\n\n# \u654f\u611f\u6027\u5206\u6790\uff08\u63a2\u7d22\u6027\uff09\n\n")
  cat(paste0(
    "\u654f\u611f\u6027\u5206\u6790\u7528\u6765\u770b\uff1a\u5982\u679c\u5f53\u521d\u9501\u5b9a\u7684\u662f\u53e6\u4e00\u4e2a\u90bb\u8fd1\u754c\u503c\uff0c\u6216\u53e6\u4e00\u5957\u5df2\u516c\u5f00\u7684\u89c4\u5219\uff0c",
    "\u5206\u7c7b\u4f1a\u4e0d\u4f1a\u53d8\u3002**\u5b83\u4e0d\u66ff\u6362\u9501\u5b9a\u89c4\u5219\uff0c\u4e5f\u4e0d\u80fd\u5728\u770b\u5230\u7ed3\u679c\u540e\u6539\u754c\u503c\u3002** ",
    "\u6b63\u5f0f\u7ed3\u8bba\u4ecd\u662f\u65b9\u6848\u91cc\u90a3\u4e00\u5957\u3002\u672c\u8282\u5168\u90e8\u662f\u63a2\u7d22\u6027\u63cf\u8ff0\u3002\n\n"
  ))
  if (is.null(sens)) {
    cat("\u672a\u8ba1\u7b97\u654f\u611f\u6027\u3002\n\n")
    return(invisible(result))
  }
  if (!is.null(sens$error) && is.null(sens$summary) && is.null(sens$kappa)) {
    cat(sens$error, "\n\n")
    if (!is.null(sens$note)) cat(sens$note, "\n\n")
    return(invisible(result))
  }
  if (!is.null(sens$note)) cat(sens$note, "\n\n")
  if (is.data.frame(sens$summary) && nrow(sens$summary)) {
    .report_print_table(
      sens$summary,
      "\u6bcf\u6b21\u53ea\u79fb\u52a8\u4e00\u4e2a\u754c\u503c\u65f6\uff0c\u76f8\u5bf9\u9501\u5b9a\u89c4\u5219\u53d1\u751f\u53d8\u5316\u7684\u6761\u76ee\u6570",
      digits = 3
    )
    cat(paste0(
      "\u600e\u4e48\u8bfb\uff1a`n_changed` \u5927\uff0c\u8bf4\u660e\u7ed3\u8bba\u9760\u8fd1\u754c\u503c\uff0c\u62a5\u544a\u91cc\u8981\u5199\u660e\u754c\u503c\u662f\u4e8b\u5148\u9501\u5b9a\u7684\u3002",
      "`n_changed` \u4e3a 0\uff0c\u53ea\u8bf4\u660e\u8fd9\u4e00\u6b21\u632a\u52a8\u6ca1\u6709\u6539\u53d8\u5206\u7c7b\uff0c\u4e0d\u8868\u793a\u53ef\u4ee5\u6362\u4e00\u5957\u89c4\u5219\u3002\n\n"
    ))
  }
  if (is.data.frame(sens$kappa) && nrow(sens$kappa)) {
    .report_print_table(sens$kappa, "\u4e0d\u540c\u89c4\u5219\u5206\u7c7b\u7684 Cohen's \u03ba\uff08\u63a2\u7d22\u6027\uff09", digits = 3)
    cat(paste0(
      "\u03ba \u63a5\u8fd1 1\uff0c\u8868\u793a\u4e24\u5957\u89c4\u5219\u7684\u5206\u7c7b\u51e0\u4e4e\u76f8\u540c\uff1b\u03ba \u4f4e\uff0c\u8bf4\u660e\u4e8b\u5148\u5199\u54ea\u4e00\u6761\u89c4\u5219\u4f1a\u6539\u53d8\u7ed3\u8bba\u3002",
      "\u56e0\u6b64\u4e0d\u80fd\u5728\u770b\u5230\u7ed3\u679c\u540e\u6539\u7528\u53e6\u4e00\u5957\u3002\n\n"
    ))
  }
  if (is.data.frame(sens$by_item) && nrow(sens$by_item) && ncol(sens$by_item) <= 12L && nrow(sens$by_item) <= 40L) {
    .report_print_table(sens$by_item, "\u6761\u76ee\u7ea7\u5bf9\u7167\uff08\u4ecd\u4ee5\u9501\u5b9a\u89c4\u5219\u4e3a\u6b63\u5f0f\u7ed3\u679c\uff09", digits = 3)
  }
  if (is.data.frame(sens$loo) && nrow(sens$loo)) {
    cat("\u9010\u4e00\u5254\u9664\u4e00\u540d\u4e13\u5bb6\u53ea\u8bf4\u660e\u7ed3\u8bba\u662f\u5426\u4f9d\u8d56\u5355\u4e2a\u4eba\uff0c\u4e0d\u4ea7\u751f\u65b0\u7684\u6b63\u5f0f\u89c4\u5219\u3002\n\n")
    .report_print_table(sens$loo, "\u9010\u4e00\u5254\u9664\u540e\u5206\u7c7b\u7ffb\u8f6c\u7684\u6761\u76ee\u6570", digits = 3)
  }
  if (length(sens$skipped)) cat(paste(sens$skipped, collapse = "\n"), "\n\n")
  invisible(result)
}

.report_print_table <- function(x, caption, digits = 3) {
  if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(invisible(NULL))
  if (!requireNamespace("knitr", quietly = TRUE)) {
    cat("\n\n", caption, "\n\n", sep = "")
    print(utils::head(x, 30))
    cat("\n\n")
    return(invisible(NULL))
  }
  cat("\n\n")
  cat(paste(as.character(knitr::kable(x, digits = digits, caption = caption)), collapse = "\n"))
  cat("\n\n")
  invisible(NULL)
}

.report_authority_block <- function(result) {
  cat("**\u6743\u5a01\u7cfb\u6570**\u6309\u4e13\u5bb6\u8ba1\u7b97\u4e00\u6b21\uff1aCr = (Ca + Cs) / 2\u3002")
  cat("Ca \u662f\u5224\u65ad\u4f9d\u636e\u7cfb\u6570\uff0cCs \u662f\u719f\u6089\u7a0b\u5ea6\u7cfb\u6570\u3002")
  cat("\u5b83\u63cf\u8ff0\u4e13\u5bb6\u5bf9\u672c\u6b21\u4e3b\u9898\u7684\u81ea\u8bc4\u628a\u63e1\uff0c\u4e0d\u6309\u6761\u76ee\u91cd\u590d\u8ba1\u7b97\uff0c\u4e5f\u4e0d\u7528\u6765\u7ed9\u5206\u6570\u52a0\u6743\u3002")
  cat("\u7ec4\u5747\u6570\u5e38\u4e0e 0.70 \u6bd4\u8f83\uff0c\u8fd9\u53ea\u662f\u53c2\u8003\uff0c\u4e0d\u662f\u56fd\u9645 Delphi \u7684\u6838\u5fc3\u8981\u6c42\u3002\n\n")
  if (is.null(result$authority)) {
    cat("\u672c\u6b21\u6ca1\u6709\u6743\u5a01\u7cfb\u6570\u81ea\u8bc4\u3002\u82e5\u9700\u8981\u62a5\u544a\uff0c\u8bf7\u5728\u9996\u6b21\u8bc4\u5206\u8f6e\u6536\u96c6\u719f\u6089\u7a0b\u5ea6\u4e0e\u5224\u65ad\u4f9d\u636e\u3002\n\n")
    return(invisible(NULL))
  }
  ov <- attr(result$authority, "overall")
  if (!is.null(ov)) {
    auth_tab <- data.frame(
      metric = c("\u7ec4\u5e73\u5747 Ca", "\u7ec4\u5e73\u5747 Cs", "\u7ec4\u5e73\u5747 Cr", "\u4e0d\u4f4e\u4e8e\u53c2\u8003\u503c\u7684\u4e13\u5bb6\u6bd4\u4f8b", "\u53c2\u8003\u503c"),
      value = c(
        round(ov$mean_ca, 3), round(ov$mean_cs, 3), round(ov$mean_cr, 3),
        sprintf("%.1f%%", ov$pct_ok), ov$threshold
      ),
      stringsAsFactors = FALSE
    )
    names(auth_tab) <- c("\u6307\u6807", "\u7ed3\u679c")
    .report_print_table(auth_tab, "\u4e13\u5bb6\u7ec4\u6743\u5a01\u7cfb\u6570\uff08\u4e0d\u52a0\u6743\uff09", digits = 3)
  }
  note <- attr(result$authority, "scale_note")
  if (!is.null(note)) cat(note, "\n\n")
  scs <- attr(result$authority, "scales")
  if (is.null(scs)) scs <- result$study$protocol$authority_scale
  if (is.null(scs)) scs <- authority_scales()
  .report_print_table(
    local({
      tab <- data.frame(familiarity = names(scs$cs), Cs = unname(scs$cs), stringsAsFactors = FALSE)
      names(tab) <- c("\u719f\u6089\u7a0b\u5ea6", "Cs")
      tab
    }),
    "\u672c\u6b21\u9501\u5b9a\u7684\u719f\u6089\u7a0b\u5ea6\u8d4b\u5206\u8868",
    digits = 2
  )
  ca <- scs$ca
  .report_print_table(
    local({
      tab <- data.frame(source = ca$source, large = ca$large, medium = ca$medium, small = ca$small, stringsAsFactors = FALSE)
      names(tab) <- c("\u5224\u65ad\u4f9d\u636e", "\u5927", "\u4e2d", "\u5c0f")
      tab
    }),
    "\u672c\u6b21\u9501\u5b9a\u7684\u5224\u65ad\u4f9d\u636e\u8d4b\u5206\u8868",
    digits = 2
  )
  invisible(NULL)
}

.report_dispersion_block <- function(result) {
  cat(paste0(
    "**\u6ee1\u5206\u6bd4**\u662f\u6253\u51fa\u91cf\u8868\u6700\u9ad8\u5206\u7684\u4eba\u6570\u5360\u6709\u6548\u8bc4\u5206\u4eba\u6570\u7684\u767e\u5206\u6bd4\u3002",
    "**\u53d8\u5f02\u7cfb\u6570**\u662f\u6807\u51c6\u5dee\u9664\u4ee5\u5747\u6570\u3002",
    "\u6ee1\u5206\u6bd4\u9ad8\u800c\u5747\u6570\u6216\u4e2d\u4f4d\u6570\u4e0d\u9ad8\uff0c\u901a\u5e38\u662f\u5c11\u6570\u4eba\u6253\u4e86\u6ee1\u5206\u3001\u53e6\u4e00\u4e9b\u4eba\u6253\u5f97\u5f88\u4f4e\u3002",
    "\u53d8\u5f02\u7cfb\u6570\u5927\uff0c\u8bf4\u660e\u5206\u6570\u66f4\u6563\u3002\u4e24\u8005\u90fd\u4e0d\u5355\u72ec\u51b3\u5b9a\u53bb\u7559\u3002",
    "IPR \u662f\u7b2c 70 \u4e0e\u7b2c 30 \u767e\u5206\u4f4d\u6570\u4e4b\u5dee\uff0c\u63cf\u8ff0\u4e2d\u90e8\u5206\u6563\u3002\n\n"
  ))
  last <- result$last_classification
  if (!is.data.frame(last) || !nrow(last)) return(invisible(NULL))
  keep <- intersect(c("item_id", "dimension", "n_valid", "median", "ipr", "mean", "cv", "full_rate", "class_zh"), names(last))
  if (length(keep) < 3L) return(invisible(NULL))
  .report_print_table(last[, keep, drop = FALSE], "\u672b\u8f6e\u6ee1\u5206\u6bd4\u3001\u53d8\u5f02\u7cfb\u6570\u4e0e IPR\uff08\u8865\u5145\uff09", digits = 3)
  invisible(NULL)
}

.report_kendall_block <- function(result) {
  cat(paste0(
    "**Kendall \u534f\u8c03\u7cfb\u6570 W**\u770b\u8fd9\u4e9b\u4e13\u5bb6\u7ed9\u5404\u6761\u6392\u51fa\u7684\u9ad8\u4f4e\u987a\u5e8f\u50cf\u4e0d\u50cf\u3002",
    "W \u63a5\u8fd1 0\uff0c\u987a\u5e8f\u5404\u6392\u5404\u7684\uff1b\u63a5\u8fd1 1\uff0c\u987a\u5e8f\u51e0\u4e4e\u4e00\u6837\u3002",
    "\u5361\u65b9\u68c0\u9a8c\u7684\u96f6\u5047\u8bbe\u662f\u6392\u5e8f\u4e92\u4e0d\u76f8\u5173\u3002",
    "P \u503c\u5c0f\u53ea\u8bf4\u660e\u8fd9\u79cd\u4e00\u81f4\u4e0d\u50cf\u7eaf\u5c5e\u5de7\u5408\uff0c\u5e76\u4e0d\u81ea\u52a8\u7b49\u4e8e\u6bcf\u4e00\u6761\u90fd\u5df2\u8fbe\u6210\u5171\u8bc6\u3002",
    "\u6709\u540c\u5206\u65f6\u5df2\u6309\u540c\u5206\u4fee\u6b63\uff1b\u65e0\u540c\u5206\u65f6\u6821\u6b63\u91cf\u4e3a 0\u3002\n\n"
  ))
  kw <- result$kendall
  if (is.null(kw)) return(invisible(NULL))
  if (!is.null(kw$error)) {
    cat(sprintf("\u672b\u8f6e Kendall W \u672a\u8ba1\u7b97\uff1a%s\n\n", kw$error))
    return(invisible(NULL))
  }
  if (is.null(kw$w)) return(invisible(NULL))
  kw_tab <- data.frame(
    W = kw$w,
    chi_sq = kw$chi_sq,
    df = kw$df,
    P = kw$p_value,
    tie_correction = kw$tie_correction %||% NA_real_,
    n_expert = kw$n_expert,
    n_item = kw$n_item,
    stringsAsFactors = FALSE
  )
  names(kw_tab) <- c("W", "\u5361\u65b9", "\u81ea\u7531\u5ea6", "P", "\u540c\u5206\u6821\u6b63", "\u4e13\u5bb6\u6570", "\u6761\u76ee\u6570")
  .report_print_table(kw_tab, "\u672b\u8f6e Kendall \u534f\u8c03\u7cfb\u6570\uff08\u6392\u5e8f\u534f\u8c03\u6027\uff0c\u4e0d\u662f\u6761\u76ee\u5171\u8bc6\uff09", digits = 4)
  if (!is.null(kw$note)) cat(kw$note, "\n\n")
  invisible(NULL)
}

.report_reliability_block <- function(result) {
  rel <- result$reliability
  cat(paste0(
    "**\u8bc4\u5206\u8005\u4fe1\u5ea6**\u5305\u62ec Cronbach's \u03b1\u3001\u7ec4\u5185\u76f8\u5173\u7cfb\u6570 ICC(2,1)\u3001ICC(2,k)",
    "\uff0c\u4ee5\u53ca\u5728\u5b89\u88c5\u4e86 `irr` \u65f6\u7684 Krippendorff's \u03b1\u3002",
    "\u03b1 \u4e0e ICC \u628a\u6709\u5e8f\u5206\u6570\u5f53\u4f5c\u7b49\u8ddd\uff0c\u53ea\u4f5c\u8865\u5145\u3002",
    "ICC \u628a\u6761\u76ee\u5f53\u4f5c\u5bf9\u8c61\u3001\u4e13\u5bb6\u5f53\u4f5c\u968f\u673a\u8bc4\u5206\u8005\uff0c\u91c7\u7528\u7edd\u5bf9\u4e00\u81f4\u3002",
    "Krippendorff's \u03b1 \u4f7f\u7528\u6709\u5e8f\u8ddd\u79bb\uff1b\u672a\u5b89\u88c5\u4f9d\u8d56\u65f6\u660e\u786e\u5199\u672a\u8ba1\u7b97\u3002",
    "\u8fd9\u4e9b\u4fe1\u5ea6\u4e0d\u51b3\u5b9a\u5355\u6761\u53bb\u7559\u3002\n\n"
  ))
  if (is.null(rel)) {
    cat("\u672a\u8ba1\u7b97\u8bc4\u5206\u8005\u4fe1\u5ea6\u3002\n\n")
    return(invisible(NULL))
  }
  if (isTRUE(rel$skipped)) {
    cat(rel$note %||% "\u672c\u6b21\u672a\u8ba1\u7b97\u8bc4\u5206\u8005\u4fe1\u5ea6\u3002", "\n\n")
    return(invisible(NULL))
  }
  if (!is.null(rel$error) && is.null(rel$cronbach_alpha)) {
    cat(rel$error, "\n\n")
    return(invisible(NULL))
  }
  .report_print_table(data.frame(
    Cronbach_alpha = rel$cronbach_alpha %||% NA_real_,
    ICC_2_1 = rel$icc_2_1 %||% NA_real_,
    ICC_2_k = rel$icc_2_k %||% NA_real_,
    Krippendorff_alpha = if (is.null(rel$krippendorff_alpha)) NA_real_ else rel$krippendorff_alpha,
    n_expert = rel$n_expert %||% NA_integer_,
    n_item = rel$n_item %||% NA_integer_,
    stringsAsFactors = FALSE
  ), "\u8bc4\u5206\u8005\u4fe1\u5ea6\uff08\u8865\u5145\uff09", digits = 3)
  if (!is.null(rel$krippendorff_note)) cat(rel$krippendorff_note, "\n\n")
  if (!is.null(rel$note)) cat(rel$note, "\n\n")
  invisible(NULL)
}
