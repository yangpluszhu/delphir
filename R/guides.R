#' 列出随包安装的教学文档
#'
#' 安装 delphir 后即可获得十一套从零开始的教学文档，无需另行下载。
#'
#' @return 不可见的数据框，同时打印路径。
#'
#' @examples
#' delphi_guides()
#'
#' @export
delphi_guides <- function() {
  info <- .guide_index()
  cat("delphir \u6559\u5b66\u6587\u6863\uff08\u5b89\u88c5\u540e\u81ea\u52a8\u53ef\u7528\uff09\n\n")
  for (i in seq_len(nrow(info))) {
    cat(sprintf("%s. %s\n   \u6253\u5f00: delphi_guide(\"%s\")\n   vignette(\"%s\", package = \"delphir\")\n\n",
                i, info$title[i], info$key[i], info$vignette[i]))
  }
  invisible(info)
}

#' 打开一套教学文档
#'
#' @param which `"clinical-consensus"`、`"rand-ucla"`、`"comet-cos"`、
#'   `"questionnaire-workflow"`、`"preset-protocol"`、`"three-round-workflow"`、
#'   `"standard-5point-workflow"`、`"new-item-workflow"`、
#'   `"rand-classic-workflow"`、`"rand-necessity-workflow"` 或
#'   `"rand-rules-workflow"`。
#'
#' @return 不可见的文件路径。
#'
#' @examples
#' \dontrun{
#' delphi_guide("clinical-consensus")
#' }
#'
#' @export
delphi_guide <- function(which = c("clinical-consensus", "rand-ucla", "comet-cos",
                                   "questionnaire-workflow", "preset-protocol",
                                   "three-round-workflow", "standard-5point-workflow",
                                   "new-item-workflow", "rand-classic-workflow",
                                   "rand-necessity-workflow", "rand-rules-workflow")) {
  which <- match.arg(which)
  info <- .guide_index()
  row <- info[info$key == which, ][1, ]
  html <- system.file("guides", row$file, package = "delphir")
  if (!nzchar(html) || !file.exists(html)) {
    html <- system.file("doc", row$file, package = "delphir")
  }
  if (nzchar(html) && file.exists(html)) {
    utils::browseURL(html)
    return(invisible(html))
  }
  vignette_ok <- tryCatch({
    utils::vignette(row$vignette, package = "delphir")
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(vignette_ok)) return(invisible(NULL))
  stop(
    "\u672a\u627e\u5230\u9884\u7f16\u8bd1\u6559\u5b66\u6587\u6863\u3002\u8bf7\u786e\u8ba4\u5305\u5df2\u5b89\u88c5\uff0c\u6216\u4f7f\u7528 vignette(package = \"delphir\")\u3002",
    call. = FALSE
  )
}

.guide_index <- function() {
  data.frame(
    key = c("clinical-consensus", "rand-ucla", "comet-cos",
            "questionnaire-workflow", "preset-protocol", "three-round-workflow",
            "standard-5point-workflow", "new-item-workflow",
            "rand-classic-workflow", "rand-necessity-workflow", "rand-rules-workflow"),
    vignette = c("clinical-consensus", "rand-ucla", "comet-cos",
                 "questionnaire-workflow", "preset-protocol", "three-round-workflow",
                 "standard-5point-workflow", "new-item-workflow",
                 "rand-classic-workflow", "rand-necessity-workflow", "rand-rules-workflow"),
    file = c(
      "clinical-consensus.html",
      "rand-ucla.html",
      "comet-cos.html",
      "questionnaire-workflow.html",
      "preset-protocol.html",
      "three-round-workflow.html",
      "standard-5point-workflow.html",
      "new-item-workflow.html",
      "rand-classic-workflow.html",
      "rand-necessity-workflow.html",
      "rand-rules-workflow.html"
    ),
    title = c(
      "\u6559\u5b66\u6587\u6863 1\uff1a\u533b\u5b66\u4e13\u5bb6\u5171\u8bc6 Modified Delphi \u5168\u6d41\u7a0b\uff08ACCORD \u62a5\u544a\uff09",
      "\u6559\u5b66\u6587\u6863 2\uff1aRAND/UCLA \u9002\u5b9c\u6027\u8bc4\u4ef7\u5168\u6d41\u7a0b",
      "\u6559\u5b66\u6587\u6863 3\uff1aCOMET \u6838\u5fc3\u7ed3\u5c40\u96c6 Delphi \u5168\u6d41\u7a0b",
      "\u6559\u5b66\u6587\u6863 4\uff1a\u4ece\u7b2c 1 \u8f6e\u95ee\u5377\u8bbe\u8ba1\u5230 ACCORD \u65b9\u6cd5\u5b66\u62a5\u544a",
      "\u6559\u5b66\u6587\u6863 5\uff1a\u9884\u8bbe Delphi \u65b9\u6848\u53c2\u6570\u8be6\u89e3",
      "\u6559\u5b66\u6587\u6863 6\uff1a\u4e09\u8f6e Delphi \u4ece\u96f6\u5230\u5b8c\u6574\u62a5\u544a",
      "\u6559\u5b66\u6587\u6863 7\uff1a5 \u5206\u91cf\u8868\u6838\u5fc3\u6280\u672f\u8981\u7d20\uff0c\u4ece\u5f00\u653e\u5f0f\u9996\u8f6e\u5230\u5b8c\u6574\u62a5\u544a",
      "\u6559\u5b66\u6587\u6863 8\uff1a\u7b2c\u4e00\u8f6e\u65b0\u589e\u6761\u76ee\uff0c\u4ece\u95ee\u5377\u5230\u4e09\u8f6e\u62a5\u544a",
      "\u6559\u5b66\u6587\u6863 9\uff1a\u7ecf\u5178 RAND \u4e24\u8f6e\u9002\u5b9c\u6027\uff0c\u4ece\u95ee\u5377\u5230 docx \u62a5\u544a",
      "\u6559\u5b66\u6587\u6863 10\uff1a\u5fc5\u8981\u6027\u8bc4\u4ef7\uff0c\u53ea\u8bc4\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u7684\u6761\u76ee",
      "\u6559\u5b66\u6587\u6863 11\uff1a\u89c4\u5219\u6bd4\u8f83\u4e0e\u63a8\u8350\u610f\u89c1\u4fee\u6539\u7248"
    ),
    stringsAsFactors = FALSE
  )
}
