#' 列出随包安装的教学文档
#'
#' 安装 delphir 后即可获得三套从零开始的教学文档，无需另行下载。
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
#' @param which `"clinical-consensus"`、`"rand-ucla"` 或 `"comet-cos"`。
#'
#' @return 不可见的文件路径。
#'
#' @examples
#' \dontrun{
#' delphi_guide("clinical-consensus")
#' }
#'
#' @export
delphi_guide <- function(which = c("clinical-consensus", "rand-ucla", "comet-cos")) {
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
    key = c("clinical-consensus", "rand-ucla", "comet-cos"),
    vignette = c("clinical-consensus", "rand-ucla", "comet-cos"),
    file = c(
      "clinical-consensus.html",
      "rand-ucla.html",
      "comet-cos.html"
    ),
    title = c(
      "\u6559\u5b66\u6587\u6863 1\uff1a\u533b\u5b66\u4e13\u5bb6\u5171\u8bc6 Modified Delphi \u5168\u6d41\u7a0b\uff08ACCORD \u62a5\u544a\uff09",
      "\u6559\u5b66\u6587\u6863 2\uff1aRAND/UCLA \u9002\u5b9c\u6027\u8bc4\u4ef7\u5168\u6d41\u7a0b",
      "\u6559\u5b66\u6587\u6863 3\uff1aCOMET \u6838\u5fc3\u7ed3\u5c40\u96c6 Delphi \u5168\u6d41\u7a0b"
    ),
    stringsAsFactors = FALSE
  )
}
