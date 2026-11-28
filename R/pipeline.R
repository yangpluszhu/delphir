#' 指定轮次全流程分析，并按需写出完整报告
#'
#' 对最后一轮或指定轮次（含此前所有轮次）运行 [analyze_delphi()]。
#' 若 `report = TRUE`，再调用 [delphi_report()]。`format` 必须能产生 **docx**
#' （单独 docx，或 `both` 同时写 html）。保存位置由 `file` 指定。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。
#' @param round `"last"` 或正整数。整数表示分析 `ratings$round <= round`。
#' @param report 是否写出报告，默认 `FALSE`。
#' @param format `"docx"`（默认）、`"html"`、`"word"`（同 docx）或 `"both"`。
#'   当 `report = TRUE` 时，`"html"` 仍会额外写一份 docx，以满足必须含 Word 的要求。
#' @param file 报告路径。文件路径（含扩展名）或目录。`NULL` 时：交互会话可
#'   `file.choose()`；否则写入 `tempdir()` 并 `message` 路径。
#' @param open 是否打开报告，默认 `FALSE`。
#' @param ... 传给 [analyze_delphi()] 的其他参数。
#'
#' @return `delphi_result`；若写了报告，属性 `report_file` 为路径（可多个）。
#'
#' @examples
#' \donttest{
#' study <- simulate_delphi(n_experts = 10, n_items = 5, n_rounds = 3, seed = 9)
#' res <- run_delphi(study, round = 2)
#' res$last_round
#' f <- run_delphi(study, report = TRUE, format = "docx",
#'                 file = tempfile(fileext = ".docx"))
#' attr(f, "report_file")
#' }
#'
#' @export
run_delphi <- function(study, round = "last", report = FALSE,
                       format = c("docx", "html", "word", "both"),
                       file = NULL, open = FALSE, ...) {
  format <- match.arg(format)
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  .stop_if(
    identical(study$protocol$scenario, "delphi_ram"),
    "\u8fd9\u662f\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM\u3002\u8bf7\u4f7f\u7528 run_delphi_ram()\uff0c\u4e0d\u8981\u4f7f\u7528 run_delphi()\u3002"
  )
  up <- NULL
  if (!identical(round, "last")) {
    up <- as.integer(round[1])
    .stop_if(is.na(up) || up < 1L, "round \u5fc5\u987b\u662f \"last\" \u6216\u6b63\u6574\u6570\u3002")
  }
  result <- analyze_delphi(study, up_to_round = up, ...)
  if (!isTRUE(report)) return(result)

  if (identical(format, "html")) format <- "both"

  if (is.null(file)) {
    if (isTRUE(interactive())) {
      message("\u8bf7\u9009\u62e9\u62a5\u544a\u4fdd\u5b58\u4f4d\u7f6e\uff08docx\uff09\u3002")
      file <- tryCatch(file.choose(), error = function(e) NULL)
    }
    if (is.null(file) || !nzchar(file[1])) {
      file <- tempfile("delphi-report-", fileext = ".docx")
      message("\u672a\u6307\u5b9a file\uff0c\u62a5\u544a\u5199\u5165: ", file)
    }
  }
  files <- delphi_report(result, file = file, format = format, open = open)
  attr(result, "report_file") <- files
  result
}
