#' 撰写 ACCORD / RAND / COMET 风格分析报告
#'
#' 将 [analyze_delphi()] 结果渲染为 HTML 或 Word。模板会分列证据与共识，
#' 并附 ACCORD、DCAT 清单。
#'
#' @param result [analyze_delphi()] 结果。
#' @param file 输出路径。若为空，写入临时文件。
#' @param format `"html"` 或 `"word"`。
#' @param template `"auto"` 时按场景选择；也可指定 `"clinical"`、
#'   `"rand_ucla"`、`"comet_cos"`。
#' @param stage `"auto"`、`"round1"` 或 `"final"`。仅一轮或显式 `round1`
#'   时输出中间报告（非最终共识）；两轮及以上默认终稿。
#' @param open 是否打开文件，默认 `FALSE`。
#'
#' @return 不可见的输出文件路径。
#'
#' @examples
#' \donttest{
#' study <- simulate_delphi(n_experts = 12, n_items = 6, seed = 23)
#' res <- analyze_delphi(study)
#' f <- delphi_report(res, file = tempfile(fileext = ".html"))
#' }
#'
#' @export
delphi_report <- function(result, file = NULL,
                          format = c("html", "word"),
                          template = c("auto", "clinical", "rand_ucla", "comet_cos"),
                          stage = c("auto", "round1", "final"),
                          open = FALSE) {
  .stop_if(!inherits(result, "delphi_result"), "\u8bf7\u5148\u7528 analyze_delphi() \u5f97\u5230\u7ed3\u679c\u3002")
  format <- match.arg(format)
  template <- match.arg(template)
  stage <- match.arg(stage)
  stage <- .resolve_report_stage(result, stage)
  if (template == "auto") {
    template <- switch(
      result$study$protocol$scenario,
      rand_ucla = "rand_ucla",
      comet_cos = "comet_cos",
      "clinical"
    )
  }
  tpl_dir <- switch(
    template,
    clinical = "delphi-clinical",
    rand_ucla = "delphi-rand-ucla",
    comet_cos = "delphi-comet-cos"
  )
  skeleton <- system.file(
    "rmarkdown", "templates", tpl_dir, "skeleton", "skeleton.Rmd",
    package = "delphir"
  )
  if (!nzchar(skeleton) || !file.exists(skeleton)) {
    skeleton <- system.file("rmarkdown", "templates", "delphi-clinical",
                            "skeleton", "skeleton.Rmd", package = "delphir")
  }
  .stop_if(!nzchar(skeleton), "\u627e\u4e0d\u5230\u62a5\u544a\u6a21\u677f\uff0c\u8bf7\u786e\u8ba4\u5305\u5df2\u6b63\u786e\u5b89\u88c5\u3002")
  if (is.null(file)) {
    ext <- if (format == "html") ".html" else ".docx"
    file <- tempfile("delphi-report-", fileext = ext)
  }
  file <- suppressWarnings(normalizePath(file, winslash = "/", mustWork = FALSE))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  out_fmt <- if (format == "html") {
    rmarkdown::html_document(
      toc = TRUE, toc_float = TRUE, theme = "flatly",
      number_sections = TRUE
    )
  } else {
    rmarkdown::word_document(toc = TRUE)
  }
  work <- tempfile("delphi-rmd-")
  dir.create(work)
  rmd <- file.path(work, "report.Rmd")
  file.copy(skeleton, rmd, overwrite = TRUE)
  old_dup <- getOption("knitr.duplicate.label")
  on.exit(options(knitr.duplicate.label = old_dup), add = TRUE)
  options(knitr.duplicate.label = "allow")
  rendered <- rmarkdown::render(
    rmd,
    output_format = out_fmt,
    output_file = basename(file),
    output_dir = dirname(file),
    params = list(result = result, template = template, stage = stage),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  outfile <- if (length(rendered) && file.exists(rendered)) rendered else file
  if (isTRUE(open) && file.exists(outfile)) {
    utils::browseURL(outfile)
  }
  invisible(outfile)
}

.resolve_report_stage <- function(result, stage) {
  if (!identical(stage, "auto")) return(stage)
  rnd <- result$last_round %||% max(result$item_summary$round)
  if (isTRUE(rnd <= 1)) "round1" else "final"
}
