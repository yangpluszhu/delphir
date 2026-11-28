#' 撰写 ACCORD / RAND / COMET 风格分析报告
#'
#' 将 [analyze_delphi()] 结果渲染为 HTML 或 Word。模板会完整打印预设方案、
#' 各轮问卷结构、逐轮结果、终结果与方法学参考文献，并附 ACCORD、DCAT、CREDES 清单。
#'
#' @param result [analyze_delphi()] 结果。
#' @param file 输出路径。若为空，写入临时文件。若为目录，使用默认文件名。
#'   `format = "both"` 时在同一目录写出 `.html` 与 `.docx`。
#' @param format `"html"`、`"word"` / `"docx"` 或 `"both"`。
#' @param template `"auto"` 时按场景选择；也可指定 `"clinical"`、
#'   `"rand_ucla"`、`"comet_cos"` 或 `"standard"`（5 分核心技术要素）。
#' @param stage `"auto"`、`"round1"`、`"round2"` 或 `"final"`。
#'   `auto`：若当前轮次仍小于方案 `max_rounds` 则写中间报告，否则写终稿。
#' @param open 是否打开文件，默认 `FALSE`。
#'
#' @return 不可见的输出文件路径（`both` 时为长度为 2 的命名向量）。
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
                          format = c("html", "word", "docx", "both"),
                          template = c("auto", "clinical", "rand_ucla", "comet_cos", "standard"),
                          stage = c("auto", "round1", "round2", "final"),
                          open = FALSE) {
  .stop_if(!inherits(result, "delphi_result"), "\u8bf7\u5148\u7528 analyze_delphi() \u5f97\u5230\u7ed3\u679c\u3002")
  format <- match.arg(format)
  if (identical(format, "docx")) format <- "word"
  template <- match.arg(template)
  stage <- .match_report_stage(stage)
  stage <- .resolve_report_stage(result, stage)
  if (template == "auto") {
    template <- switch(
      result$study$protocol$scenario,
      rand_ucla = "rand_ucla",
      comet_cos = "comet_cos",
      standard_element = "standard",
      "clinical"
    )
  }
  if (identical(format, "both")) {
    base <- .report_stem(file, result)
    html <- delphi_report(result, file = paste0(base, ".html"), format = "html",
                          template = template, stage = stage, open = FALSE)
    docx <- delphi_report(result, file = paste0(base, ".docx"), format = "word",
                          template = template, stage = stage, open = open)
    out <- c(html = html, docx = docx)
    return(invisible(out))
  }
  tpl_dir <- switch(
    template,
    clinical = "delphi-clinical",
    rand_ucla = "delphi-rand-ucla",
    comet_cos = "delphi-comet-cos",
    standard = "delphi-standard"
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
  if (is.null(file) || (length(file) && dir.exists(file[1]))) {
    ext <- if (format == "html") ".html" else ".docx"
    stem <- if (is.null(file)) tempfile("delphi-report-") else file.path(file[1], "delphi-report")
    file <- paste0(stem, ext)
  } else if (dir.exists(dirname(file)) && !grepl("\\.(html|docx)$", file, ignore.case = TRUE)) {
    ext <- if (format == "html") ".html" else ".docx"
    file <- paste0(file, ext)
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

.match_report_stage <- function(stage) {
  allowed <- c("auto", "round1", "round2", "final")
  if (length(stage) != 1L) return(match.arg(stage, allowed))
  if (stage %in% allowed) return(stage)
  if (grepl("^round[0-9]+$", stage)) return(stage)
  match.arg(stage, allowed)
}

.report_stem <- function(file, result) {
  if (is.null(file) || !nzchar(file[1])) {
    return(tempfile("delphi-report-"))
  }
  file <- file[1]
  if (dir.exists(file)) return(file.path(file, "delphi-report"))
  sub("\\.(html|docx)$", "", file, ignore.case = TRUE)
}

.resolve_report_stage <- function(result, stage) {
  if (!identical(stage, "auto")) return(stage)
  rnd <- result$last_round %||% max(result$item_summary$round)
  maxr <- result$study$protocol$stability$max_rounds %||% 3L
  if (isTRUE(rnd <= 1)) return("round1")
  if (isTRUE(rnd < maxr)) return(sprintf("round%s", rnd))
  "final"
}

#' @keywords internal
#' @noRd
.delphi_references <- function() {
  data.frame(
    id = seq_len(13),
    citation = c(
      "Hasson F, Keeney S, McKenna H. Research guidelines for the Delphi survey technique. J Adv Nurs. 2000;32(4):1008-1015.",
      "Diamond IR, Grant RC, Feldman BM, et al. Defining consensus: a systematic review recommends methodologic criteria for reporting of Delphi studies. J Clin Epidemiol. 2014;67(4):401-409.",
      "Junger S, Payne SA, Brine J, Radbruch L, Brearley SG. Guidance on Conducting and REporting DElphi Studies (CREDES) in palliative care. Palliat Med. 2017;31(8):684-706.",
      "Gattrell WT, Logullo P, van Zuuren EJ, et al. ACCORD (ACcurate COnsensus Reporting Document): a reporting guideline for consensus methods in biomedicine developed via a modified Delphi. PLoS Med. 2024;21(1):e1004326.",
      "Manyara AM, Purvis A, Ciani O, et al. Sample size in Delphi studies: a systematic review and proposal of a reporting guideline (DCAT). BMJ. 2025;388:e082006.",
      "Fitch K, Bernstein SJ, Aguilar MD, et al. The RAND/UCLA Appropriateness Method User's Manual. Santa Monica: RAND; 2001.",
      "Williamson PR, Altman DG, Bagley H, et al. The COMET Handbook: version 1.0. Trials. 2017;18(Suppl 3):280.",
      "Kirkham JJ, Davis K, Altman DG, et al. Core Outcome Set-STAndards for Development: the COS-STAD recommendations. PLoS Med. 2017;14(11):e1002447.",
      "Humphrey-Murto S, Varpio L, Wood TJ, et al. The use of the Delphi and other consensus group methods in medical education research. Acad Med. 2017;92(11):1491-1498.",
      "Boulkedid R, Abdoul H, Loustau M, Sibony O, Alberti C. Using and reporting the Delphi method for selecting healthcare quality indicators. PLoS One. 2011;6(6):e20476.",
      "Zeng G. Modern Epidemiological Methods and Application. Beijing: Beijing Medical University Press; 1994. (曾光. 现代流行病学方法与应用. 权威系数赋分表)",
      "Niederberger M, Spranger J. Delphi technique in health sciences: a map. Front Public Health. 2020;8:457.",
      sprintf("Yang M. delphir: Structured Delphi and RAND/UCLA Analysis for Consensus Studies. R package version %s. https://github.com/yangpluszhu/delphir",
              as.character(.pkg_version()))
    ),
    stringsAsFactors = FALSE
  )
}

.questionnaire_round_outline <- function(result, round) {
  study <- result$study
  items <- study$items
  cls <- result$classification
  sub <- if ("round" %in% names(cls)) cls[cls$round == round, , drop = FALSE] else cls
  dim0 <- unique(sub$dimension)
  prefer <- intersect(c("\u91cd\u8981\u7a0b\u5ea6", "\u91cd\u8981\u6027", "overall", "\u9002\u5b9c\u6027"), dim0)
  if (length(prefer)) sub <- sub[sub$dimension == prefer[1], , drop = FALSE]
  list(
    round = round,
    n_items = nrow(items),
    item_ids = as.character(items$item_id),
    classification = sub,
    includes_authority = .is_first_scoring_round(study$protocol, round),
    dimensions = .questionnaire_dimensions(study, round)
  )
}
