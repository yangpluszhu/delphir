#' 导出空白问卷与数据录入模板
#'
#' 把随包安装的第 1/2/3 轮问卷样稿和 CSV 表头拷到本地目录，便于邮件发放与录入。
#' 正式研究请优先用 [delphi_questionnaire()] 按锁定方案生成 docx/xlsx。
#'
#' @param dir 目标目录，不存在时会创建。
#' @param include_mini_example 是否同时拷贝 6 名专家 × 4 条目的填好示例，默认 `TRUE`。
#' @param overwrite 目标文件已存在时是否覆盖，默认 `FALSE`。
#'
#' @return 不可见的写出文件路径向量。
#'
#' @examples
#' \donttest{
#' d <- tempfile("delphi-survey-")
#' write_survey_templates(d)
#' }
#'
#' @export
write_survey_templates <- function(dir, include_mini_example = TRUE, overwrite = FALSE) {
  .stop_if(missing(dir) || !nzchar(dir[1]), "\u8bf7\u63d0\u4f9b\u76ee\u6807\u76ee\u5f55 dir\u3002")
  dir <- dir[1]
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  qdir <- file.path(dir, "questionnaires")
  tdir <- file.path(dir, "templates")
  dir.create(qdir, showWarnings = FALSE)
  dir.create(tdir, showWarnings = FALSE)
  written <- character()
  qsrc <- system.file("questionnaires", package = "delphir")
  tsrc <- system.file("extdata", "templates", package = "delphir")
  written <- c(written, .copy_dir_files(qsrc, qdir, overwrite))
  written <- c(written, .copy_dir_files(tsrc, tdir, overwrite))
  if (isTRUE(include_mini_example)) {
    edir <- file.path(dir, "mini_example")
    dir.create(edir, showWarnings = FALSE)
    ex <- system.file("extdata", package = "delphir")
    mini <- c(
      "survey_panel.csv", "survey_authority_raw.csv", "survey_items.csv",
      "survey_ratings.csv", "survey_comments.csv"
    )
    for (f in mini) {
      from <- file.path(ex, f)
      if (file.exists(from)) {
        to <- file.path(edir, f)
        if (!file.exists(to) || isTRUE(overwrite)) {
          file.copy(from, to, overwrite = TRUE)
          written <- c(written, to)
        }
      }
    }
  }
  invisible(written)
}

#' 按锁定方案生成 Delphi 问卷
#'
#' 第 1 轮含邀请说明、锁定的共识规则、**一次**权威程度自评和逐条 Likert + 无法判断。
#' 第 2 轮及以后使用 [prepare_feedback()] 填入本人上轮评分与组结果，**不再出现 Ca/Cs 表**。
#' 输出可为 Markdown、HTML、Word（docx）或 Excel（xlsx）；`format = "both"` 同时写
#' docx 与 xlsx。轮次可为 1 至方案中的 `max_rounds`（默认 3）。
#'
#' @param study [delphi_study()]、[delphi_blueprint()] 或 [analyze_delphi()] 结果。
#'   `NULL` 且 `sample = TRUE` 时返回随包样稿。
#' @param round 轮次，正整数。第 1 轮不需要已有评分。
#' @param expert_id 第 2 轮及以后要生成的专家编号；缺省则生成全部专家。
#' @param format `"markdown"`、`"html"`、`"docx"`、`"xlsx"` 或 `"both"`（docx+xlsx）。
#' @param file 输出路径。单个文档写文件；目录则写入 `roundN.docx` / `roundN.xlsx`。
#' @param items 第 2 轮及以后纳入 `"all"` 全部条目或 `"unresolved"`（默认，非 keep）。
#' @param sample 若 `TRUE`，忽略 `study`，直接返回随包完整样稿（markdown/html）。
#' @param split_experts 第 2 轮及以后是否按专家拆成单独文件，默认 `FALSE`。
#' @param prefill 若 `TRUE` 且 `study` 已有该轮评分，xlsx 中预填 score/unable。
#'
#' @return `format` 为 markdown/html 且未指定 `file` 时返回字符向量；否则不可见地
#'   返回写出路径。
#'
#' @examples
#' md <- delphi_questionnaire(sample = TRUE, round = 1)
#' substr(md, 1, 40)
#'
#' \donttest{
#' bp <- delphi_blueprint(
#'   preset_protocol("clinical_consensus"),
#'   data.frame(expert_id = "E01", stakeholder = "临床"),
#'   data.frame(item_id = "C01", item_text = "应报告脱落原因。"),
#'   meta = list(title = "示例")
#' )
#' f <- tempfile(fileext = ".xlsx")
#' delphi_questionnaire(bp, round = 1, format = "xlsx", file = f)
#' }
#'
#' @export
delphi_questionnaire <- function(study = NULL, round = 1,
                                 expert_id = NULL,
                                 format = c("markdown", "html", "docx", "xlsx", "both"),
                                 file = NULL,
                                 items = c("all", "unresolved"),
                                 sample = FALSE,
                                 split_experts = FALSE,
                                 prefill = FALSE) {
  format <- match.arg(format)
  items <- match.arg(items)
  round <- as.integer(round[1])
  .stop_if(is.na(round) || round < 1L, "round \u5fc5\u987b\u662f\u6b63\u6574\u6570\u3002")

  if (isTRUE(sample) || is.null(study)) {
    fname <- sprintf("round%s-complete.md", round)
    src <- system.file("questionnaires", fname, package = "delphir")
    if (!nzchar(src) || !file.exists(src)) {
      fname <- if (round == 1L) "round1-complete.md" else "round2-complete.md"
      src <- system.file("questionnaires", fname, package = "delphir")
    }
    .stop_if(!nzchar(src) || !file.exists(src), "\u672a\u627e\u5230\u968f\u5305\u95ee\u5377\u6837\u7a3f\u3002")
    md <- paste(readLines(src, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    if (format %in% c("markdown", "html") && is.null(file)) {
      return(if (format == "html") .md_to_html(md, title = fname) else md)
    }
    docs <- md
    names(docs) <- sprintf("round%s", round)
    if (format %in% c("xlsx", "docx", "both") && is.null(study)) {
      study <- .mini_example_study()
    }
    if (is.null(study)) {
      out <- if (format == "html") .md_to_html(md, title = fname) else md
      return(.write_q_file(out, file, format))
    }
    return(.write_questionnaires(docs, study, round, format, file, items,
                                 expert_id, NULL, split_experts, prefill))
  }

  result <- NULL
  if (inherits(study, "delphi_result")) {
    result <- study
    study <- study$study
  } else {
    study <- .match_study(study)
  }
  maxr <- study$protocol$stability$max_rounds %||% 3L
  .stop_if(round > maxr, sprintf("round=%s \u8d85\u8fc7\u65b9\u6848 max_rounds=%s\u3002", round, maxr))

  if (round == 1L) {
    md <- .questionnaire_round1(study)
    docs <- md
    names(docs) <- "round1"
    if (format %in% c("markdown", "html") && is.null(file)) {
      return(if (format == "html") .md_to_html(md, study$meta$title %||% "\u7b2c 1 \u8f6e Delphi \u95ee\u5377") else md)
    }
    return(.write_questionnaires(docs, study, round, format, file, items,
                                 expert_id, result, FALSE, prefill))
  }

  .stop_if(is.null(study$ratings) || nrow(study$ratings) == 0L,
           "\u751f\u6210\u7b2c 2 \u8f6e\u53ca\u4ee5\u540e\u95ee\u5377\u9700\u8981\u5df2\u6709\u4e0a\u4e00\u8f6e\u8bc4\u5206\u3002")
  src_round <- round - 1L
  have <- unique(study$ratings$round)
  .stop_if(!src_round %in% have, sprintf("\u7f3a\u5c11\u7b2c %s \u8f6e\u8bc4\u5206\uff0c\u65e0\u6cd5\u751f\u6210\u7b2c %s \u8f6e\u53cd\u9988\u3002", src_round, round))

  fb <- prepare_feedback(if (!is.null(result)) result else study, round = src_round)
  ids <- if (is.null(expert_id)) unique(as.character(fb$expert_id)) else as.character(expert_id)
  missing <- setdiff(ids, unique(as.character(fb$expert_id)))
  .stop_if(length(missing) > 0L, sprintf("\u53cd\u9988\u8868\u4e2d\u6ca1\u6709\u4e13\u5bb6: %s", paste(missing, collapse = ", ")))
  cls <- NULL
  if (!is.null(result)) cls <- result$classification
  else {
    s <- summarize_items(study, rounds = src_round)
    if (identical(study$protocol$scenario, "rand_ucla")) {
      cls <- classify_rand(study, round = src_round, summary = s)
    } else {
      cls <- classify_consensus(s, study$protocol)
    }
  }
  docs <- vapply(ids, function(eid) {
    .questionnaire_followup(study, fb[fb$expert_id == eid, , drop = FALSE], cls, items, round)
  }, character(1))
  names(docs) <- ids
  if (format %in% c("markdown", "html") && is.null(file)) {
    if (format == "html") {
      docs <- vapply(seq_along(docs), function(i) {
        .md_to_html(docs[[i]], sprintf("\u7b2c %s \u8f6e\u53cd\u9988\u5355 \u00b7 %s", round, ids[i]))
      }, character(1))
      names(docs) <- ids
    }
    return(docs)
  }
  .write_questionnaires(docs, study, round, format, file, items,
                        expert_id, result, split_experts, prefill)
}

#' 根据本轮分析结果生成下一轮问卷
#'
#' 等价于 `delphi_questionnaire(result, round = last_round + 1, ...)`，
#' 但会检查是否已达到方案中的 `max_rounds`。
#'
#' @param result [analyze_delphi()] 结果。
#' @param format 见 [delphi_questionnaire()]，默认 `"both"`（docx + xlsx）。
#' @param file 输出路径或目录。
#' @param items 默认 `"unresolved"`。
#' @param ... 传给 [delphi_questionnaire()] 的其他参数。
#'
#' @return 见 [delphi_questionnaire()]。
#'
#' @examples
#' \donttest{
#' study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 4)
#' res <- analyze_delphi(study)
#' d <- tempfile("r2-")
#' next_round_questionnaire(res, format = "xlsx", file = d)
#' }
#'
#' @export
next_round_questionnaire <- function(result, format = "both", file = NULL,
                                     items = "unresolved", ...) {
  .stop_if(!inherits(result, "delphi_result"), "\u8bf7\u63d0\u4f9b analyze_delphi() \u7ed3\u679c\u3002")
  nxt <- as.integer(result$last_round) + 1L
  maxr <- result$study$protocol$stability$max_rounds %||% 3L
  .stop_if(
    nxt > maxr,
    sprintf("\u5df2\u8fbe\u65b9\u6848\u89c4\u5b9a\u7684\u6700\u591a\u8f6e\u6b21 %s\uff0c\u4e0d\u518d\u751f\u6210\u4e0b\u4e00\u8f6e\u95ee\u5377\u3002", maxr)
  )
  delphi_questionnaire(
    study = result,
    round = nxt,
    format = format,
    file = file,
    items = items,
    ...
  )
}

.mini_example_study <- function() {
  ex <- system.file("extdata", package = "delphir")
  panel_f <- file.path(ex, "survey_panel.csv")
  if (!file.exists(panel_f)) {
    return(simulate_delphi(n_experts = 6, n_items = 4, n_rounds = 1, seed = 1))
  }
  panel <- utils::read.csv(panel_f, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  items <- utils::read.csv(file.path(ex, "survey_items.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  ratings <- utils::read.csv(file.path(ex, "survey_ratings.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  raw_f <- file.path(ex, "survey_authority_raw.csv")
  if (file.exists(raw_f)) {
    raw <- utils::read.csv(raw_f, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    scored <- tryCatch(score_authority(raw), error = function(e) NULL)
    if (!is.null(scored)) {
      panel$Ca <- scored$Ca[match(panel$expert_id, scored$expert_id)]
      panel$Cs <- scored$Cs[match(panel$expert_id, scored$expert_id)]
    }
  }
  suppressWarnings(delphi_study(
    protocol = preset_protocol("clinical_consensus"),
    panel = panel,
    items = items,
    ratings = ratings,
    meta = list(title = "\u8ff7\u4f60\u793a\u4f8b", objective = "\u6559\u5b66",
                users = "\u7814\u7a76\u8005", geography = "\u4e2d\u56fd\u5927\u9646"),
    validate = FALSE
  ))
}

.copy_dir_files <- function(from, to, overwrite) {
  if (!nzchar(from) || !dir.exists(from)) return(character())
  fs <- list.files(from, full.names = TRUE)
  out <- character()
  for (f in fs) {
    dest <- file.path(to, basename(f))
    if (!file.exists(dest) || isTRUE(overwrite)) {
      file.copy(f, dest, overwrite = TRUE)
      out <- c(out, dest)
    }
  }
  out
}

.write_q_file <- function(docs, file, format) {
  if (is.null(file)) return(docs)
  ext <- if (identical(format, "html")) ".html" else ".md"
  if (length(docs) == 1L && !dir.exists(file) && !grepl("/|\\\\$", file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(docs, file, useBytes = FALSE)
    return(invisible(docs))
  }
  if (!dir.exists(file)) dir.create(file, recursive = TRUE, showWarnings = FALSE)
  nms <- names(docs)
  if (is.null(nms)) nms <- paste0("questionnaire", seq_along(docs))
  for (i in seq_along(docs)) {
    writeLines(docs[[i]], file.path(file, paste0(nms[i], ext)), useBytes = FALSE)
  }
  invisible(docs)
}

.protocol_rules_md <- function(p) {
  r <- p$rules
  pct <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x)) return("\uff08\u672c\u573a\u666f\u4e0d\u9002\u7528\uff09")
    sprintf("%.0f%%", 100 * x)
  }
  num <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x)) return("\uff08\u672c\u573a\u666f\u4e0d\u9002\u7528\uff09")
    as.character(x)
  }
  paste(
    sprintf("- **\u9ad8\u5ea6\u5171\u8bc6\uff1a** \u8bc4\u5206 %s\u2013%s \u7684\u6bd4\u4f8b \u2265 %s\uff0c\u4e14 Median \u2265 %s\uff0c\u4e14 IQR \u2264 %s",
            p$high_range[1], p$high_range[2], pct(r$high_pct), num(r$median_min), num(r$iqr_max)),
    sprintf("- **\u57fa\u672c\u5171\u8bc6\uff1a** %s \u2264 \u9ad8\u533a\u6bd4\u4f8b < %s\uff0c\u4e14 Median \u2265 %s",
            pct(r$basic_pct), pct(r$high_pct), num(r$median_min)),
    sprintf("- **\u503e\u5411\u6392\u9664\uff1a** \u8bc4\u5206 %s\u2013%s \u7684\u6bd4\u4f8b \u2265 %s",
            p$low_range[1], p$low_range[2], pct(r$reject_pct)),
    sprintf("- **\u4e24\u6781\u5206\u5316\uff1a** \u9ad8\u533a\u4e0e\u4f4e\u533a\u540c\u65f6 \u2265 %s",
            pct(r$polarization_threshold)),
    sprintf("- **\u6700\u591a\u8f6e\u6b21\uff1a** %s\uff1b\u76f8\u90bb\u8f6e\u5206\u7c7b\u4e0d\u53d8 \u2265 %s\uff1bMedian \u53d8\u5316 \u2264 %s\uff1b\u9ad8\u533a\u6bd4\u4f8b\u53d8\u5316 < %s",
            p$stability$max_rounds, pct(p$stability$class_pct),
            num(p$stability$median_delta), pct(p$stability$high_pct_delta)),
    sep = "\n"
  )
}

.likert_line <- function(p, label) {
  nums <- paste(seq(p$scale_min, p$scale_max), collapse = "  ")
  sprintf("**%s\uff08%s\u2013%s\uff09\uff1a** %s\u3000\u3000\u25a1 \u65e0\u6cd5\u5224\u65ad / \u8d85\u51fa\u672c\u4eba\u4e13\u4e1a\u9886\u57df",
          label, p$scale_min, p$scale_max, nums)
}

.questionnaire_round1 <- function(study) {
  p <- study$protocol
  meta <- study$meta
  title <- meta$title %||% "Modified Delphi \u4e13\u5bb6\u54a8\u8be2"
  sc <- authority_scales()
  cs_tab <- paste0(
    "| \u8bf7\u9009 | \u719f\u6089\u7a0b\u5ea6 | \u8d4b\u5206 Cs |\n|---|---|---|\n",
    paste(sprintf("| \u25a1 | %s | %.1f |", names(sc$cs), unname(sc$cs)), collapse = "\n")
  )
  ca <- sc$ca
  ca_tab <- paste0(
    "| \u5224\u65ad\u4f9d\u636e | \u5927 | \u4e2d | \u5c0f |\n|---|---|---|---|\n",
    paste(sprintf("| %s\uff08%.1f / %.1f / %.1f\uff09 | \u25a1 | \u25a1 | \u25a1 |",
                  ca$source, ca$large, ca$medium, ca$small), collapse = "\n")
  )
  item_blocks <- apply(study$items, 1, function(row) {
    dims <- .questionnaire_dimensions(study, 1L)
    likert <- paste(vapply(dims, function(d) .likert_line(p, d), character(1)), collapse = "\n")
    paste0(
      "### \u6761\u76ee ", row[["item_id"]], "\n\n",
      "**", row[["item_text"]], "**\n\n",
      if (!is.na(row[["domain"]]) && nzchar(row[["domain"]])) paste0("- \u9886\u57df\uff1a", row[["domain"]], "\n") else "",
      if ("evidence_level" %in% names(row) && !is.na(row[["evidence_level"]]))
        paste0("- \u6761\u76ee\u6765\u6e90 / \u8bc1\u636e\u51c6\u5907\uff1a", row[["evidence_level"]], "\n") else "",
      "\n", likert, "\n",
      "**\u4fee\u6539\u610f\u89c1\uff1a** ________________________________________________\n"
    )
  })
  paste0(
    "# \u7b2c 1 \u8f6e Delphi \u4e13\u5bb6\u54a8\u8be2\u95ee\u5377\n\n",
    "**\u7814\u7a76\u9898\u76ee\uff1a** ", title, "\n\n",
    "**\u76ee\u7684\uff1a** ", meta$objective %||% "\uff08\u8bf7\u5728 study$meta$objective \u4e2d\u8865\u5145\uff09", "\n\n",
    "**\u76ee\u6807\u4f7f\u7528\u8005\uff1a** ", meta$users %||% "\u672a\u8bf4\u660e", "\n\n",
    "**\u5730\u7406\u8303\u56f4\uff1a** ", meta$geography %||% "\u672a\u8bf4\u660e", "\n\n",
    "**\u533f\u540d\uff1a** ", meta$anonymity %||% "\u8bc4\u5206\u533f\u540d\u6c47\u603b", "\n\n",
    "> \u8bc4\u5206\u533f\u540d\u6c47\u603b\u3002Ca/Cs \u53ea\u8bc4\u4e00\u6b21\uff0c\u4e0d\u6309\u6761\u76ee\u91cd\u590d\uff1b\u6761\u76ee\u4e0d\u719f\u8bf7\u52fe\u9009\u300c\u65e0\u6cd5\u5224\u65ad\u300d\uff0c\u52ff\u586b 5 \u5206\u3002\n\n",
    "## \u5df2\u9501\u5b9a\u7684\u5171\u8bc6\u89c4\u5219\n\n",
    .protocol_rules_md(p), "\n\n",
    "**\u4e0d\u662f\u6240\u6709\u6761\u76ee\u90fd\u5fc5\u987b\u8fbe\u6210\u5171\u8bc6\u3002** \u4e0a\u8ff0\u89c4\u5219\u4e0d\u4f1a\u6839\u636e\u7ed3\u679c\u56de\u6539\u3002\n\n",
    "## \u4e13\u5bb6\u80cc\u666f\n\n",
    "\u8bf7\u586b\u5199\u5229\u76ca\u76f8\u5173\u8005\u7c7b\u578b\u3001\u4e13\u4e1a\u3001\u5730\u533a\u3001\u804c\u79f0\u4e0e\u5e74\u9650\uff08\u4e0e panel.csv \u5bf9\u9f50\uff09\u3002\n\n",
    "## \u6743\u5a01\u7a0b\u5ea6\u81ea\u8bc4\uff08\u5bf9\u672c\u6b21\u4e3b\u9898\u6574\u4f53\u53ea\u8bc4\u4e00\u6b21\uff09\n\n",
    "### \u719f\u6089\u7a0b\u5ea6 Cs\n\n", cs_tab, "\n\n",
    "### \u5224\u65ad\u4f9d\u636e Ca\n\n", ca_tab, "\n\n",
    "> Cr = (Ca + Cs) / 2\u3002\u4e0d\u7528\u4e8e\u52a0\u6743 1\u20139 \u5206\uff1b\u7b2c\u4e8c\u8f6e\u4e0d\u5fc5\u91cd\u8bc4\u3002\n\n",
    "## \u6761\u76ee\u8bc4\u5206\n\n",
    paste(item_blocks, collapse = "\n"),
    "\n## \u65b0\u589e\u6761\u76ee\uff08\u53ef\u9009\uff09\n\n",
    "\u5efa\u8bae\u8868\u8ff0 / \u7406\u7531 / \u60a8\u8bc4\u7684\u91cd\u8981\u6027\uff1a\n\n",
    "________________________________________________________________\n"
  )
}

.questionnaire_round2 <- function(study, fb_one, cls, items) {
  .questionnaire_followup(study, fb_one, cls, items, round = 2L)
}

.questionnaire_followup <- function(study, fb_one, cls, items, round) {
  eid <- fb_one$expert_id[1]
  title <- study$meta$title %||% "Modified Delphi"
  src_round <- as.integer(round) - 1L
  dim0 <- fb_one$dimension[1]
  if (identical(items, "unresolved")) {
    keep_ids <- .unresolved_item_ids(cls, src_round, items, dim0)
    fb_use <- fb_one[fb_one$item_id %in% keep_ids, , drop = FALSE]
    fb_skip <- fb_one[!fb_one$item_id %in% keep_ids, , drop = FALSE]
  } else {
    fb_use <- fb_one
    fb_skip <- fb_one[0, ]
  }
  dim_label <- .primary_dimension(study, round)
  block <- function(d) {
    if (!nrow(d)) return("")
    paste(apply(d, 1, function(row) {
      paste0(
        "### \u6761\u76ee ", row[["item_id"]], "\n\n",
        "**", row[["item_text"]], "**\n\n",
        "| \u60a8\u7684\u4e0a\u8f6e\u8bc4\u5206 | \u7ec4 Median\uff08IQR\uff09 | \u9ad8\u533a | \u4f4e\u533a | \u5206\u7c7b |\n",
        "|---|---|---|---|---|\n",
        sprintf("| %s | %s | %s%% | %s%% | %s |\n\n",
                row[["own_score"]], row[["median_iqr"]],
                round(as.numeric(row[["p_high"]]), 1),
                round(as.numeric(row[["p_low"]]), 1),
                row[["class_zh"]]),
        if (!is.na(row[["anonymous_comments"]]) && nzchar(row[["anonymous_comments"]]))
          paste0("**\u533f\u540d\u610f\u89c1\u6458\u8981\uff1a** ", row[["anonymous_comments"]], "\n\n") else "",
        .likert_line(study$protocol, paste0("\u672c\u8f6e", dim_label)), "\n",
        "**\u672c\u8f6e\u610f\u89c1\uff08\u53ef\u7a7a\uff09\uff1a** ________________________________________________\n"
      )
    }), collapse = "\n")
  }
  skip_tab <- ""
  if (nrow(fb_skip)) {
    skip_tab <- paste0(
      sprintf("## \u7b2c %s \u8f6e\u5df2\u8fbe\u9ad8\u5ea6/\u57fa\u672c\u5171\u8bc6\u3001\u672c\u8f6e\u53ef\u4e0d\u91cd\u8bc4\u7684\u6761\u76ee\n\n", src_round),
      "| \u6761\u76ee | \u539f\u6587 | Median\uff08IQR\uff09 | \u9ad8\u533a | \u5206\u7c7b |\n|---|---|---|---|---|\n",
      paste(sprintf("| %s | %s | %s | %s%% | %s |",
                    fb_skip$item_id,
                    substr(fb_skip$item_text, 1, 36),
                    fb_skip$median_iqr,
                    round(fb_skip$p_high, 1),
                    fb_skip$class_zh), collapse = "\n"),
      "\n\n"
    )
  }
  paste0(
    sprintf("# \u7b2c %s \u8f6e Delphi \u4e13\u5bb6\u54a8\u8be2\u95ee\u5377\n\n", round),
    "**\u7814\u7a76\u9898\u76ee\uff1a** ", title, "\n\n",
    "**\u672c\u5377\u9002\u7528\u4e13\u5bb6\u7f16\u53f7\uff1a** ", eid, "\n\n",
    sprintf("\u7b2c %s \u8f6e**\u4e0d\u518d**\u586b\u5199\u719f\u6089\u7a0b\u5ea6\u4e0e\u5224\u65ad\u4f9d\u636e\uff08Ca\u3001Cs\uff09\u3002", round),
    "\u8bf7\u5148\u9605\u8bfb\u7ec4\u7ed3\u679c\u4e0e\u60a8\u7684\u4e0a\u8f6e\u8bc4\u5206\uff0c\u518d\u7ed9\u51fa\u672c\u8f6e\u5206\u6570\u3002\u5171\u8bc6\u89c4\u5219\u4e0e\u7b2c 1 \u8f6e\u76f8\u540c\u3002\n\n",
    "## \u9700\u8981\u672c\u8f6e\u91cd\u8bc4\u7684\u6761\u76ee\n\n",
    block(fb_use), "\n",
    skip_tab
  )
}

.md_to_html <- function(md, title = "Delphi") {
  lines <- unlist(strsplit(md, "\n", fixed = TRUE))
  html <- character()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    ln <- lines[i]
    if (grepl("^\\|", ln)) {
      rows <- character()
      while (i <= n && grepl("^\\|", lines[i])) {
        rows <- c(rows, lines[i])
        i <- i + 1L
      }
      html <- c(html, .md_table_html(rows))
      next
    }
    if (grepl("^### ", ln)) {
      html <- c(html, paste0("<h3>", .html_esc(sub("^### ", "", ln)), "</h3>"))
    } else if (grepl("^## ", ln)) {
      html <- c(html, paste0("<h2>", .html_esc(sub("^## ", "", ln)), "</h2>"))
    } else if (grepl("^# ", ln)) {
      html <- c(html, paste0("<h1>", .html_esc(sub("^# ", "", ln)), "</h1>"))
    } else if (grepl("^---+$", ln) || grepl("^\\*\\*\\*+$", ln)) {
      html <- c(html, "<hr/>")
    } else if (!nzchar(trimws(ln))) {
      html <- c(html, "")
    } else {
      html <- c(html, paste0("<p>", .inline_md(.html_esc(ln)), "</p>"))
    }
    i <- i + 1L
  }
  paste0(
    "<!DOCTYPE html><html lang='zh-CN'><head><meta charset='utf-8'/>",
    "<title>", .html_esc(title), "</title><style>",
    "body{font-family:Segoe UI,'Source Han Sans SC',sans-serif;max-width:880px;",
    "margin:2rem auto;line-height:1.65;color:#1a202c;padding:0 1rem;}",
    "table{border-collapse:collapse;width:100%;margin:1rem 0;font-size:0.95rem;}",
    "th,td{border:1px solid #cbd5e0;padding:6px 8px;vertical-align:top;}",
    "th{background:#edf2f7;} h1,h2,h3{color:#1a365d;} blockquote{border-left:4px solid #3182ce;",
    "margin:1rem 0;padding:0.2rem 1rem;background:#ebf8ff;}</style></head><body>\n",
    paste(html, collapse = "\n"),
    "\n</body></html>"
  )
}

.md_table_html <- function(rows) {
  rows <- rows[!grepl("^\\|\\s*[-:]+", rows)]
  parse_row <- function(r) {
    cells <- strsplit(gsub("^\\||\\|$", "", r), "\\|", perl = TRUE)[[1]]
    trimws(cells)
  }
  if (!length(rows)) return("")
  hdr <- parse_row(rows[1])
  body <- if (length(rows) > 1L) rows[-1] else character()
  th <- paste0("<th>", vapply(hdr, .html_esc, character(1)), "</th>", collapse = "")
  trs <- vapply(body, function(r) {
    paste0("<tr>", paste0("<td>", vapply(parse_row(r), function(z) .inline_md(.html_esc(z)), character(1)), "</td>", collapse = ""), "</tr>")
  }, character(1))
  paste0("<table><thead><tr>", th, "</tr></thead><tbody>", paste(trs, collapse = ""), "</tbody></table>")
}

.html_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

.inline_md <- function(x) {
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x)
  x <- gsub("`([^`]+)`", "<code>\\1</code>", x)
  if (startsWith(trimws(x), "&gt; ") || startsWith(trimws(x), "&gt;")) {
    x <- paste0("<blockquote>", sub("^\\s*&gt;\\s?", "", x), "</blockquote>")
  }
  x
}
