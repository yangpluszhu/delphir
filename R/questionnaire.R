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
#' @param items 第 2 轮及以后纳入 `"all"`、`"unresolved"`（默认，已完成纳入或排除的不再重评）
#'   或 `"iteration"`（新条目、未共识条目、实质性修改）。首次进入的条目总会列入，
#'   并标明没有上轮统计。
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
                                 items = c("all", "unresolved", "iteration"),
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
  .stop_if(
    identical(study$protocol$scenario, "delphi_ram"),
    "\u573a\u666f delphi_ram \u8bf7\u4f7f\u7528 delphi_ram_questionnaire()\uff0c\u4e0d\u8981\u4f7f\u7528 delphi_questionnaire()\u3002"
  )
  maxr <- study$protocol$stability$max_rounds %||% 3L
  .stop_if(round > maxr, sprintf("round=%s \u8d85\u8fc7\u65b9\u6848 max_rounds=%s\u3002", round, maxr))
  if (.is_standard_scenario(study$protocol) && identical(items, "unresolved")) {
    items <- "all"
  }

  if (.is_open_round(study$protocol, round)) {
    md <- .questionnaire_open(study)
    docs <- md
    names(docs) <- "round1"
    if (format %in% c("markdown", "html") && is.null(file)) {
      return(if (format == "html") .md_to_html(md, study$meta$title %||% "\u7b2c 1 \u8f6e\u5f00\u653e\u5f0f\u5f81\u8be2") else md)
    }
    return(.write_questionnaires(docs, study, round, format, file, items,
                                 expert_id, result, FALSE, prefill))
  }

  if (.is_first_scoring_round(study$protocol, round)) {
    md <- if (identical(study$protocol$scenario, "rand_ucla")) {
      .questionnaire_rand_round1(study)
    } else {
      .questionnaire_round1(study, round = round)
    }
    docs <- md
    names(docs) <- sprintf("round%s", round)
    if (format %in% c("markdown", "html") && is.null(file)) {
      return(if (format == "html") .md_to_html(md, study$meta$title %||% "\u7b2c 1 \u8f6e Delphi \u95ee\u5377") else md)
    }
    return(.write_questionnaires(docs, study, round, format, file, items,
                                 expert_id, result, FALSE, prefill))
  }

  .stop_if(is.null(study$ratings) || nrow(study$ratings) == 0L,
           "\u751f\u6210\u53cd\u9988\u8f6e\u95ee\u5377\u9700\u8981\u5df2\u6709\u4e0a\u4e00\u8f6e\u8bc4\u5206\u3002")
  src_round <- round - 1L
  have <- unique(study$ratings$round)
  .stop_if(!src_round %in% have, sprintf("\u7f3a\u5c11\u7b2c %s \u8f6e\u8bc4\u5206\uff0c\u65e0\u6cd5\u751f\u6210\u7b2c %s \u8f6e\u53cd\u9988\u3002", src_round, round))
  ids <- if (is.null(expert_id)) unique(as.character(study$panel$expert_id)) else as.character(expert_id)

  if (identical(study$protocol$scenario, "rand_ucla")) {
    if (identical(items, "unresolved")) {
      items <- "all"
      message("RAND \u7b2c 2 \u8f6e\u53ca\u4ee5\u540e\u590d\u8bc4\u5168\u90e8\u6761\u76ee\uff0c\u4e0d\u5254\u9664\u5df2\u5224\u4e3a\u9002\u5b9c\u6216\u4e0d\u9002\u5b9c\u7684\u6761\u76ee\u3002")
    }
    docs <- .questionnaire_rand_followup(study, round, ids)
    if (format %in% c("markdown", "html") && is.null(file)) {
      if (format == "html") {
        docs <- vapply(seq_along(docs), function(i) {
          .md_to_html(docs[[i]], sprintf("\u7b2c %s \u8f6e RAND \u00b7 %s", round, ids[i]))
        }, character(1))
        names(docs) <- ids
      }
      return(docs)
    }
    return(.write_questionnaires(docs, study, round, format, file, items,
                                 expert_id, result, split_experts, prefill))
  }

  fb_dim <- if (.is_standard_scenario(study$protocol)) "\u91cd\u8981\u7a0b\u5ea6" else NULL
  fb <- prepare_feedback(if (!is.null(result)) result else study, round = src_round, dimension = fb_dim)
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
  fb_fam <- NULL
  if (.is_standard_scenario(study$protocol)) {
    fb_fam <- prepare_feedback(
      if (!is.null(result)) result else study,
      round = src_round, dimension = "\u719f\u6089\u7a0b\u5ea6"
    )
  }
  docs <- vapply(ids, function(eid) {
    one <- fb[fb$expert_id == eid, , drop = FALSE]
    if (.is_standard_scenario(study$protocol)) {
      fam <- if (is.null(fb_fam)) one[0, ] else fb_fam[fb_fam$expert_id == eid, , drop = FALSE]
      .questionnaire_standard_followup(study, one, fam, round)
    } else {
      .questionnaire_followup(study, one, cls, items, round)
    }
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

.protocol_rules_rand <- function(p) {
  s <- .rand_settings(p)
  mode <- switch(
    s$round2_mode,
    in_person = "\u9762\u5bf9\u9762\u4f1a\u8bae\u540e\u518d\u6b21\u72ec\u7acb\u8bc4\u5206",
    online = "\u7ebf\u4e0a\u4f1a\u8bae\u540e\u518d\u6b21\u72ec\u7acb\u8bc4\u5206\uff08\u5bf9\u7ecf\u5178\u9762\u5bf9\u9762\u4f1a\u8bae\u7684\u53d8\u901a\uff09",
    mail = "\u518d\u6b21\u51fd\u8be2\uff0c\u6ca1\u6709\u4f1a\u8bae\uff08\u5bf9\u7ecf\u5178 RAM \u7684\u53d8\u901a\uff09",
    s$round2_mode
  )
  rule <- switch(
    s$rule,
    table4 = "\u624b\u518c\u8868 4\uff1a\u6309\u5b9e\u9645\u5e94\u7b54\u4eba\u6570\u67e5\u4e00\u81f4\u4e0e\u5206\u6b67\u9608\u503c\uff08Fitch \u7b49\uff0c2001\uff09\u3002",
    ipras = sprintf("IPRAS\uff1aIPR = P70 - P30\uff0cIPRAS = %s + %s \u00d7 |5 - IPR \u4e2d\u5fc3|\u3002IPR \u4e25\u683c\u5927\u4e8e IPRAS \u4e3a\u5206\u6b67\u3002\u5206\u4f4d\u6570 type = %s\u3002",
                     s$iprr, s$cfa, s$qtype),
    classic_relaxed = "9 \u4eba\u5bbd\u677e\u5b9a\u4e49\uff1a\u4e24\u7aef\u5404\u81f3\u5c11 3 \u4eba\u4e3a\u5206\u6b67\uff1b\u5254\u9664\u4e00\u4e2a\u6700\u9ad8\u5206\u548c\u4e00\u4e2a\u6700\u4f4e\u5206\u540e\uff0c\u5176\u4f59\u5206\u6570\u843d\u5728\u4efb\u610f 3 \u5206\u8de8\u5ea6\u5185\u4e3a\u4e00\u81f4\u3002",
    classic_strict = "9 \u4eba\u4e25\u683c\u5b9a\u4e49\uff1a\u4e24\u7aef\u5404\u81f3\u5c11 1 \u4eba\u4e3a\u5206\u6b67\uff1b\u5168\u90e8\u8bc4\u5206\u843d\u5728\u4efb\u610f 3 \u5206\u8de8\u5ea6\u5185\u4e3a\u4e00\u81f4\u3002",
    percent = sprintf("\u767e\u5206\u6bd4\u89c4\u5219\uff08RAM \u4fee\u6539\u7248\uff09\uff1a\u9ad8\u533a\u6bd4\u4f8b \u2265 %.0f%% \u4e14\u4f4e\u533a\u6bd4\u4f8b < %.0f%% \u4e3a\u4e00\u81f4\u3002\u4e0d\u5957\u7528 IPRAS \u5e38\u6570\u3002",
                       100 * s$percent_high, 100 * s$percent_low),
    both = "\u540c\u65f6\u8ba1\u7b97\u8868 4 \u4e0e IPRAS\uff0c\u4efb\u4e00\u5224\u4e3a\u5206\u6b67\u5373\u8bb0\u4e3a\u5206\u6b67\u3002",
    s$rule
  )
  bits <- c(
    sprintf("- **\u91cf\u8868\uff1a** %s\u2013%s\u3002%s", p$scale_min, p$scale_max, .rand_anchor_text(.rand_kind(p))),
    sprintf("- **\u5206\u6b67\u89c4\u5219\uff1a** %s", rule),
    "- **\u9002\u5b9c\u6027\uff1a** \u4e2d\u4f4d\u6570 7\u20139 \u4e14\u65e0\u5206\u6b67\u4e3a\u9002\u5b9c\uff1b1\u20133 \u4e14\u65e0\u5206\u6b67\u4e3a\u4e0d\u9002\u5b9c\uff1b\u4e2d\u4f4d\u6570 4\u20136\uff0c\u6216\u4efb\u4f55\u5206\u6b67\uff0c\u4e3a\u4e0d\u786e\u5b9a\u3002\u4e0d\u786e\u5b9a\u4f1a\u533a\u5206\u300c\u4e2d\u4f4d\u6570\u5c45\u4e2d\u300d\u4e0e\u300c\u5b58\u5728\u5206\u6b67\u300d\u3002",
    sprintf("- **\u8f6e\u6b21\uff1a** \u7b2c 1 \u8f6e\u72ec\u7acb\u8bc4\u5206\uff1b\u7b2c 2 \u8f6e\u4e3a%s\u3002\u7ecf\u5178\u9002\u5b9c\u6027\u4e0d\u8d85\u8fc7 2 \u8f6e\u3002", mode),
    if (isTRUE(s$necessity)) {
      "- **\u5fc5\u8981\u6027\uff1a** \u53ea\u8bc4\u4ef7\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u7684\u6761\u76ee\u3002\u5fc5\u8981\u6027\u4e2d\u4f4d\u6570 \u2265 7 \u4e14\u65e0\u5206\u6b67\u4e3a\u5fc5\u8981\uff0c\u5426\u5219\u4e3a\u9002\u5b9c\u4f46\u975e\u5fc5\u8981\u3002"
    } else {
      "- **\u5fc5\u8981\u6027\uff1a** \u672c\u65b9\u6848\u4e0d\u505a\u5fc5\u8981\u6027\u8bc4\u4ef7\u3002"
    },
    if (isTRUE(p$rand$nonstandard_ipras)) "- **\u975e\u6807\u51c6 IPRAS \u53c2\u6570\uff1a** \u5df2\u504f\u79bb 2.35\u30011.5 \u6216 type 7\uff0c\u62a5\u544a\u4e2d\u6309\u4fee\u6539\u7248\u8bf4\u660e\u3002" else NULL
  )
  paste(bits, collapse = "\n")
}

.protocol_rules_md <- function(p) {
  r <- p$rules
  if (identical(p$scenario, "rand_ucla")) return(.protocol_rules_rand(p))
  if (identical(p$scenario, "delphi_ram")) return(.protocol_rules_delphi_ram(p))
  if (.is_standard_scenario(p)) {
    return(paste(
      sprintf("- **\u91cf\u8868\uff1a** %s\u2013%s \u5206\u3002\u91cd\u8981\u7a0b\u5ea6\uff1a1 \u5f88\u4e0d\u91cd\u8981\uff0c2 \u4e0d\u91cd\u8981\uff0c3 \u4e00\u822c\uff0c4 \u6bd4\u8f83\u91cd\u8981\uff0c5 \u5f88\u91cd\u8981\u3002",
              p$scale_min, p$scale_max),
      "\u719f\u6089\u7a0b\u5ea6\uff1a1 \u5f88\u4e0d\u719f\u6089\uff0c2 \u4e0d\u719f\u6089\uff0c3 \u4e00\u822c\uff0c4 \u6bd4\u8f83\u719f\u6089\uff0c5 \u5f88\u719f\u6089\u3002",
      sprintf("- **\u7eb3\u5165\u6838\u5fc3\u6280\u672f\u8981\u7d20\uff1a** \u6700\u7ec8\u8f6e\u91cd\u8981\u7a0b\u5ea6\u5747\u6570 \u2265 %s\uff0c\u4e14\u8be5\u6761\u53d8\u5f02\u7cfb\u6570 \u2264 %s\u3002\u6ee1\u5206\u6bd4\u5fc5\u987b\u62a5\u544a\uff0c\u4e0d\u5355\u72ec\u4f5c\u4e3a\u5254\u9664\u7ebf\u3002",
              r$mean_min, r$cv_max),
      sprintf("- **\u7ed3\u675f\u8c03\u67e5\uff1a** \u81f3\u5c11\u4e24\u8f6e 5 \u5206\u8bc4\u5206\u540e\uff0c\u82e5\u91cd\u8981\u7a0b\u5ea6 Kendall W \u2265 %s\u3001\u5361\u65b9\u68c0\u9a8c P < %s\uff0c\u4e14\u5404\u6761\u76ee\u5747\u6570\u76f8\u5bf9\u4e0a\u8f6e\u7684\u5e73\u5747\u7edd\u5bf9\u53d8\u5316 < %s\uff0c\u5219\u534f\u8c03\u7a0b\u5ea6\u8f83\u597d\uff0c\u8c03\u67e5\u7ed3\u675f\u3002\u6700\u591a %s \u8f6e\u3002",
              r$w_min, r$w_p_max, r$mean_delta_max, p$stability$max_rounds),
      sprintf("- **\u53c2\u8003\u754c\u503c\uff08\u4e0d\u52a0\u6743\uff09\uff1a** \u79ef\u6781\u7cfb\u6570 \u2265 %s%%\uff1b\u7ec4\u5747\u6743\u5a01\u7cfb\u6570 \u2265 %s\u3002",
              round(100 * r$response_rate_min), r$cr_min),
      if (isTRUE(p$open_first)) {
        "- **\u7b2c 1 \u8f6e\uff1a** \u5f00\u653e\u5f0f\u5f81\u8be2\uff0c\u4e0d\u6253\u5206\u3002\u7528\u4e8e\u8865\u9f50\u7f3a\u5c11\u524d\u671f\u57fa\u7840\u7684\u8981\u7d20\u3002"
      } else {
        "- **\u7f16\u53f7\uff1a** \u8981\u7d20\u5747\u5df2\u6709\u524d\u671f\u57fa\u7840\uff0c\u672c\u6b21\u4ece\u8bc4\u5206\u8f6e\u5f00\u59cb\uff0c\u5bf9\u5e94\u6280\u672f\u8981\u6c42\u4e2d\u7684\u7b2c 2 \u8f6e\u8bc4\u5206\u8c03\u67e5\u3002"
      },
      sep = "\n"
    ))
  }
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
    if (!is.null(p$new_items) && isTRUE(p$new_items$allow)) {
      paste(
        sprintf("- **\u65b0\u589e\u6761\u76ee\uff1a** \u7b2c 1 \u8f6e\u53ef\u8865\u5145\uff0c\u6bcf\u4eba\u6700\u591a %s \u9879\u3002\u81f3\u5c11 %s \u540d\u4e13\u5bb6\u72ec\u7acb\u63d0\u51fa\u540c\u4e00\u65b0\u6982\u5ff5\uff0c\u6216\u4ec5 1 \u540d\u4f46\u6709\u6587\u732e\u3001\u8bbf\u8c08\u3001\u6307\u5357\u6216\u91cd\u8981\u4e34\u5e8a\u4f9d\u636e\uff0c\u5e76\u7ecf\u6761\u76ee\u5ba1\u67e5\u7ec4\u786e\u8ba4\u540e\uff0c\u624d\u8fdb\u5165\u4e0b\u4e00\u8f6e\u3002",
                p$new_items$max_suggestions, p$new_items$min_independent),
        "- \u8303\u56f4\u5916\u3001\u4e0e\u5df2\u6709\u6761\u76ee\u91cd\u590d\u3001\u6d4b\u91cf\u5de5\u5177\u3001\u6d4b\u91cf\u65f6\u70b9\u6216\u5355\u7eaf\u6587\u5b57\u4fee\u6539\uff0c\u4e0d\u4f5c\u4e3a\u65b0\u6761\u76ee\u3002\u672a\u8fdb\u5165\u4e0b\u4e00\u8f6e\u7684\u5efa\u8bae\u4ecd\u4f1a\u8bb0\u5f55\u7406\u7531\u3002",
        "- \u65b0\u6761\u76ee\u5728\u4e0b\u4e00\u8f6e\u662f\u9996\u6b21\u8bc4\u5206\uff0c\u4e0d\u63d0\u4f9b\u4e0a\u8f6e\u7edf\u8ba1\u3002\u81f3\u5c11\u518d\u7ecf\u8fc7\u4e00\u8f6e\u53cd\u9988\u540e\u7684\u8bc4\u5206\uff0c\u624d\u6309\u5171\u8bc6\u6807\u51c6\u6700\u7ec8\u7eb3\u5165\u3002",
        sep = "\n"
      )
    },
    sep = "\n"
  )
}

.scale_anchor <- function(p, label) {
  if (!.is_standard_scenario(p)) return("")
  if (grepl("\u719f\u6089", label)) {
    return("\uff081 \u5f88\u4e0d\u719f\u6089\uff0c2 \u4e0d\u719f\u6089\uff0c3 \u4e00\u822c\uff0c4 \u6bd4\u8f83\u719f\u6089\uff0c5 \u5f88\u719f\u6089\uff09")
  }
  if (grepl("\u91cd\u8981", label)) {
    return("\uff081 \u5f88\u4e0d\u91cd\u8981\uff0c2 \u4e0d\u91cd\u8981\uff0c3 \u4e00\u822c\uff0c4 \u6bd4\u8f83\u91cd\u8981\uff0c5 \u5f88\u91cd\u8981\uff09")
  }
  ""
}

.likert_line <- function(p, label) {
  nums <- paste(seq(p$scale_min, p$scale_max), collapse = "  ")
  sprintf("**%s\uff08%s\u2013%s\uff09%s\uff1a** %s\u3000\u3000\u25a1 \u65e0\u6cd5\u5224\u65ad / \u8d85\u51fa\u672c\u4eba\u4e13\u4e1a\u9886\u57df",
          label, p$scale_min, p$scale_max, .scale_anchor(p, label), nums)
}

.questionnaire_open <- function(study) {
  meta <- study$meta
  title <- meta$title %||% "\u6838\u5fc3\u6280\u672f\u8981\u7d20\u5fb7\u5c14\u83f2\u8c03\u67e5"
  blocks <- apply(study$items, 1, function(row) {
    src <- if ("source_type" %in% names(row) && !is.na(row[["source_type"]])) {
      paste0("- \u6765\u6e90\uff1a", row[["source_type"]],
             if ("source_note" %in% names(row) && !is.na(row[["source_note"]]) && nzchar(row[["source_note"]])) {
               paste0("\uff08", row[["source_note"]], "\uff09")
             } else "",
             "\n")
    } else ""
    paste0(
      "### \u8981\u7d20 ", row[["item_id"]], "\n\n",
      "**", row[["item_text"]], "**\n\n",
      src,
      "\u8bf7\u52fe\u9009\u4e00\u9879\uff1a\u25a1 \u4fdd\u7559\u3000\u25a1 \u4fee\u6539\u8868\u8ff0\u3000\u25a1 \u4e0d\u5efa\u8bae\u4f5c\u4e3a\u6838\u5fc3\u8981\u7d20\n\n",
      "**\u610f\u89c1\uff1a** ________________________________________________\n"
    )
  })
  paste0(
    "# \u7b2c 1 \u8f6e\u5f00\u653e\u5f0f\u5f81\u8be2\n\n",
    "**\u7814\u7a76\u9898\u76ee\uff1a** ", title, "\n\n",
    "**\u76ee\u7684\uff1a** ", meta$objective %||% "\uff08\u8bf7\u5728 study$meta$objective \u4e2d\u8865\u5145\uff09", "\n\n",
    "**\u5730\u7406\u8303\u56f4\uff1a** ", meta$geography %||% "\u672a\u8bf4\u660e", "\n\n",
    "> \u672c\u8f6e\u4e0d\u6253\u5206\u3002\u90e8\u5206\u6838\u5fc3\u6280\u672f\u8981\u7d20\u8fd8\u7f3a\u5c11\u53e4\u7c4d\u6216\u73b0\u4ee3\u6587\u732e\u57fa\u7840\uff0c",
    "\u8bf7\u5148\u63d0\u51fa\u4fdd\u7559\u3001\u4fee\u6539\u6216\u65b0\u589e\u5efa\u8bae\u3002\u7814\u7a76\u7ec4\u4f1a\u628a\u610f\u89c1\u51dd\u7ec3\u6210\u6761\u6b3e\uff0c\u518d\u8fdb\u5165 5 \u5206\u91cf\u8868\u8bc4\u5206\u3002\n\n",
    "## \u5df2\u6709\u8981\u7d20\n\n",
    paste(blocks, collapse = "\n"),
    "\n## \u5efa\u8bae\u65b0\u589e\u7684\u6838\u5fc3\u6280\u672f\u8981\u7d20\n\n",
    "\u8bf7\u5199\u51fa\u8981\u7d20\u8868\u8ff0\u3001\u4f9d\u636e\u6765\u6e90\uff08\u53e4\u7c4d\u3001\u73b0\u4ee3\u6587\u732e\u6216\u4e34\u5e8a\u7ecf\u9a8c\uff09\uff1a\n\n",
    "________________________________________________________________\n"
  )
}

.questionnaire_round1 <- function(study, round = 1L) {
  p <- study$protocol
  meta <- study$meta
  title <- meta$title %||% "Modified Delphi \u4e13\u5bb6\u54a8\u8be2"
  sc <- .protocol_authority_scale(p)
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
    dims <- .questionnaire_dimensions(study, round)
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
    sprintf("# \u7b2c %s \u8f6e Delphi \u4e13\u5bb6\u54a8\u8be2\u95ee\u5377\n\n", round),
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
    "> Cr = (Ca + Cs) / 2\u3002\u4e0d\u7528\u4e8e\u52a0\u6743\u8bc4\u5206\uff1b\u53ea\u5728\u9996\u6b21\u8bc4\u5206\u8f6e\u81ea\u8bc4\u4e00\u6b21\uff0c\u4ee5\u540e\u5404\u8f6e\u4e0d\u518d\u586b\u5199\u3002\n\n",
    "## \u6761\u76ee\u8bc4\u5206\n\n",
    paste(item_blocks, collapse = "\n"),
    .new_item_prompt_md(study),
    "\n"
  )
}

.new_item_prompt_md <- function(study) {
  p <- study$protocol
  if (is.null(p$new_items) || !isTRUE(p$new_items$allow)) {
    return(paste0(
      "\n## \u5176\u4ed6\u610f\u89c1\uff08\u53ef\u9009\uff09\n\n",
      "________________________________________________________________\n"
    ))
  }
  n <- p$new_items$max_suggestions
  objective <- study$meta$objective %||% "\u672c\u7814\u7a76\u8981\u8bc4\u4ef7\u7684\u95ee\u9898"
  blocks <- vapply(seq_len(n), function(i) {
    paste0(
      "### \u5efa\u8bae ", i, "\n\n",
      "**1. \u5efa\u8bae\u65b0\u589e\u6307\u6807\u540d\u79f0\uff1a** ________________________________\n\n",
      "**2. \u6307\u6807\u542b\u4e49\uff1a** ________________________________\n\n",
      "**3. \u63a8\u8350\u7406\u7531\uff1a** ________________________________\n\n",
      "**4. \u4e0e\u73b0\u6709\u6761\u76ee\u7684\u533a\u522b\uff1a** ________________________________\n\n",
      "**5. \u6587\u732e\u3001\u6307\u5357\u3001\u60a3\u8005\u8bbf\u8c08\u6216\u4e34\u5e8a\u4f9d\u636e\uff1a** ________________________________\n"
    )
  }, character(1))
  paste0(
    "\n## \u8865\u5145\u9057\u6f0f\u7684\u8bc4\u4ef7\u6307\u6807\n\n",
    "\u8bf7\u4e0d\u8981\u53ea\u5199\u300c\u8fd8\u6709\u5176\u4ed6\u610f\u89c1\u5417\u300d\u3002\u8bf7\u9488\u5bf9\u4e0b\u9762\u8fd9\u4e2a\u95ee\u9898\u9010\u9879\u56de\u7b54\u3002\n\n",
    "**\u60a8\u8ba4\u4e3a\u4e0a\u8ff0\u5019\u9009\u6307\u6807\u662f\u5426\u9057\u6f0f\u4e86\u8bc4\u4ef7\u672c\u7814\u7a76\u76ee\u7684\u6240\u5fc5\u9700\u7684\u91cd\u8981\u6307\u6807\uff1f**\n\n",
    "\u672c\u7814\u7a76\u76ee\u7684\uff1a", objective, "\n\n",
    sprintf("\u5982\u6709\uff0c\u8bf7\u6700\u591a\u586b\u5199 %s \u9879\u3002\u6bcf\u4e00\u9879\u90fd\u5199\u6e05\u540d\u79f0\u3001\u542b\u4e49\u3001\u7406\u7531\u3001\u4e0e\u73b0\u6709\u6761\u76ee\u7684\u533a\u522b\uff0c\u4ee5\u53ca\u4f9d\u636e\u3002", n),
    "\u7814\u7a76\u7ec4\u4f1a\u53bb\u91cd\u3001\u5f52\u5e76\uff0c\u5e76\u6309\u4e8b\u5148\u9501\u5b9a\u7684\u53cc\u901a\u9053\u89c4\u5219\u51b3\u5b9a\u662f\u5426\u8fdb\u5165\u4e0b\u4e00\u8f6e\u3002",
    "\u5199\u5230\u5e76\u4e0d\u7b49\u4e8e\u4f1a\u52a0\u5165\u3002\u672a\u8fdb\u5165\u7684\u5efa\u8bae\u4e5f\u4f1a\u8bb0\u5f55\u7406\u7531\u3002",
    "\u540c\u4e00\u4f4d\u4e13\u5bb6\u628a\u540c\u4e00\u6761\u5199\u591a\u6b21\uff0c\u53ea\u7b97 1 \u4eba\u3002\n\n",
    paste(blocks, collapse = "\n")
  )
}

.questionnaire_round2 <- function(study, fb_one, cls, items) {
  .questionnaire_followup(study, fb_one, cls, items, round = 2L)
}

.questionnaire_standard_followup <- function(study, fb_imp, fb_fam, round) {
  eid <- if (nrow(fb_imp)) fb_imp$expert_id[1] else if (nrow(fb_fam)) fb_fam$expert_id[1] else ""
  title <- study$meta$title %||% "\u6838\u5fc3\u6280\u672f\u8981\u7d20\u5fb7\u5c14\u83f2\u8c03\u67e5"
  src_round <- as.integer(round) - 1L
  ids <- unique(c(as.character(fb_imp$item_id), as.character(fb_fam$item_id)))
  blocks <- vapply(ids, function(iid) {
    imp <- fb_imp[fb_imp$item_id == iid, , drop = FALSE]
    fam <- fb_fam[fb_fam$item_id == iid, , drop = FALSE]
    text <- if (nrow(imp)) imp$item_text[1] else if (nrow(fam)) fam$item_text[1] else iid
    cell <- function(d, col, fmt = "%s") {
      if (!nrow(d) || !col %in% names(d) || is.na(d[[col]][1])) return("\u2014")
      if (is.numeric(d[[col]][1])) sprintf(fmt, d[[col]][1]) else as.character(d[[col]][1])
    }
    note <- if (nrow(imp) && !is.na(imp$anonymous_comments[1]) && nzchar(imp$anonymous_comments[1])) {
      paste0("**\u533f\u540d\u610f\u89c1\u6458\u8981\uff1a** ", imp$anonymous_comments[1], "\n\n")
    } else ""
    paste0(
      "### \u8981\u7d20 ", iid, "\n\n",
      "**", text, "**\n\n",
      "| \u7ef4\u5ea6 | \u60a8\u7684\u4e0a\u8f6e\u8bc4\u5206 | \u7ec4\u5747\u6570 | \u6ee1\u5206\u6bd4 | \u6709\u6548\u4eba\u6570 |\n",
      "|---|---|---|---|---|\n",
      sprintf("| \u91cd\u8981\u7a0b\u5ea6 | %s | %s | %s%% | %s |\n",
              cell(imp, "own_score"), cell(imp, "mean", "%.2f"),
              cell(imp, "full_rate", "%.0f"), cell(imp, "n_valid", "%.0f")),
      sprintf("| \u719f\u6089\u7a0b\u5ea6 | %s | %s | %s%% | %s |\n\n",
              cell(fam, "own_score"), cell(fam, "mean", "%.2f"),
              cell(fam, "full_rate", "%.0f"), cell(fam, "n_valid", "%.0f")),
      note,
      .likert_line(study$protocol, "\u672c\u8f6e\u91cd\u8981\u7a0b\u5ea6"), "\n",
      .likert_line(study$protocol, "\u672c\u8f6e\u719f\u6089\u7a0b\u5ea6"), "\n",
      "**\u672c\u8f6e\u610f\u89c1\uff08\u53ef\u7a7a\uff09\uff1a** ________________________________________________\n"
    )
  }, character(1))
  paste0(
    sprintf("# \u7b2c %s \u8f6e\u5fb7\u5c14\u83f2\u8bc4\u5206\u95ee\u5377\n\n", round),
    "**\u7814\u7a76\u9898\u76ee\uff1a** ", title, "\n\n",
    "**\u672c\u5377\u9002\u7528\u4e13\u5bb6\u7f16\u53f7\uff1a** ", eid, "\n\n",
    sprintf("\u8bf7\u5148\u770b\u7b2c %s \u8f6e\u7684\u7ec4\u5747\u6570\u3001\u6ee1\u5206\u6bd4\uff0c\u4ee5\u53ca\u60a8\u672c\u4eba\u4e0a\u4e00\u8f6e\u7684\u91cd\u8981\u7a0b\u5ea6\u548c\u719f\u6089\u7a0b\u5ea6\uff0c\u518d\u7ed9\u51fa\u672c\u8f6e\u5206\u6570\u3002", src_round),
    "\u672c\u8f6e\u4e0d\u518d\u586b\u5199\u6743\u5a01\u7cfb\u6570\u3002\u6bcf\u4e00\u6761\u8981\u7d20\u90fd\u8981\u91cd\u65b0\u56de\u7b54\u3002\n\n",
    "## \u5df2\u9501\u5b9a\u7684\u5224\u5b9a\u89c4\u5219\n\n",
    .protocol_rules_md(study$protocol), "\n\n",
    paste(blocks, collapse = "\n")
  )
}

.questionnaire_followup <- function(study, fb_one, cls, items, round) {
  eid <- if (nrow(fb_one)) fb_one$expert_id[1] else ""
  title <- study$meta$title %||% "Modified Delphi"
  src_round <- as.integer(round) - 1L
  selected <- .items_for_round(study, cls, items, round)
  fb_use <- fb_one[fb_one$item_id %in% selected, , drop = FALSE]
  fb_skip <- fb_one[!fb_one$item_id %in% selected, , drop = FALSE]
  first_ids <- setdiff(selected, as.character(fb_use$item_id))
  dim_label <- .primary_dimension(study, round)
  block <- function(d) {
    if (!nrow(d)) return("")
    paste(apply(d, 1, function(row) {
      provisional <- !is.na(row[["class_zh"]]) && startsWith(as.character(row[["class_zh"]]), "\u6682\u65f6")
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
        if (provisional) {
          "\u4e0a\u4e00\u8f6e\u662f\u8be5\u6761\u76ee\u7684\u9996\u6b21\u8bc4\u5206\u3002\u5373\u4f7f\u5df2\u7ecf\u8fc7\u7ebf\uff0c\u4e5f\u8fd8\u4e0d\u662f\u6700\u7ec8\u7eb3\u5165\u3002\u8bf7\u9605\u8bfb\u53cd\u9988\u540e\u91cd\u65b0\u8bc4\u5206\u3002\n\n"
        } else "",
        if (!is.na(row[["anonymous_comments"]]) && nzchar(row[["anonymous_comments"]]))
          paste0("**\u533f\u540d\u610f\u89c1\u6458\u8981\uff1a** ", row[["anonymous_comments"]], "\n\n") else "",
        .likert_line(study$protocol, paste0("\u672c\u8f6e", dim_label)), "\n",
        "**\u672c\u8f6e\u610f\u89c1\uff08\u53ef\u7a7a\uff09\uff1a** ________________________________________________\n"
      )
    }), collapse = "\n")
  }
  first_block <- function(ids) {
    if (!length(ids)) return("")
    paste(vapply(ids, function(iid) {
      row <- study$items[as.character(study$items$item_id) == iid, , drop = FALSE]
      text <- if (nrow(row)) row$item_text[1] else iid
      note <- ""
      if (nrow(row) && "source_note" %in% names(row) && !is.na(row$source_note[1]) && nzchar(row$source_note[1])) {
        note <- paste0("- \u6765\u6e90\u8bf4\u660e\uff1a", row$source_note[1], "\n\n")
      }
      paste0(
        "### \u6761\u76ee ", iid, "\n\n",
        "**", text, "**\n\n",
        "> \u4ee5\u4e0b\u4e3a\u7b2c\u4e00\u8f6e\u4e13\u5bb6\u65b0\u589e\u6761\u76ee\u3002\u672c\u8f6e\u662f\u7b2c\u4e00\u6b21\u8bc4\u5206\u3002",
        "\u6ca1\u6709\u4e0a\u4e00\u8f6e\u4e2d\u4f4d\u6570\u3001IQR \u6216\u533a\u95f4\u6bd4\u4f8b\uff0c\u8bf7\u4e0d\u8981\u628a\u7a7a\u767d\u7406\u89e3\u6210 0 \u5206\u6216\u4e2d\u95f4\u5206\u3002\n\n",
        note,
        .likert_line(study$protocol, paste0("\u672c\u8f6e", dim_label)), "\n",
        "**\u672c\u8f6e\u610f\u89c1\uff08\u53ef\u7a7a\uff09\uff1a** ________________________________________________\n"
      )
    }, character(1)), collapse = "\n")
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
  first_sec <- ""
  if (length(first_ids)) {
    first_sec <- paste0(
      "## \u7b2c\u4e00\u8f6e\u4e13\u5bb6\u65b0\u589e\u6761\u76ee\uff08\u672c\u8f6e\u4e3a\u9996\u6b21\u8bc4\u5206\uff09\n\n",
      "\u8fd9\u4e9b\u6761\u76ee\u6ca0\u6709\u4e0a\u4e00\u8f6e\u7edf\u8ba1\u3002\u8bf7\u6309\u91cf\u8868\u9996\u6b21\u8bc4\u5206\u3002\u65e0\u6cd5\u8bc4\u4ef7\u8bf7\u52fe\u9009\u300c\u65e0\u6cd5\u5224\u65ad\u300d\uff0c\u4e0d\u8981\u586b\u4e2d\u95f4\u5206\u3002\n\n",
      first_block(first_ids), "\n"
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
    first_sec,
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
