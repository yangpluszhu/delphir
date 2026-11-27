# Excel / Word questionnaire I/O -------------------------------------------

.SURVEY_SCHEMA <- "1.0"

.sheet_alias <- function(sheets, candidates) {
  hit <- intersect(candidates, sheets)
  if (length(hit)) return(hit[1])
  low <- tolower(sheets)
  for (cnd in candidates) {
    w <- which(low == tolower(cnd))
    if (length(w)) return(sheets[w[1]])
  }
  NA_character_
}

.questionnaire_dimensions <- function(study, round = 1L) {
  p <- study$protocol
  force_round <- attr(study, "rand_force_round")
  force_dim <- attr(study, "rand_force_dimension")
  if (!is.null(force_dim) && (is.null(force_round) || identical(as.integer(round), as.integer(force_round)))) {
    return(as.character(force_dim))
  }
  if (.is_open_round(p, round)) return(character())
  if (.is_standard_scenario(p)) return(c("\u91cd\u8981\u7a0b\u5ea6", "\u719f\u6089\u7a0b\u5ea6"))
  if (!is.null(study$ratings) && nrow(study$ratings) > 0L) {
    r <- .ensure_dimension(study$ratings)
    have <- unique(as.character(r$dimension[r$round == round]))
    have <- have[!is.na(have) & nzchar(have)]
    if (length(have)) return(have)
  }
  switch(
    p$scenario,
    clinical_consensus = if (round <= 1L) c("\u91cd\u8981\u6027", "\u53ef\u884c\u6027") else "\u91cd\u8981\u6027",
    definition = if (round <= 1L) c("\u91cd\u8981\u6027", "\u53ef\u884c\u6027") else "\u91cd\u8981\u6027",
    rand_ucla = if (identical(p$rand$purpose, "recommendation")) "\u540c\u610f\u7a0b\u5ea6" else "\u9002\u5b9c\u6027",
    "\u91cd\u8981\u6027"
  )
}

.primary_dimension <- function(study, round = 1L) {
  dims <- .questionnaire_dimensions(study, round)
  prefer <- intersect(c("\u91cd\u8981\u7a0b\u5ea6", "\u91cd\u8981\u6027", "overall", "\u9002\u5b9c\u6027"), dims)
  if (length(prefer)) prefer[1] else dims[1]
}

.unresolved_item_ids <- function(cls, round, items = "unresolved", dimension = NULL) {
  sub <- cls
  if (is.null(sub) || !nrow(sub)) return(character())
  if ("round" %in% names(sub)) sub <- sub[sub$round == round, , drop = FALSE]
  if (!is.null(dimension) && "dimension" %in% names(sub) && nrow(sub)) {
    sub <- sub[sub$dimension == dimension, , drop = FALSE]
  }
  if (identical(items, "all") || !nrow(sub)) {
    return(unique(as.character(sub$item_id)))
  }
  unresolved <- rep(FALSE, nrow(sub))
  if ("action" %in% names(sub)) {
    unresolved <- unresolved | sub$action %in% "discuss"
    unresolved <- unresolved | !sub$action %in% c("keep", "drop")
  }
  if ("class" %in% names(sub)) {
    unresolved <- unresolved | sub$class %in% c(
      "borderline", "no_consensus", "polarized", "uncertain"
    )
  }
  unique(as.character(sub$item_id[unresolved]))
}

.write_survey_xlsx <- function(study, round, file, items = "unresolved",
                               expert_id = NULL, result = NULL, prefill = FALSE) {
  .need_pkg("openxlsx")
  round <- as.integer(round[1])
  p <- study$protocol
  meta <- study$meta
  dims <- .questionnaire_dimensions(study, round)
  panel <- study$panel
  ids_all <- as.character(panel$expert_id)
  if (!is.null(expert_id)) ids_all <- intersect(ids_all, as.character(expert_id))
  .stop_if(!length(ids_all), "\u6ca0\u6709\u53ef\u5199\u5165\u95ee\u5377\u7684\u4e13\u5bb6\u3002")

  fb <- NULL
  cls <- NULL
  src_round <- round - 1L
  feedback_round <- round >= 2L && !.is_first_scoring_round(p, round) && !.is_open_round(p, round)
  if (feedback_round) {
    dims_prev <- .questionnaire_dimensions(study, src_round)
    if (!length(dims_prev)) dims_prev <- dims
    src <- if (!is.null(result)) result else study
    fb <- do.call(rbind, lapply(dims_prev, function(dim) {
      prepare_feedback(src, round = src_round, dimension = dim)
    }))
    rownames(fb) <- NULL
    if (!is.null(result)) cls <- result$classification
    else {
      s <- summarize_items(study, rounds = src_round)
      cls <- if (identical(p$scenario, "rand_ucla")) {
        classify_rand(study, round = src_round, summary = s)
      } else if (identical(p$scenario, "delphi_ram")) {
        classify_delphi_ram(study, round = src_round, summary = s)
      } else classify_consensus(s, p)
    }
  }

  item_ids <- .items_for_round(study, cls, items, round)
  if (!length(item_ids)) item_ids <- .active_item_ids(study$items)

  rating_rows <- list()
  for (eid in ids_all) {
    for (iid in item_ids) {
      for (dim in dims) {
        rec <- data.frame(
          expert_id = eid,
          item_id = iid,
          item_text = study$items$item_text[match(iid, study$items$item_id)],
          dimension = dim,
          round = round,
          score = NA_real_,
          unable = FALSE,
          stringsAsFactors = FALSE
        )
        if (round >= 2L && !is.null(fb)) {
          hit <- fb[fb$expert_id == eid & fb$item_id == iid, , drop = FALSE]
          if ("dimension" %in% names(fb) && nrow(hit)) {
            hit_dim <- hit[hit$dimension == dim, , drop = FALSE]
            if (nrow(hit_dim)) hit <- hit_dim
          }
          if (nrow(hit)) {
            rec$own_prev <- hit$own_score[1]
            rec$median_iqr <- hit$median_iqr[1]
            rec$p_high <- hit$p_high[1]
            rec$p_low <- hit$p_low[1]
            rec$group_mean <- if ("mean" %in% names(hit)) hit$mean[1] else NA_real_
            rec$full_rate <- if ("full_rate" %in% names(hit)) hit$full_rate[1] else NA_real_
            rec$class_zh <- hit$class_zh[1]
            rec$anonymous_comments <- hit$anonymous_comments[1]
            rec$first_scoring <- FALSE
            rec$scoring_note <- if (!is.na(rec$class_zh) && startsWith(as.character(rec$class_zh), "\u6682\u65f6")) {
              "\u4e0a\u4e00\u8f6e\u662f\u9996\u6b21\u8bc4\u5206\uff0c\u5206\u7c7b\u662f\u6682\u65f6\u7684\uff0c\u672c\u8f6e\u8bf7\u5728\u53cd\u9988\u540e\u91cd\u8bc4\u3002"
            } else {
              ""
            }
          } else {
            rec$own_prev <- NA_character_
            rec$median_iqr <- NA_character_
            rec$p_high <- NA_real_
            rec$p_low <- NA_real_
            rec$group_mean <- NA_real_
            rec$full_rate <- NA_real_
            rec$class_zh <- NA_character_
            rec$anonymous_comments <- ""
            rec$first_scoring <- TRUE
            rec$scoring_note <- "\u9996\u6b21\u8bc4\u5206\uff0c\u65e0\u4e0a\u8f6e\u7edf\u8ba1\u3002\u8bf7\u52ff\u628a\u7a7a\u767d\u7406\u89e3\u6210 0 \u6216\u4e2d\u95f4\u5206\u3002"
          }
        }
        if (isTRUE(prefill) && !is.null(study$ratings) && nrow(study$ratings)) {
          rr <- .ensure_dimension(.ensure_unable(study$ratings))
          old <- rr[rr$expert_id == eid & rr$item_id == iid &
                      rr$round == round & rr$dimension == dim, , drop = FALSE]
          if (nrow(old)) {
            rec$score <- old$score[1]
            rec$unable <- isTRUE(old$unable[1])
          }
        }
        rating_rows[[length(rating_rows) + 1L]] <- rec
      }
    }
  }
  ratings_df <- if (length(rating_rows)) {
    out <- do.call(rbind, rating_rows)
    rownames(out) <- NULL
    out
  } else {
    data.frame(
      expert_id = character(), item_id = character(), item_text = character(),
      dimension = character(), round = integer(), score = numeric(),
      unable = logical(), stringsAsFactors = FALSE
    )
  }

  if (.is_open_round(p, round)) {
    suggest_id <- "\u65b0\u589e\u5efa\u8bae"
    comment_items <- unique(c(as.character(study$items$item_id), suggest_id))
    comments_df <- expand.grid(
      expert_id = ids_all,
      item_id = comment_items,
      stringsAsFactors = FALSE
    )
    comments_df$round <- round
    comments_df$comment <- ""
    comments_df$theme <- ifelse(comments_df$item_id == suggest_id, "\u65b0\u589e", "")
  } else {
    comments_df <- data.frame(
      expert_id = ratings_df$expert_id,
      item_id = ratings_df$item_id,
      round = round,
      comment = "",
      stringsAsFactors = FALSE
    )
    comments_df <- comments_df[!duplicated(comments_df[c("expert_id", "item_id", "round")]), ]
  }
  if (isTRUE(prefill) && !is.null(study$comments) && nrow(study$comments)) {
    cc <- study$comments[study$comments$round == round, ]
    if (nrow(cc)) {
      comments_df$comment <- cc$comment[match(
        paste(comments_df$expert_id, comments_df$item_id),
        paste(cc$expert_id, cc$item_id)
      )]
      comments_df$comment[is.na(comments_df$comment)] <- ""
      if ("theme" %in% names(cc)) {
        if (!"theme" %in% names(comments_df)) comments_df$theme <- ""
        got_theme <- cc$theme[match(
          paste(comments_df$expert_id, comments_df$item_id),
          paste(cc$expert_id, cc$item_id)
        )]
        fill <- !is.na(got_theme) & nzchar(as.character(got_theme))
        comments_df$theme[fill] <- as.character(got_theme[fill])
      }
    }
  }

  proto <- protocol_table(p)
  proto_out <- proto[, c("parameter", "value", "preset", "meaning")]
  names(proto_out) <- c("\u53c2\u6570", "\u503c", "\u521d\u59cb\u9501\u5b9a\u503c", "\u542b\u4e49")

  panel_cols <- intersect(
    c("expert_id", "sex", "age", "education", "stakeholder", "specialty",
      "department", "region", "title", "title_level", "years",
      "in_expert_group", "invited"),
    names(panel)
  )
  panel_out <- panel[panel$expert_id %in% ids_all, panel_cols, drop = FALSE]

  auth_cols <- c("expert_id", "familiarity", "practice", "theory", "literature", "intuition")
  auth_out <- NULL
  if (.is_first_scoring_round(p, round)) {
    auth_out <- data.frame(expert_id = ids_all, stringsAsFactors = FALSE)
    for (nm in setdiff(auth_cols, "expert_id")) {
      auth_out[[nm]] <- if (nm %in% names(panel)) {
        as.character(panel[[nm]][match(ids_all, panel$expert_id)])
      } else NA_character_
    }
  }

  item_cols <- intersect(
    c("item_id", "item_text", "domain", "source_type", "source_note",
      "evidence_level", "outcome_domain", "indication", "procedure"),
    names(study$items)
  )
  items_out <- study$items[, item_cols, drop = FALSE]

  readme <- data.frame(
    note = c(
      sprintf("\u7b2c %s \u8f6e Delphi \u95ee\u5377\uff08xlsx \u5f55\u5165\u8868\uff09", round),
      sprintf("\u7814\u7a76\uff1a%s", meta$title %||% ""),
      "\u9ec4\u8272\u5217\u4e3a\u5fc5\u586b\uff1a\u8bc4\u5206 score\u3001\u65e0\u6cd5\u5224\u65ad unable\u3002",
      "score \u5fc5\u987b\u662f\u91cf\u8868\u5185\u6574\u6570\uff1b\u82e5\u65e0\u6cd5\u5224\u65ad\u8bf7\u586b unable=TRUE \u5e76\u7559\u7a7a score\u3002",
      "\u4e0d\u8981\u628a\u300c\u4e0d\u719f\u300d\u586b\u6210\u4e2d\u95f4\u5206\u3002\u6743\u5a01\u7cfb\u6570\u53ea\u5728\u9996\u6b21\u8bc4\u5206\u8f6e\u586b\u300c\u6743\u5a01\u7cfb\u6570\u300d\u8868\u3002",
      if (.is_open_round(p, round)) {
        "\u672c\u8f6e\u4e3a\u5f00\u653e\u5f0f\u5f81\u8be2\uff1a\u8bf7\u5728\u300c\u610f\u89c1\u300d\u8868\u586b\u5199\u4fee\u6539\u6216\u65b0\u589e\u5efa\u8bae\uff0c\u4e0d\u8981\u5728\u300c\u8bc4\u5206\u300d\u8868\u6253\u5206\u3002"
      } else       if (feedback_round) {
        paste0(
          "\u8bf7\u5148\u770b own_prev\uff08\u672c\u4eba\u4e0a\u8f6e\u8bc4\u5206\uff09\u4e0e\u7ec4\u7ed3\u679c\uff0c\u518d\u586b\u5199\u672c\u8f6e score\u3002",
          "first_scoring \u4e3a TRUE \u7684\u884c\u6ca0\u6709\u4e0a\u8f6e\u7edf\u8ba1\uff0c\u4e0d\u8981\u628a\u7a7a\u767d\u5f53\u6210 0 \u5206\u3002\u4e0d\u8981\u91cd\u586b\u6743\u5a01\u7cfb\u6570\u3002"
        )
      } else {
        "\u8bf7\u586b\u5199\u4e13\u5bb6\u80cc\u666f\u3001\u6743\u5a01\u7cfb\u6570\u81ea\u8bc4\uff0c\u5e76\u5bf9\u6bcf\u4e2a\u8981\u7d20\u7684\u91cd\u8981\u7a0b\u5ea6\u4e0e\u719f\u6089\u7a0b\u5ea6\u6253\u5206\u3002"
      },
      "\u65b9\u6848\u8868\u4e3a\u53ea\u8bfb\u3002\u56de\u6536\u540e\u7528 read_survey_xlsx() \u8bfb\u5165\u3002",
      paste(.protocol_rules_md(p), collapse = " ")
    ),
    stringsAsFactors = FALSE
  )
  names(readme)[1] <- "\u8bf4\u660e"

  meta_df <- data.frame(
    key = c("schema_version", "package", "package_version", "round", "scenario",
            "title", "items_mode"),
    value = c(
      .SURVEY_SCHEMA, "delphir", .pkg_version(),
      as.character(round), p$scenario, meta$title %||% "", items
    ),
    stringsAsFactors = FALSE
  )

  wb <- openxlsx::createWorkbook()
  add <- function(name, data, withFilter = TRUE) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, data, withFilter = withFilter)
  }
  add("\u8bf4\u660e", readme, withFilter = FALSE)
  add("\u65b9\u6848", proto_out)
  add("\u4e13\u5bb6", panel_out)
  if (!is.null(auth_out)) add("\u6743\u5a01\u7cfb\u6570", auth_out)
  add("\u6761\u76ee", items_out)
  add("\u8bc4\u5206", ratings_df)
  add("\u610f\u89c1", comments_df)
  sugg_out <- .suggestion_sheet(study, round, ids_all, prefill)
  if (!is.null(sugg_out)) add("\u65b0\u589e\u5efa\u8bae", sugg_out)
  add("_meta", meta_df, withFilter = FALSE)

  header <- openxlsx::createStyle(
    fgFill = "#1A365D", fontColour = "white", textDecoration = "bold",
    locked = TRUE, wrapText = TRUE
  )
  yellow <- openxlsx::createStyle(fgFill = "#FFF2CC", locked = FALSE)
  locked <- openxlsx::createStyle(locked = TRUE)
  for (sh in names(wb)) {
    nr <- openxlsx::read.xlsx(wb, sh)
    ncols <- if (is.null(nr) || !ncol(nr)) 1L else ncol(nr)
    nrows <- if (is.null(nr)) 0L else nrow(nr)
    openxlsx::addStyle(wb, sh, header, rows = 1, cols = seq_len(ncols),
                       gridExpand = TRUE, stack = TRUE)
    if (nrows > 0L) {
      openxlsx::addStyle(wb, sh, locked, rows = 2:(nrows + 1L), cols = seq_len(ncols),
                         gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
  }

  score_col <- match("score", names(ratings_df))
  unable_col <- match("unable", names(ratings_df))
  nrt <- nrow(ratings_df)
  if (nrt > 0L && !is.na(score_col)) {
    openxlsx::addStyle(wb, "\u8bc4\u5206", yellow, rows = 2:(nrt + 1L), cols = score_col,
                       gridExpand = TRUE, stack = TRUE)
    scale_vals <- paste(seq(p$scale_min, p$scale_max), collapse = ",")
    openxlsx::dataValidation(
      wb, "\u8bc4\u5206", cols = score_col, rows = 2:(nrt + 1L),
      type = "list", value = sprintf('"%s"', scale_vals)
    )
  }
  if (nrt > 0L && !is.na(unable_col)) {
    openxlsx::addStyle(wb, "\u8bc4\u5206", yellow, rows = 2:(nrt + 1L), cols = unable_col,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::dataValidation(
      wb, "\u8bc4\u5206", cols = unable_col, rows = 2:(nrt + 1L),
      type = "list", value = '"TRUE,FALSE"'
    )
  }
  if (nrow(comments_df) > 0L) {
    ccol <- match("comment", names(comments_df))
    openxlsx::addStyle(wb, "\u610f\u89c1", yellow, rows = 2:(nrow(comments_df) + 1L),
                       cols = ccol, gridExpand = TRUE, stack = TRUE)
  }
  if (!is.null(sugg_out) && nrow(sugg_out) > 0L) {
    for (nm in c("raw_name", "meaning", "rationale", "distinction", "evidence_note")) {
      cc <- match(nm, names(sugg_out))
      if (!is.na(cc)) {
        openxlsx::addStyle(wb, "\u65b0\u589e\u5efa\u8bae", yellow,
                           rows = 2:(nrow(sugg_out) + 1L), cols = cc,
                           gridExpand = TRUE, stack = TRUE)
      }
    }
  }
  if (!is.null(auth_out) && nrow(auth_out) > 0L) {
    for (nm in setdiff(names(auth_out), "expert_id")) {
      cc <- match(nm, names(auth_out))
      openxlsx::addStyle(wb, "\u6743\u5a01\u7cfb\u6570", yellow,
                         rows = 2:(nrow(auth_out) + 1L), cols = cc,
                         gridExpand = TRUE, stack = TRUE)
    }
  }

  openxlsx::protectWorksheet(wb, "\u65b9\u6848", protect = TRUE)
  openxlsx::protectWorksheet(wb, "_meta", protect = TRUE)

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

#' 读取自动生成的 Delphi 问卷 xlsx
#'
#' 识别由 [delphi_questionnaire()] 写出的工作簿（或同一结构的目录下多个
#' xlsx），整理为分析用的 `panel` / `items` / `ratings` / `comments` /
#' `authority_raw`。若提供已有 `study`，则把本轮数据 [update_delphi_study()]
#' 并入并返回更新后的研究。
#'
#' @param path 单个 `.xlsx` 文件，或含多个问卷工作簿的目录。
#' @param study 可选的已有 [delphi_study()]；提供时追加本轮评分。
#' @param protocol 若 `study` 为空，用于组装新研究的方案；缺省则尝试从工作簿
#'   「方案」表还原。
#'
#' @return 若未提供 `study`，返回列表（含 `round`、`panel`、`items`、`ratings`、
#'   `comments`、`authority_raw`、`protocol`）。若提供 `study`，返回更新后的
#'   `delphi_study`。
#'
#' @examples
#' \donttest{
#' study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 8)
#' f <- tempfile(fileext = ".xlsx")
#' delphi_questionnaire(study, round = 1, format = "xlsx", file = f, prefill = TRUE)
#' got <- read_survey_xlsx(f)
#' nrow(got$ratings)
#' }
#'
#' @export
read_survey_xlsx <- function(path, study = NULL, protocol = NULL) {
  files <- .xlsx_paths(path)
  .stop_if(!length(files), "\u672a\u627e\u5230 xlsx \u95ee\u5377\u3002")
  parsed <- lapply(files, .read_one_survey_xlsx)
  proto <- protocol
  if (is.null(proto) && !is.null(study)) proto <- study$protocol
  if (is.null(proto)) proto <- parsed[[1]]$protocol

  panel <- .rbind_fill_unique(lapply(parsed, `[[`, "panel"), "expert_id")
  items <- .rbind_fill_unique(lapply(parsed, `[[`, "items"), "item_id")
  ratings <- do.call(rbind, lapply(parsed, `[[`, "ratings"))
  comments <- do.call(rbind, lapply(parsed, `[[`, "comments"))
  suggestions <- do.call(rbind, lapply(parsed, function(x) {
    if (is.null(x$suggestions) || !nrow(x$suggestions)) return(NULL)
    x$suggestions
  }))
  authority_raw <- .rbind_fill_unique(lapply(parsed, `[[`, "authority_raw"), "expert_id")
  round <- unique(unlist(lapply(parsed, `[[`, "round")))

  if (!is.null(authority_raw) && nrow(authority_raw) &&
      all(c("familiarity", "practice", "theory", "literature", "intuition") %in% names(authority_raw))) {
    filled <- stats::complete.cases(authority_raw[, c("familiarity", "practice", "theory",
                                                      "literature", "intuition")])
    if (any(filled)) {
      scored <- tryCatch(
        score_authority(authority_raw[filled, , drop = FALSE]),
        error = function(e) NULL
      )
      if (!is.null(scored) && !is.null(panel)) {
        panel$Ca <- scored$Ca[match(panel$expert_id, scored$expert_id)]
        panel$Cs <- scored$Cs[match(panel$expert_id, scored$expert_id)]
        for (nm in c("familiarity", "practice", "theory", "literature", "intuition")) {
          if (!nm %in% names(panel)) panel[[nm]] <- NA_character_
          panel[[nm]] <- ifelse(
            is.na(panel[[nm]]) | !nzchar(as.character(panel[[nm]])),
            as.character(authority_raw[[nm]][match(panel$expert_id, authority_raw$expert_id)]),
            as.character(panel[[nm]])
          )
        }
      }
    }
  }

  if (!is.null(ratings) && nrow(ratings)) ratings <- delphi_ratings(ratings)
  if (!is.null(comments) && nrow(comments)) comments <- delphi_comments(comments)
  if (!is.null(panel) && nrow(panel)) panel <- delphi_panel(panel)
  if (!is.null(items) && nrow(items)) items <- delphi_items(items)

  if (!is.null(study)) {
    return(update_delphi_study(
      study,
      ratings = ratings,
      comments = comments,
      panel = panel,
      items = items,
      suggestions = suggestions
    ))
  }

  list(
    round = round,
    protocol = proto,
    panel = panel,
    items = items,
    ratings = ratings,
    comments = comments,
    suggestions = suggestions,
    authority_raw = authority_raw
  )
}

.xlsx_paths <- function(path) {
  path <- path[1]
  if (dir.exists(path)) {
    return(list.files(path, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE))
  }
  .stop_if(!file.exists(path), sprintf("\u627e\u4e0d\u5230\u6587\u4ef6: %s", path))
  path
}

.read_one_survey_xlsx <- function(file) {
  .need_pkg("openxlsx")
  sheets <- openxlsx::getSheetNames(file)
  sh <- function(...) .sheet_alias(sheets, c(...))
  s_meta <- sh("_meta", "meta")
  s_proto <- sh("\u65b9\u6848", "protocol")
  s_panel <- sh("\u4e13\u5bb6", "panel")
  s_auth <- sh("\u6743\u5a01\u7cfb\u6570", "authority")
  s_items <- sh("\u6761\u76ee", "items")
  s_rat <- sh("\u8bc4\u5206", "ratings")
  s_com <- sh("\u610f\u89c1", "comments")
  s_sugg <- sh("\u65b0\u589e\u5efa\u8bae", "suggestions")
  .stop_if(is.na(s_rat), "xlsx \u7f3a\u5c11\u300c\u8bc4\u5206\u300d\u5de5\u4f5c\u8868\u3002")

  read_sh <- function(nm) {
    if (is.na(nm)) return(NULL)
    openxlsx::read.xlsx(file, sheet = nm)
  }
  meta <- read_sh(s_meta)
  round <- NA_integer_
  if (!is.null(meta) && all(c("key", "value") %in% names(meta))) {
    round <- as.integer(meta$value[meta$key == "round"][1])
  }

  proto <- NULL
  pt <- read_sh(s_proto)
  if (!is.null(pt) && ncol(pt) >= 2L) {
    kv <- pt
    nms <- names(kv)
    key_col <- if ("parameter" %in% nms) "parameter" else nms[1]
    val_col <- if ("value" %in% nms) "value" else nms[2]
    preset_col <- if ("preset" %in% nms) {
      "preset"
    } else if ("\u521d\u59cb\u9501\u5b9a\u503c" %in% nms) {
      "\u521d\u59cb\u9501\u5b9a\u503c"
    } else {
      NA_character_
    }
    preset <- if (!is.na(preset_col)) kv[[preset_col]] else NULL
    proto <- tryCatch(
      .protocol_from_table(kv[[key_col]], kv[[val_col]], preset = preset),
      error = function(e) NULL
    )
  }

  panel <- read_sh(s_panel)
  items <- read_sh(s_items)
  ratings <- read_sh(s_rat)
  comments <- read_sh(s_com)
  suggestions <- .clean_suggestion_sheet(read_sh(s_sugg))
  authority_raw <- read_sh(s_auth)

  if (is.null(ratings) || !nrow(ratings)) {
    ratings <- data.frame(
      expert_id = character(), item_id = character(), round = integer(),
      score = numeric(), dimension = character(), unable = logical(),
      stringsAsFactors = FALSE
    )
  } else {
    .need_cols(ratings, c("expert_id", "item_id"), "\u8bc4\u5206\u8868")
    if (!"round" %in% names(ratings) || all(is.na(ratings$round))) {
      ratings$round <- if (!is.na(round)) round else 1L
    }
    if (!"score" %in% names(ratings)) ratings$score <- NA_real_
    ratings$score <- .parse_survey_score(ratings$score)
    if (!"unable" %in% names(ratings)) {
      ratings$unable <- FALSE
    } else {
      ratings$unable <- .parse_survey_logical(ratings$unable)
      ratings$unable[is.na(ratings$unable)] <- FALSE
    }
    ratings$expert_id <- as.character(ratings$expert_id)
    ratings$item_id <- as.character(ratings$item_id)
    if (!"dimension" %in% names(ratings)) ratings$dimension <- "overall"
    ratings <- ratings[, intersect(c("expert_id", "item_id", "round", "score",
                                     "dimension", "unable"), names(ratings)), drop = FALSE]
    keep <- !is.na(ratings$score) | ratings$unable
    ratings <- ratings[keep, , drop = FALSE]
  }
  key <- .rating_key(ratings)
  .stop_if(anyDuplicated(key) > 0L, "xlsx \u8bc4\u5206\u8868\u5b58\u5728\u91cd\u590d\u952e\u3002")

  if (!is.null(comments) && nrow(comments)) {
    if (!"comment" %in% names(comments) && "\u610f\u89c1" %in% names(comments)) {
      comments$comment <- comments[["\u610f\u89c1"]]
    }
    keep <- intersect(c("expert_id", "item_id", "round", "comment", "theme"), names(comments))
    comments <- comments[, keep, drop = FALSE]
    if (!"round" %in% names(comments)) comments$round <- if (!is.na(round)) round else 1L
  }

  if (!is.null(panel) && nrow(panel) && !"expert_id" %in% names(panel)) {
    panel <- NULL
  }
  if (is.null(panel) || !nrow(panel)) {
    panel <- data.frame(expert_id = unique(ratings$expert_id), stringsAsFactors = FALSE)
  }
  if (is.null(items) || !nrow(items) || !"item_id" %in% names(items)) {
    items <- data.frame(
      item_id = unique(ratings$item_id),
      item_text = unique(ratings$item_id),
      stringsAsFactors = FALSE
    )
  }
  if (!"item_text" %in% names(items)) items$item_text <- items$item_id

  list(
    round = unique(as.integer(ratings$round)),
    protocol = proto,
    panel = panel,
    items = items,
    ratings = ratings,
    comments = comments,
    suggestions = suggestions,
    authority_raw = authority_raw
  )
}

.clean_suggestion_sheet <- function(suggestions) {
  if (is.null(suggestions) || !nrow(suggestions) || !"raw_name" %in% names(suggestions)) {
    return(NULL)
  }
  suggestions$raw_name <- trimws(as.character(suggestions$raw_name))
  suggestions <- suggestions[!is.na(suggestions$raw_name) & nzchar(suggestions$raw_name), , drop = FALSE]
  if (!nrow(suggestions)) return(NULL)
  if (!"round" %in% names(suggestions) || all(is.na(suggestions$round))) suggestions$round <- 1L
  suggestions
}

.suggestion_sheet <- function(study, round, ids_all, prefill) {
  p <- study$protocol
  if (round != 1L || is.null(p$new_items) || !isTRUE(p$new_items$allow) || .is_open_round(p, round)) {
    return(NULL)
  }
  n <- as.integer(p$new_items$max_suggestions)
  sugg <- expand.grid(expert_id = ids_all, slot = seq_len(n), stringsAsFactors = FALSE)
  sugg <- sugg[order(sugg$expert_id, sugg$slot), , drop = FALSE]
  sugg$round <- round
  sugg$raw_name <- ""
  sugg$meaning <- ""
  sugg$rationale <- ""
  sugg$distinction <- ""
  sugg$evidence_note <- ""
  sugg$concept_id <- ""
  if (isTRUE(prefill) && !is.null(study$suggestions) && nrow(study$suggestions)) {
    src <- study$suggestions
    if ("round" %in% names(src)) src <- src[is.na(src$round) | src$round == round, , drop = FALSE]
    fields <- c("raw_name", "meaning", "rationale", "distinction", "evidence_note", "concept_id")
    for (eid in ids_all) {
      one <- src[as.character(src$expert_id) == eid, , drop = FALSE]
      if (!nrow(one)) next
      if ("slot" %in% names(one) && any(!is.na(one$slot))) {
        for (i in seq_len(nrow(one))) {
          w <- which(sugg$expert_id == eid & sugg$slot == suppressWarnings(as.integer(one$slot[i])))
          if (!length(w)) next
          for (nm in fields) {
            if (nm %in% names(one) && !is.na(one[[nm]][i])) sugg[[nm]][w[1]] <- as.character(one[[nm]][i])
          }
        }
      } else {
        w <- which(sugg$expert_id == eid)
        k <- min(length(w), nrow(one))
        for (nm in fields) {
          if (!nm %in% names(one)) next
          val <- as.character(one[[nm]][seq_len(k)])
          val[is.na(val)] <- ""
          sugg[[nm]][w[seq_len(k)]] <- val
        }
      }
    }
  }
  rownames(sugg) <- NULL
  sugg
}

.parse_survey_score <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "无法判断", "\u65e0\u6cd5\u5224\u65ad")] <- NA_character_
  as.numeric(x)
}

.parse_survey_logical <- function(x) {
  if (is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x)
  }
  x <- tolower(trimws(as.character(x)))
  out <- x %in% c("true", "t", "1", "yes", "y", "\u662f", "\u65e0\u6cd5\u5224\u65ad")
  out
}

.protocol_from_table <- function(keys, values, preset = NULL) {
  getv <- function(nm, default = NULL) {
    w <- which(keys == nm)[1]
    if (is.na(w)) return(default)
    values[w]
  }
  parse_rng <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
    as.numeric(strsplit(gsub("\u2013|-|/", ",", x), ",")[[1]])
  }
  num <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(as.character(x)) || identical(x, "NA")) return(NA_real_)
    as.numeric(x)
  }
  scenario <- getv("scenario", "clinical_consensus")
  args <- list(scenario = scenario)
  sm <- num(getv("scale_min"))
  sx <- num(getv("scale_max"))
  if (!is.na(sm)) args$scale_min <- sm
  if (!is.na(sx)) args$scale_max <- sx
  for (nm in c("high_pct", "basic_pct", "median_min", "iqr_max", "reject_pct",
               "comet_in_high", "comet_in_low_max", "comet_out_low", "comet_out_high_max",
               "rand_di_threshold", "max_rounds", "stability_class_pct",
               "stability_median_delta", "stability_high_pct_delta",
               "polarization_threshold", "cv_max", "ranking_topk",
               "mean_min", "w_min", "w_p_max", "mean_delta_max", "min_experts",
               "response_rate_min", "cr_min")) {
    v <- num(getv(nm))
    if (!is.na(v)) args[[nm]] <- v
  }
  lr <- parse_rng(getv("low_range"))
  if (length(lr) == 2L) args$low_range <- lr
  mr <- parse_rng(getv("mid_range"))
  if (length(mr) == 2L) args$mid_range <- mr
  hr <- parse_rng(getv("high_range"))
  if (length(hr) == 2L) args$high_range <- hr
  rd <- getv("rand_disagreement")
  if (!is.null(rd) && !is.na(rd) && nzchar(rd)) args$rand_disagreement <- rd
  for (nm in c("rand_round2_mode", "rand_boundary", "rand_purpose")) {
    v <- getv(nm)
    if (!is.null(v) && !is.na(v) && nzchar(as.character(v))) args[[nm]] <- as.character(v)
  }
  for (nm in c("rand_iprr", "rand_cfa", "rand_qtype", "rand_percent_high", "rand_percent_low")) {
    v <- num(getv(nm))
    if (!is.na(v)) args[[nm]] <- v
  }
  nec <- getv("rand_necessity")
  if (!is.null(nec) && !is.na(nec) && nzchar(as.character(nec))) {
    args$rand_necessity <- tolower(as.character(nec)) %in% c("true", "1", "yes", "\u662f")
  }
  rat <- getv("rationale")
  if (!is.null(rat) && !is.na(rat) && nzchar(rat)) args$rationale <- rat
  of <- getv("open_first")
  if (!is.null(of) && !is.na(of) && nzchar(as.character(of))) {
    args$open_first <- tolower(as.character(of)) %in% c("true", "1", "yes", "\u662f")
  }
  as_bool <- function(x) tolower(as.character(x)) %in% c("true", "1", "yes", "\u662f")
  for (nm in c("allow_new", "allow_single_with_evidence", "require_iteration")) {
    v <- getv(nm)
    if (!is.null(v) && !is.na(v) && nzchar(as.character(v))) args[[nm]] <- as_bool(v)
  }
  for (nm in c("min_independent", "max_suggestions")) {
    v <- num(getv(nm))
    if (!is.na(v)) args[[nm]] <- v
  }
  rp <- getv("review_panel")
  if (!is.null(rp) && !is.na(rp) && nzchar(as.character(rp))) args$review_panel <- as.character(rp)
  cs_txt <- getv("authority_cs")
  ca_txt <- getv("authority_ca")
  if (!is.null(cs_txt) && !is.null(ca_txt) &&
      !is.na(cs_txt) && !is.na(ca_txt) &&
      nzchar(as.character(cs_txt)) && nzchar(as.character(ca_txt))) {
    args$authority_scale <- .decode_authority_scale(as.character(cs_txt), as.character(ca_txt))
  }
  proto <- do.call(delphi_protocol, args)
  if (is.null(preset)) return(proto)
  base <- .protocol_from_table(keys, preset, preset = NULL)
  proto$study_preset <- base$study_preset
  proto$modified_from_preset <- .protocol_differs(proto, proto$study_preset)
  if (isTRUE(proto$modified_from_preset) && .rationale_missing(proto$rationale)) {
    .warn_revision_rationale()
  }
  proto
}

.rbind_fill_unique <- function(lst, key) {
  lst <- lst[!vapply(lst, is.null, logical(1))]
  lst <- lst[vapply(lst, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(lst)) return(NULL)
  out <- lst[[1]]
  if (length(lst) > 1L) {
    for (i in 2:length(lst)) out <- .rbind_fill(out, lst[[i]])
  }
  if (key %in% names(out)) out <- out[!duplicated(out[[key]]), , drop = FALSE]
  out
}

.md_to_docx <- function(md, file, title = "Delphi") {
  .need_pkg("officer")
  doc <- officer::read_docx()
  lines <- unlist(strsplit(md, "\n", fixed = TRUE))
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
      tbl <- .md_table_df(rows)
      if (!is.null(tbl) && nrow(tbl)) {
        doc <- officer::body_add_table(doc, tbl)
      }
      next
    }
    if (grepl("^### ", ln)) {
      doc <- officer::body_add_par(doc, sub("^### ", "", ln), style = "heading 3")
    } else if (grepl("^## ", ln)) {
      doc <- officer::body_add_par(doc, sub("^## ", "", ln), style = "heading 2")
    } else if (grepl("^# ", ln)) {
      doc <- officer::body_add_par(doc, sub("^# ", "", ln), style = "heading 1")
    } else if (grepl("^---+$", ln) || grepl("^\\*\\*\\*+$", ln)) {
      doc <- officer::body_add_par(doc, "")
    } else if (!nzchar(trimws(ln))) {
      doc <- officer::body_add_par(doc, "")
    } else {
      txt <- gsub("\\*\\*", "", ln)
      txt <- sub("^>\\s?", "", txt)
      doc <- officer::body_add_par(doc, txt, style = "Normal")
    }
    i <- i + 1L
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = file)
  invisible(file)
}

.md_table_df <- function(rows) {
  rows <- rows[!grepl("^\\|\\s*[-:]+", rows)]
  if (!length(rows)) return(NULL)
  parse_row <- function(r) {
    cells <- strsplit(gsub("^\\||\\|$", "", r), "\\|", perl = TRUE)[[1]]
    trimws(cells)
  }
  hdr <- parse_row(rows[1])
  body <- if (length(rows) > 1L) rows[-1] else character()
  if (!length(body)) {
    return(as.data.frame(matrix(character(), ncol = length(hdr),
                                dimnames = list(NULL, hdr)),
                         stringsAsFactors = FALSE))
  }
  mat <- do.call(rbind, lapply(body, function(r) {
    v <- parse_row(r)
    length(v) <- length(hdr)
    v
  }))
  colnames(mat) <- make.unique(hdr)
  as.data.frame(mat, stringsAsFactors = FALSE)
}

.q_output_stem <- function(file, round, format) {
  if (is.null(file) || !nzchar(file[1])) {
    return(file.path(tempdir(), sprintf("delphi_round%s", round)))
  }
  file <- file[1]
  if (dir.exists(file) || grepl("/$|\\\\$", file) ||
      (!grepl("\\.(md|html|docx|xlsx)$", file, ignore.case = TRUE) &&
       (format %in% c("both", "xlsx", "docx") && file.exists(file) && file.info(file)$isdir))) {
    dir.create(file, recursive = TRUE, showWarnings = FALSE)
    return(file.path(file, sprintf("round%s", round)))
  }
  if (grepl("\\.(md|html|docx|xlsx)$", file, ignore.case = TRUE)) {
    return(sub("\\.(md|html|docx|xlsx)$", "", file, ignore.case = TRUE))
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  file
}

.write_questionnaires <- function(docs, study, round, format, file, items,
                                  expert_id, result, split_experts, prefill) {
  written <- character()
  stem <- .q_output_stem(file, round, format)
  want_md <- format %in% c("markdown")
  want_html <- format %in% c("html")
  want_docx <- format %in% c("docx", "both")
  want_xlsx <- format %in% c("xlsx", "both")

  if (want_md || (is.null(file) && format == "markdown")) {
    if (is.null(file) && identical(format, "markdown")) return(docs)
    written <- c(written, .write_q_file(docs, if (is.null(file)) paste0(stem, ".md") else file, "markdown"))
  }
  if (want_html) {
    html_docs <- if (is.character(docs)) {
      nms <- names(docs)
      out <- vapply(seq_along(docs), function(i) {
        .md_to_html(docs[[i]], sprintf("\u7b2c %s \u8f6e Delphi \u00b7 %s", round, nms[i] %||% ""))
      }, character(1))
      names(out) <- nms
      out
    } else docs
    target <- if (!is.null(file) && grepl("\\.html$", file, ignore.case = TRUE)) file else paste0(stem, ".html")
    .write_q_file(html_docs, target, "html")
    written <- c(written, target)
  }
  if (want_docx) {
    if (isTRUE(split_experts) && length(docs) > 1L) {
      outdir <- if (!is.null(file) && (dir.exists(file) || !grepl("\\.", basename(file)))) {
        file
      } else dirname(paste0(stem, ".docx"))
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      nms <- names(docs)
      if (is.null(nms)) nms <- paste0("expert", seq_along(docs))
      for (i in seq_along(docs)) {
        fp <- file.path(outdir, paste0(nms[i], ".docx"))
        .md_to_docx(docs[[i]], fp)
        written <- c(written, fp)
      }
    } else {
      combined <- if (length(docs) == 1L) docs[[1]] else paste(docs, collapse = "\n\n---\n\n")
      fp <- if (!is.null(file) && grepl("\\.docx$", file, ignore.case = TRUE) && !isTRUE(split_experts)) {
        file
      } else paste0(stem, ".docx")
      .md_to_docx(combined, fp)
      written <- c(written, fp)
    }
  }
  if (want_xlsx) {
    if (isTRUE(split_experts) && !is.null(names(docs)) && length(docs) > 1L) {
      outdir <- if (!is.null(file) && (dir.exists(file) || !grepl("\\.", basename(file)))) {
        file
      } else dirname(paste0(stem, ".xlsx"))
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      for (eid in names(docs)) {
        fp <- file.path(outdir, paste0(eid, ".xlsx"))
        .write_survey_xlsx(study, round, fp, items, eid, result, prefill)
        written <- c(written, fp)
      }
    } else {
      fp <- if (!is.null(file) && grepl("\\.xlsx$", file, ignore.case = TRUE)) file else paste0(stem, ".xlsx")
      .write_survey_xlsx(study, round, fp, items, expert_id, result, prefill)
      written <- c(written, fp)
    }
  }
  invisible(unique(written))
}
