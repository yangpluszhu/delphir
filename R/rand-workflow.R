# Necessity, rounds, sensitivity, questionnaires, report driver, plots ------

.cohen_kappa <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- as.character(x[ok])
  y <- as.character(y[ok])
  if (length(x) < 2L) return(NA_real_)
  lev <- union(unique(x), unique(y))
  tab <- table(factor(x, lev), factor(y, lev))
  n <- sum(tab)
  if (n == 0L) return(NA_real_)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (!is.finite(pe) || abs(1 - pe) < 1e-12) return(NA_real_)
  (po - pe) / (1 - pe)
}

.rand_round2_text <- function(mode) {
  switch(
    mode %||% "in_person",
    in_person = "\u7b2c 2 \u8f6e\u662f\u9762\u5bf9\u9762\u4f1a\u8bae\u3002\u8bf7\u5148\u9605\u8bfb\u533f\u540d\u53cd\u9988\uff0c\u8ba8\u8bba\u540e\u518d\u72ec\u7acb\u8bc4\u5206\u3002\u8ba8\u8bba\u4e0d\u80fd\u4ee3\u66ff\u8bc4\u5206\u3002",
    online = "\u7b2c 2 \u8f6e\u662f\u7ebf\u4e0a\u4f1a\u8bae\u3002\u8bf7\u5148\u9605\u8bfb\u53cd\u9988\uff0c\u8ba8\u8bba\u540e\u518d\u72ec\u7acb\u8bc4\u5206\u3002\u8fd9\u662f\u5bf9\u7ecf\u5178\u9762\u5bf9\u9762\u4f1a\u8bae\u7684\u53d8\u901a\uff0c\u62a5\u544a\u4e2d\u4f1a\u8bf4\u660e\u3002",
    mail = "\u7b2c 2 \u8f6e\u662f\u518d\u6b21\u51fd\u8be2\uff0c\u6ca1\u6709\u4f1a\u8bae\u3002\u8bf7\u9605\u8bfb\u53cd\u9988\u540e\u72ec\u7acb\u590d\u8bc4\u3002\u8fd9\u662f\u5bf9\u7ecf\u5178 RAM \u7684\u53d8\u901a\uff0c\u62a5\u544a\u4e2d\u4f1a\u8bf4\u660e\u3002",
    "\u8bf7\u5728\u9605\u8bfb\u53cd\u9988\u540e\u72ec\u7acb\u590d\u8bc4\u3002"
  )
}

.rand_anchor_text <- function(kind = c("appropriateness", "necessity", "recommendation")) {
  kind <- match.arg(kind)
  switch(
    kind,
    appropriateness = "1 \u8868\u793a\u9884\u671f\u5371\u5bb3\u8fdc\u5927\u4e8e\u83b7\u76ca\uff0c5 \u8868\u793a\u83b7\u76ca\u4e0e\u5371\u5bb3\u5927\u81f4\u76f8\u5f53\u6216\u65e0\u6cd5\u5224\u65ad\uff0c9 \u8868\u793a\u9884\u671f\u83b7\u76ca\u8fdc\u5927\u4e8e\u5371\u5bb3\u3002",
    necessity = "\u4ec5\u8bc4\u4ef7\u5df2\u5224\u4e3a\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u7684\u6761\u76ee\u30021 \u8868\u793a\u663e\u7136\u4e0d\u5fc5\u8981\uff0c9 \u8868\u793a\u663e\u7136\u5fc5\u8981\u3002\u4e0d\u63d0\u4f9b\u8be5\u9879\u63aa\u65bd\u5373\u5c5e\u4e0d\u5f53\u533b\u7597\u3001\u6709\u5408\u7406\u83b7\u76ca\u673a\u4f1a\u3001\u83b7\u76ca\u5e45\u5ea6\u4e0d\u5c0f\uff0c\u662f\u8bc4\u5206\u65f6\u7684\u6982\u5ff5\u6807\u51c6\uff0c\u4e0d\u662f\u7b97\u6cd5\u8f93\u5165\u3002",
    recommendation = "\u672c\u573a\u666f\u4e3a RAM \u4fee\u6539\u7248\uff1a\u5bf9\u63a8\u8350\u610f\u89c1\u800c\u4e0d\u662f\u6307\u5f81\u00d7\u5e72\u9884\u77e9\u9635\u8bc4\u5206\u30021 \u8868\u793a\u5f3a\u70c8\u53cd\u5bf9\uff0c9 \u8868\u793a\u5f3a\u70c8\u540c\u610f\u3002\u5224\u5b9a\u7528\u9884\u5148\u9501\u5b9a\u7684\u767e\u5206\u6bd4\u89c4\u5219\uff0c\u4e0d\u5957\u7528 IPRAS \u5e38\u6570\u3002"
  )
}

#' 必要性分类（Kahan 等，1994）
#'
#' 只对最终轮已被判为「适宜且无分歧」的条目做必要性评分。必要性中位数
#' 至少为 7 且按同一分歧规则无分歧时，记为必要；否则记为适宜但非必要。
#' 未达适宜的条目即使有分数，也不给出必要性结论。
#'
#' @param study [delphi_study()] 对象。必要性评分的 `dimension` 须含「必要」。
#' @param appropriateness 可选的 [classify_rand()] 结果。缺省时用适宜性最后一轮。
#' @param necessity_round 必要性评分所在轮次。缺省为必要性评分的最大轮次。
#' @param appropriateness_round 对照的适宜性轮次。缺省为适宜性最后一轮。
#'
#' @return 数据框。`eligible` 为是否允许评价必要性；`necessity_class` 为
#'   `necessary`、`appropriate_not_necessary` 或 `NA`。
#'
#' @section 输入输出:
#' **输入**适宜性评分与必要性评分放在同一研究里，用 `dimension` 区分。
#' 必要性通常是单独一轮邮寄评分，不是第三轮适宜性评分。
#'
#' **输出**每个适宜性条目一行，并保留必要性中位数、IPR、DI 和分歧标记。
#'
#' @examples
#' nec <- simulate_rand_necessity()
#' classify_necessity(nec)
#'
#' @export
classify_necessity <- function(study, appropriateness = NULL,
                               necessity_round = NULL,
                               appropriateness_round = NULL) {
  study <- .match_study(study)
  settings <- .rand_settings(study$protocol)
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  nec_all <- r[grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  app_r <- r[!grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  .stop_if(!nrow(app_r), "\u6ca1\u6709\u9002\u5b9c\u6027\u8bc4\u5206\uff0c\u65e0\u6cd5\u5224\u65ad\u54ea\u4e9b\u6761\u76ee\u53ef\u4ee5\u8bc4\u5fc5\u8981\u6027\u3002")
  if (is.null(appropriateness_round)) appropriateness_round <- max(app_r$round)
  if (is.null(appropriateness)) {
    appropriateness <- classify_rand(study, round = appropriateness_round)
  } else {
    appropriateness <- appropriateness[appropriateness$round == appropriateness_round, , drop = FALSE]
  }
  .stop_if(!nrow(appropriateness), "\u9002\u5b9c\u6027\u5206\u7c7b\u4e3a\u7a7a\u3002")
  if (!is.null(necessity_round)) {
    nec_all <- nec_all[nec_all$round == necessity_round, , drop = FALSE]
  } else if (nrow(nec_all)) {
    necessity_round <- max(nec_all$round)
    nec_all <- nec_all[nec_all$round == necessity_round, , drop = FALSE]
  }

  rows <- lapply(seq_len(nrow(appropriateness)), function(i) {
    item <- appropriateness[i, , drop = FALSE]
    eligible <- identical(as.character(item$class[1]), "appropriate") &&
      !isTRUE(item$disagreement[1])
    sub <- nec_all[nec_all$item_id == item$item_id[1], , drop = FALSE]
    xs <- if (nrow(sub)) .valid_scores(sub$score, sub$unable) else numeric()
    d <- if (length(xs)) {
      rand_disagreement(
        xs, method = settings$rule, di_threshold = settings$di_threshold,
        warn = FALSE, low = settings$low, mid = settings$mid, high = settings$high,
        iprr = settings$iprr, cfa = settings$cfa, qtype = settings$qtype,
        percent_high = settings$percent_high, percent_low = settings$percent_low,
        boundary = settings$boundary, scale = settings$scale
      )
    } else {
      .rand_empty_disagreement()
    }
    if (!eligible && length(xs)) {
      warning(
        sprintf("\u6761\u76ee %s \u672a\u8fbe\u300c\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u300d\uff0c\u5df2\u6709\u7684\u5fc5\u8981\u6027\u5206\u6570\u4e0d\u53c2\u4e0e\u5206\u7c7b\u3002", item$item_id[1]),
        call. = FALSE
      )
    }
    if (!eligible) {
      nec_class <- NA_character_
      nec_zh <- NA_character_
      note <- "\u672a\u8fbe\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\uff0c\u4e0d\u8bc4\u5fc5\u8981\u6027"
    } else if (!length(xs)) {
      nec_class <- NA_character_
      nec_zh <- "\u5c1a\u672a\u8bc4\u5206"
      note <- "\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\uff0c\u4f46\u5c1a\u672a\u505a\u5fc5\u8981\u6027\u8bc4\u5206"
    } else if (!is.na(d$median) && d$median >= 7 && !isTRUE(d$disagreement)) {
      nec_class <- "necessary"
      nec_zh <- "\u5fc5\u8981"
      note <- "\u5fc5\u8981\u6027\u4e2d\u4f4d\u6570 \u2265 7 \u4e14\u65e0\u5206\u6b67"
    } else {
      nec_class <- "appropriate_not_necessary"
      nec_zh <- "\u9002\u5b9c\u4f46\u975e\u5fc5\u8981"
      note <- "\u5fc5\u8981\u6027\u4e2d\u4f4d\u6570 < 7\uff0c\u6216\u5b58\u5728\u5206\u6b67"
    }
    data.frame(
      item_id = as.character(item$item_id[1]),
      item_text = if ("item_text" %in% names(item)) as.character(item$item_text[1]) else NA_character_,
      appropriateness_round = appropriateness_round,
      appropriateness_class = as.character(item$class[1]),
      appropriateness_zh = as.character(item$class_zh[1]),
      eligible = eligible,
      necessity_round = if (length(xs)) necessity_round else NA_integer_,
      necessity_class = nec_class,
      necessity_zh = nec_zh,
      note = note,
      n_valid = d$n_valid,
      median = if (length(xs)) d$median else NA_real_,
      ipr = if (length(xs)) d$ipr else NA_real_,
      ipras = if (length(xs)) d$ipras else NA_real_,
      di = if (length(xs)) d$di else NA_real_,
      disagreement = if (length(xs)) d$disagreement else NA,
      agreement_status = if (length(xs)) d$agreement_status else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "rule") <- settings$rule
  .add_class(out, "rand_necessity")
}

#' 比较 RAND 各轮中位数、IPR 与分类
#'
#' 适宜性条目在相邻轮之间是否改变分类。Wilcoxon 符号秩检验只作探索性描述，
#' 不作为是否适宜的判定。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。
#' @param dimension 适宜性维度。缺省时去掉名称中含「必要」的维度。
#'
#' @return 列表：`items` 为条目级变化，`group` 为各轮构成，`note` 说明
#'   Wilcoxon 的探索性性质。
#'
#' @section 输入输出:
#' **输入**至少两轮适宜性评分。
#'
#' **输出** `items$changed` 为分类是否变化。`group$mean_ipr` 是该轮 IPR 的均数。
#'
#' @examples
#' cmp <- rand_compare_rounds(simulate_ram_classic())
#' cmp$group
#'
#' @export
rand_compare_rounds <- function(study, dimension = NULL) {
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[!grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  if (!is.null(dimension)) r <- r[r$dimension %in% dimension, , drop = FALSE]
  .stop_if(!nrow(r), "\u6ca1\u6709\u53ef\u6bd4\u8f83\u7684\u9002\u5b9c\u6027\u8bc4\u5206\u3002")
  rounds <- .unique_sorted(r$round)
  cls <- do.call(rbind, lapply(rounds, function(rnd) classify_rand(study, round = rnd)))
  rownames(cls) <- NULL
  group <- do.call(rbind, lapply(rounds, function(rnd) {
    sub <- cls[cls$round == rnd, , drop = FALSE]
    data.frame(
      round = rnd,
      n_items = nrow(sub),
      n_appropriate = sum(sub$class == "appropriate", na.rm = TRUE),
      n_inappropriate = sum(sub$class == "inappropriate", na.rm = TRUE),
      n_uncertain_equipoise = sum(sub$class_detail == "uncertain_equipoise", na.rm = TRUE),
      n_uncertain_disagreement = sum(sub$class_detail == "uncertain_disagreement", na.rm = TRUE),
      mean_ipr = mean(sub$ipr, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  if (length(rounds) < 2L) {
    return(structure(
      list(items = cls, group = group, note = "\u53ea\u6709\u4e00\u8f6e\uff0c\u65e0\u6cd5\u6bd4\u8f83\u53d8\u5316\u3002"),
      class = "rand_round_comparison"
    ))
  }
  pieces <- lapply(seq_len(length(rounds) - 1L), function(i) {
    a <- cls[cls$round == rounds[i], , drop = FALSE]
    b <- cls[cls$round == rounds[i + 1L], , drop = FALSE]
    m <- merge(
      a[, c("item_id", "dimension", "median", "ipr", "class", "class_zh", "class_detail")],
      b[, c("item_id", "dimension", "median", "ipr", "class", "class_zh", "class_detail")],
      by = c("item_id", "dimension"), suffixes = c("_from", "_to")
    )
    m$from_round <- rounds[i]
    m$to_round <- rounds[i + 1L]
    m$changed <- !is.na(m$class_from) & !is.na(m$class_to) & m$class_from != m$class_to
    m$wilcoxon_p <- vapply(seq_len(nrow(m)), function(j) {
      x <- r[r$item_id == m$item_id[j] & r$dimension == m$dimension[j] & r$round == rounds[i], , drop = FALSE]
      y <- r[r$item_id == m$item_id[j] & r$dimension == m$dimension[j] & r$round == rounds[i + 1L], , drop = FALSE]
      both <- intersect(x$expert_id[!x$unable & !is.na(x$score)], y$expert_id[!y$unable & !is.na(y$score)])
      if (length(both) < 5L) return(NA_real_)
      xs <- x$score[match(both, x$expert_id)]
      ys <- y$score[match(both, y$expert_id)]
      if (all(xs == ys)) return(NA_real_)
      out <- tryCatch(stats::wilcox.test(xs, ys, paired = TRUE, exact = FALSE)$p.value, error = function(e) NA_real_)
      as.numeric(out)
    }, numeric(1))
    m
  })
  items <- do.call(rbind, pieces)
  rownames(items) <- NULL
  structure(
    list(
      items = items,
      group = group,
      note = "Wilcoxon \u7b26\u53f7\u79e9\u68c0\u9a8c\u53ea\u63cf\u8ff0\u5404\u6761\u76ee\u8bc4\u5206\u662f\u5426\u6574\u4f53\u79fb\u52a8\uff0c\u4e0d\u6539\u53d8\u9002\u5b9c\u6027\u5206\u7c7b\u3002"
    ),
    class = "rand_round_comparison"
  )
}

#' 规则敏感性与逐一剔除专家
#'
#' 在表 4、IPRAS、9 人严格定义和宽松定义下重算分类，并给出无加权 Cohen's kappa。
#' 逐一剔除专家只描述分类是否翻转，不产生新的正式规则。
#'
#' @param study [delphi_study()] 或分析结果。
#' @param round 轮次，默认适宜性最后一轮。
#' @param rules 要比较的规则。
#' @param loo 是否逐一剔除专家。人数很多时会变慢。
#'
#' @return 列表：`by_item` 为各规则分类，`kappa` 为规则两两一致性，
#'   `loo` 为每位专家被剔除后分类翻转的条目数，`note` 为方法学说明。
#'
#' @section 输入输出:
#' **输入**与 [classify_rand()] 相同。`method` 不改写方案里锁定的规则。
#'
#' **输出** `by_item` 每个条目一行。`kappa` 接近 1 表示两套规则分类几乎相同。
#'
#' @examples
#' sens <- rand_sensitivity(simulate_ram_classic(), round = 2, loo = FALSE)
#' sens$kappa
#'
#' @export
rand_sensitivity <- function(study, round = NULL,
                             rules = c("table4", "ipras", "classic_relaxed", "classic_strict"),
                             loo = TRUE) {
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  settings <- .rand_settings(study$protocol)
  rules <- unique(as.character(rules))
  r0 <- .ensure_dimension(.ensure_unable(study$ratings))
  r0 <- r0[!grepl("\u5fc5\u8981", r0$dimension), , drop = FALSE]
  .stop_if(!nrow(r0), "\u6ca1\u6709\u9002\u5b9c\u6027\u8bc4\u5206\u3002")
  if (is.null(round)) round <- max(r0$round)
  pieces <- list()
  skipped <- character()
  for (rule in rules) {
    one <- tryCatch(
      classify_rand(study, round = round, method = rule),
      error = function(e) {
        skipped <<- c(skipped, paste0(rule, ": ", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(one)) {
      one$rule <- rule
      pieces[[rule]] <- one[, c("item_id", "dimension", "class", "class_detail", "disagreement")]
    }
  }
  .stop_if(!length(pieces), "\u6ca1\u6709\u4efb\u4f55\u89c4\u5219\u80fd\u5b8c\u6210\u5206\u7c7b\u3002")
  by_item <- pieces[[1]][, c("item_id", "dimension")]
  for (rule in names(pieces)) {
    col <- paste0("class_", rule)
    by_item[[col]] <- pieces[[rule]]$class[match(
      paste(by_item$item_id, by_item$dimension),
      paste(pieces[[rule]]$item_id, pieces[[rule]]$dimension)
    )]
  }
  pairs <- utils::combn(names(pieces), 2, simplify = FALSE)
  kappa <- do.call(rbind, lapply(pairs, function(pr) {
    data.frame(
      rule_a = pr[1],
      rule_b = pr[2],
      kappa = .cohen_kappa(pieces[[pr[1]]]$class, pieces[[pr[2]]]$class),
      n = nrow(pieces[[pr[1]]]),
      stringsAsFactors = FALSE
    )
  }))
  loo_tab <- NULL
  if (isTRUE(loo)) {
    base <- classify_rand(study, round = round, method = settings$rule)
    experts <- unique(as.character(r0$expert_id[r0$round == round]))
    loo_tab <- do.call(rbind, lapply(experts, function(eid) {
      st <- study
      drop <- st$ratings$round == round & as.character(st$ratings$expert_id) == eid &
        !grepl("\u5fc5\u8981", .ensure_dimension(st$ratings)$dimension)
      st$ratings <- st$ratings[!drop, , drop = FALSE]
      alt <- suppressWarnings(classify_rand(st, round = round, method = settings$rule))
      key <- paste(base$item_id, base$dimension)
      alt_class <- alt$class[match(key, paste(alt$item_id, alt$dimension))]
      changed <- !is.na(base$class) & !is.na(alt_class) & base$class != alt_class
      data.frame(
        expert_id = eid,
        n_items = sum(!is.na(base$class)),
        n_changed = sum(changed),
        changed_items = paste(base$item_id[changed], collapse = ", "),
        stringsAsFactors = FALSE
      )
    }))
    rownames(loo_tab) <- NULL
  }
  structure(
    list(
      round = round,
      locked_rule = settings$rule,
      by_item = by_item,
      kappa = kappa,
      loo = loo_tab,
      skipped = skipped,
      note = "\u654f\u611f\u6027\u5206\u6790\u4e0d\u66ff\u6362\u9501\u5b9a\u89c4\u5219\u3002\u03ba \u63cf\u8ff0\u4e24\u5957\u89c4\u5219\u7684\u5206\u7c7b\u4e00\u81f4\u7a0b\u5ea6\u3002\u9010\u4e00\u5254\u9664\u53ea\u8bf4\u660e\u7ed3\u8bba\u662f\u5426\u4f9d\u8d56\u5355\u4e2a\u4e13\u5bb6\u3002"
    ),
    class = "rand_sensitivity"
  )
}

.rand_kind <- function(protocol, necessity = FALSE) {
  if (isTRUE(necessity)) return("necessity")
  if (identical(.rand_settings(protocol)$purpose, "recommendation")) return("recommendation")
  "appropriateness"
}

.rand_item_heading <- function(items, iid) {
  row <- items[as.character(items$item_id) == iid, , drop = FALSE]
  if (!nrow(row)) return(iid)
  extra <- character()
  if ("indication" %in% names(row) && !is.na(row$indication[1]) && nzchar(row$indication[1])) {
    extra <- c(extra, paste0("- \u4e34\u5e8a\u60c5\u5883\uff1a", row$indication[1]))
  }
  if ("procedure" %in% names(row) && !is.na(row$procedure[1]) && nzchar(row$procedure[1])) {
    extra <- c(extra, paste0("- \u5e72\u9884\uff1a", row$procedure[1]))
  }
  if ("evidence_level" %in% names(row) && !is.na(row$evidence_level[1]) && nzchar(as.character(row$evidence_level[1]))) {
    extra <- c(extra, paste0("- \u8bc1\u636e\u51c6\u5907\uff1a", row$evidence_level[1]))
  }
  paste0("**", row$item_text[1], "**\n\n", if (length(extra)) paste(c(extra, ""), collapse = "\n") else "")
}

.questionnaire_rand_round1 <- function(study) {
  p <- study$protocol
  meta <- study$meta
  kind <- .rand_kind(p, FALSE)
  dims <- .questionnaire_dimensions(study, 1L)
  blocks <- apply(study$items, 1, function(row) {
    likert <- paste(vapply(dims, function(d) .likert_line(p, d), character(1)), collapse = "\n")
    paste0(
      "### ", row[["item_id"]], "\n\n",
      .rand_item_heading(study$items, row[["item_id"]]),
      likert, "\n",
      "**\u610f\u89c1\uff08\u53ef\u7a7a\uff0c\u4e0d\u8981\u5199\u59d3\u540d\uff09\uff1a** ________________________________\n"
    )
  })
  paste0(
    "# \u7b2c 1 \u8f6e RAND \u72ec\u7acb\u8bc4\u5206\n\n",
    "**\u7814\u7a76\u9898\u76ee\uff1a** ", meta$title %||% "RAND/UCLA \u9002\u5b9c\u6027\u8bc4\u4ef7", "\n\n",
    "**\u76ee\u7684\uff1a** ", meta$objective %||% "\u5224\u65ad\u7279\u5b9a\u60c5\u5883\u4e0b\u63aa\u65bd\u662f\u5426\u9002\u5b9c\uff0c\u800c\u4e0d\u662f\u5f3a\u8feb\u5f62\u6210\u5355\u4e00\u7ed3\u8bba\u3002", "\n\n",
    "**\u76ee\u6807\u4f7f\u7528\u8005\uff1a** ", meta$users %||% "\u672a\u8bf4\u660e", "\n\n",
    "**\u5730\u7406\u8303\u56f4\uff1a** ", meta$geography %||% "\u672a\u8bf4\u660e", "\n\n",
    "> \u672c\u8f6e\u72ec\u7acb\u3001\u533f\u540d\u5b8c\u6210\uff0c\u4e0d\u8981\u4e0e\u5176\u4ed6\u4e13\u5bb6\u8ba8\u8bba\u3002",
    .rand_anchor_text(kind),
    "\u65e0\u6cd5\u5224\u65ad\u8bf7\u52fe\u9009\uff0c\u4e0d\u8981\u586b 5 \u5206\u5145\u6570\u3002\u65e0\u6cd5\u5224\u65ad\u4e0d\u8fdb\u5165\u4e2d\u4f4d\u6570\u548c\u5206\u6b67\u5224\u5b9a\u3002\n\n",
    "## \u5df2\u9501\u5b9a\u7684\u5224\u5b9a\u89c4\u5219\n\n",
    .protocol_rules_md(p), "\n\n",
    "**\u4e0d\u786e\u5b9a\u548c\u5206\u6b67\u90fd\u662f\u6b63\u5f0f\u7ed3\u679c\uff0c\u4e0d\u662f\u8c03\u67e5\u5931\u8d25\u3002** \u89c4\u5219\u4e0d\u4f1a\u6839\u636e\u7ed3\u679c\u56de\u6539\u3002\n\n",
    "## \u8865\u5145\uff1a\u6743\u5a01\u7a0b\u5ea6\uff08\u53ef\u9009\uff0c\u4e0d\u53c2\u4e0e\u9002\u5b9c\u6027\u5224\u5b9a\uff09\n\n",
    "\u82e5\u56e2\u961f\u9700\u8981\u62a5\u544a\u56fd\u5185\u5e38\u7528\u7684\u719f\u6089\u7a0b\u5ea6\u4e0e\u5224\u65ad\u4f9d\u636e\uff0c\u8bf7\u5728\u5de5\u4f5c\u7c3f\u300c\u6743\u5a01\u7cfb\u6570\u300d\u8868\u6574\u4f53\u81ea\u8bc4\u4e00\u6b21\u3002",
    "Cr \u4e0d\u52a0\u6743\u3001\u4e0d\u66ff\u4ee3\u4e2d\u4f4d\u6570\u4e0e\u5206\u6b67\u5224\u5b9a\u3002\n\n",
    "## \u6761\u76ee\n\n",
    paste(blocks, collapse = "\n")
  )
}

.questionnaire_rand_followup <- function(study, round, expert_ids = NULL) {
  p <- study$protocol
  settings <- .rand_settings(p)
  src <- as.integer(round) - 1L
  cls <- classify_rand(study, round = src)
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[r$round == src & !grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  if (is.null(expert_ids)) expert_ids <- unique(as.character(study$panel$expert_id))
  comments <- study$comments
  docs <- vapply(expert_ids, function(eid) {
    blocks <- vapply(cls$item_id, function(iid) {
      row <- cls[cls$item_id == iid, , drop = FALSE][1, ]
      own <- r[r$expert_id == eid & r$item_id == iid, , drop = FALSE]
      own_txt <- if (!nrow(own)) {
        "\u2014"
      } else if (isTRUE(own$unable[1]) || is.na(own$score[1])) {
        "\u65e0\u6cd5\u5224\u65ad"
      } else {
        as.character(own$score[1])
      }
      note <- ""
      if (!is.null(comments) && nrow(comments)) {
        cc <- comments[comments$round == src & comments$item_id == iid, , drop = FALSE]
        if (nrow(cc)) note <- paste0("**\u533f\u540d\u610f\u89c1\uff1a** ", paste(utils::head(unique(cc$comment), 3), collapse = " | "), "\n\n")
      }
      paste0(
        "### ", iid, "\n\n",
        .rand_item_heading(study$items, iid),
        sprintf("| \u60a8\u7684\u4e0a\u8f6e\u8bc4\u5206 | \u4e2d\u4f4d\u6570 | 1\u20133 | 4\u20136 | 7\u20139 | \u5206\u6b67 | \u5206\u7c7b |\n"),
        "|---|---|---|---|---|---|---|\n",
        sprintf("| %s | %s | %s | %s | %s | %s | %s |\n\n",
                own_txt,
                ifelse(is.na(row$median), "\u2014", row$median),
                row$n_low, row$n_mid, row$n_high,
                if (isTRUE(row$disagreement)) "\u662f" else "\u5426",
                row$class_zh),
        note,
        .likert_line(p, "\u672c\u8f6e\u590d\u8bc4"), "\n",
        "**\u672c\u8f6e\u610f\u89c1\uff08\u53ef\u7a7a\uff09\uff1a** ________________________________\n"
      )
    }, character(1))
    paste0(
      sprintf("# \u7b2c %s \u8f6e RAND \u53cd\u9988\u540e\u590d\u8bc4\n\n", round),
      "**\u7814\u7a76\u9898\u76ee\uff1a** ", study$meta$title %||% "RAND/UCLA", "\n\n",
      "**\u672c\u5377\u4e13\u5bb6\u7f16\u53f7\uff1a** ", eid, "\n\n",
      .rand_round2_text(settings$round2_mode), "\n\n",
      "\u4e0b\u8868\u53ea\u663e\u793a\u7ec4\u5185\u5206\u5e03\u548c\u60a8\u672c\u4eba\u7684\u4e0a\u8f6e\u8bc4\u5206\uff0c\u4e0d\u51fa\u73b0\u5176\u4ed6\u4e13\u5bb6\u59d3\u540d\u3002",
      "\u8bf7\u5bf9**\u5168\u90e8**\u6761\u76ee\u518d\u6b21\u72ec\u7acb\u8bc4\u5206\u3002\u5df2\u5224\u4e3a\u9002\u5b9c\u6216\u4e0d\u9002\u5b9c\u7684\u6761\u76ee\u4e5f\u8981\u590d\u8bc4\uff0c\u4e0d\u50cf Delphi \u90a3\u6837\u5254\u9664\u3002\n\n",
      .rand_anchor_text(.rand_kind(p, FALSE)), "\n\n",
      paste(blocks, collapse = "\n")
    )
  }, character(1))
  names(docs) <- expert_ids
  docs
}

.questionnaire_rand_necessity <- function(study, expert_ids = NULL) {
  nec <- classify_necessity(study)
  eligible <- nec$item_id[isTRUE(nec$eligible) | nec$eligible %in% TRUE]
  .stop_if(!length(eligible), "\u6ca1\u6709\u300c\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u300d\u7684\u6761\u76ee\uff0c\u4e0d\u80fd\u751f\u6210\u5fc5\u8981\u6027\u95ee\u5377\u3002")
  p <- study$protocol
  if (is.null(expert_ids)) expert_ids <- unique(as.character(study$panel$expert_id))
  blocks <- vapply(eligible, function(iid) {
    row <- nec[nec$item_id == iid, , drop = FALSE][1, ]
    paste0(
      "### ", iid, "\n\n",
      .rand_item_heading(study$items, iid),
      sprintf("\u9002\u5b9c\u6027\u7ed3\u8bba\uff1a%s\u3002\u672c\u9879\u53ea\u8bc4\u4ef7\u5fc5\u8981\u6027\u3002\n\n", row$appropriateness_zh),
      .likert_line(p, "\u5fc5\u8981\u6027"), "\n",
      "**\u610f\u89c1\uff08\u53ef\u7a7a\uff09\uff1a** ________________________________\n"
    )
  }, character(1))
  docs <- vapply(expert_ids, function(eid) {
    paste0(
      "# \u5fc5\u8981\u6027\u8bc4\u4ef7\uff08\u4e0d\u662f\u7b2c\u4e09\u8f6e\u9002\u5b9c\u6027\u8bc4\u5206\uff09\n\n",
      "**\u7814\u7a76\u9898\u76ee\uff1a** ", study$meta$title %||% "RAND/UCLA", "\n\n",
      "**\u672c\u5377\u4e13\u5bb6\u7f16\u53f7\uff1a** ", eid, "\n\n",
      .rand_anchor_text("necessity"), "\n\n",
      "\u672a\u5217\u5165\u672c\u5377\u7684\u6761\u76ee\u4e0d\u662f\u9057\u6f0f\uff1a\u5b83\u4eec\u672a\u8fbe\u5230\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\uff0c\u6309 Kahan \u7b49\uff081994\uff09\u4e0d\u505a\u5fc5\u8981\u6027\u5224\u5b9a\u3002\n\n",
      paste(blocks, collapse = "\n")
    )
  }, character(1))
  names(docs) <- expert_ids
  docs
}

#' 按锁定的 RAND 方案生成两轮或必要性问卷
#'
#' 第 1 轮是独立评分，不展示他人分数。第 2 轮给每位专家本人上轮分数、
#' 1–3 / 4–6 / 7–9 频数、中位数和分歧标记，并要求对全部条目复评。
#' `round = "necessity"` 只列入适宜且无分歧的条目。
#'
#' @param study [delphi_study()]，方案场景须为 `rand_ucla`。
#' @param round `1`、`2` 或 `"necessity"`。
#' @param expert_id 要生成的专家。缺省为全体。
#' @param format `"docx"`、`"xlsx"` 或 `"both"`。`both` 同时写两种文件。
#' @param file 文件或目录。`both` 时用同一主文件名写出 `.docx` 与 `.xlsx`。
#' @param prefill 若研究里已经有该轮分数，是否写入 xlsx。默认否。
#'
#' @return 不可见的写出路径。
#'
#' @section 输入输出:
#' **输入**条目表建议含 `item_id`、`item_text`，适宜性场景还可含
#' `indication` 与 `procedure`。专家表须有唯一 `expert_id`。
#'
#' **输出** docx 供发放，xlsx 供回收后用 [read_survey_xlsx()] 读回。
#' xlsx 工作表与 [delphi_questionnaire()] 相同：说明、方案、专家、条目、评分、意见。
#' 第 2 轮评分表含 `own_prev`、`class_zh` 等反馈列。必要性问卷的 `dimension` 为「必要性」。
#'
#' @examples
#' \donttest{
#' ram <- simulate_ram_classic()
#' d <- tempfile("ram-")
#' dir.create(d)
#' rand_questionnaire(ram, round = 1, format = "both", file = d)
#' }
#'
#' @export
rand_questionnaire <- function(study, round = 1,
                               expert_id = NULL,
                               format = c("both", "docx", "xlsx"),
                               file = NULL,
                               prefill = FALSE) {
  format <- match.arg(format)
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  .stop_if(!identical(study$protocol$scenario, "rand_ucla"),
           "rand_questionnaire() \u53ea\u7528\u4e8e\u573a\u666f rand_ucla\u3002")
  necessity <- identical(as.character(round), "necessity")
  if (!necessity) {
    round <- as.integer(round)[1]
    .stop_if(is.na(round) || round < 1L, "round \u5fc5\u987b\u662f 1\u30012 \u6216 \"necessity\"\u3002")
    if (round >= 3L) {
      warning(
        "\u7ecf\u5178 RAM \u4e0d\u628a\u7b2c 3 \u8f6e\u53ca\u4ee5\u4e0a\u7684\u9002\u5b9c\u6027\u8bc4\u5206\u5f53\u4f5c\u6807\u51c6\u6d41\u7a0b\u3002\u82e5\u8981\u8bc4\u5fc5\u8981\u6027\uff0c\u8bf7\u4f7f\u7528 round = \"necessity\"\u3002",
        call. = FALSE
      )
    }
  }
  ids <- if (is.null(expert_id)) unique(as.character(study$panel$expert_id)) else as.character(expert_id)
  if (necessity) {
    docs <- .questionnaire_rand_necessity(study, ids)
    nec <- classify_necessity(study)
    eligible <- nec$item_id[nec$eligible %in% TRUE]
    study2 <- study
    study2$items <- study2$items[study2$items$item_id %in% eligible, , drop = FALSE]
    attr(study2, "rand_force_dimension") <- "\u5fc5\u8981\u6027"
    sheet_round <- if (nrow(study$ratings)) max(study$ratings$round[!grepl("\u5fc5\u8981", study$ratings$dimension)]) + 1L else 3L
    if (!is.finite(sheet_round)) sheet_round <- 3L
    attr(study2, "rand_force_round") <- as.integer(sheet_round)
    items_mode <- "all"
  } else if (round <= 1L) {
    docs <- .questionnaire_rand_round1(study)
    names(docs) <- "round1"
    study2 <- study
    attr(study2, "rand_force_dimension") <- if (identical(.rand_settings(study$protocol)$purpose, "recommendation")) {
      "\u540c\u610f\u7a0b\u5ea6"
    } else {
      "\u9002\u5b9c\u6027"
    }
    attr(study2, "rand_force_round") <- 1L
    sheet_round <- 1L
    items_mode <- "all"
  } else {
    docs <- .questionnaire_rand_followup(study, round, ids)
    study2 <- study
    attr(study2, "rand_force_dimension") <- if (identical(.rand_settings(study$protocol)$purpose, "recommendation")) {
      "\u540c\u610f\u7a0b\u5ea6"
    } else {
      "\u9002\u5b9c\u6027"
    }
    attr(study2, "rand_force_round") <- as.integer(round)
    sheet_round <- round
    items_mode <- "all"
  }
  .write_questionnaires(
    docs, study2, sheet_round, format, file, items_mode,
    ids, NULL, FALSE, prefill
  )
}

.rand_meta <- function(study) {
  p <- study$protocol
  s <- .rand_settings(p)
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  app <- r[!grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  rounds <- if (nrow(app)) .unique_sorted(app$round) else integer()
  list(
    rule = s$rule,
    round2_mode = s$round2_mode,
    purpose = s$purpose,
    necessity = s$necessity,
    deviation = any(rounds >= 3L),
    modified_ram = identical(s$rule, "percent") || isTRUE(p$rand$nonstandard_ipras) ||
      !isTRUE(all.equal(s$scale, c(1, 9))),
    nonstandard_ipras = isTRUE(p$rand$nonstandard_ipras),
    supplement = "\u6743\u5a01\u7cfb\u6570\u4e0e Kendall's W \u53ea\u4f5c\u8865\u5145\uff0c\u4e0d\u66ff\u4ee3\u4e2d\u4f4d\u6570\u3001\u5206\u6b67\u548c\u9002\u5b9c\u6027\u5206\u7c7b\u3002"
  )
}

#' 完成 RAND 末轮分析，并按需写出报告
#'
#' 对最后一轮适宜性评分做校验、描述、查表或 IPRAS 分类、轮次比较和规则敏感性。
#' 若有必要性评分，同时分类。若有利益相关者分组，沿用已有的分层。
#' 权威系数和 Kendall's W 若能计算，只作为补充指标。
#'
#' @param study [delphi_study()] 或已有的分析结果。
#' @param round `"last"` 或正整数。整数表示只分析到该轮（含此前轮次）。
#' @param report 是否写报告。默认否，只返回分析结果。
#' @param format `"docx"`、`"html"`、`"word"` 或 `"both"`。选择 html 时仍会另写一份 docx。
#' @param file 报告路径或目录。交互会话中留空时可弹出文件选择；否则写入临时目录并提示路径。
#' @param open 是否打开报告。
#' @param loo 敏感性分析是否逐一剔除专家。
#' @param ... 传给 [analyze_delphi()]。
#'
#' @return `delphi_result`。附加 `necessity`、`round_comparison`、`sensitivity`、
#'   `rand_meta`。若写了报告，属性 `report_file` 为路径。
#'
#' @section 输入输出:
#' **输入**为已经锁定 `rand_ucla` 方案并读入评分的研究。
#'
#' **输出**分类表可直接核对。报告至少含 docx，内容包括规则出处、第 2 轮形式、
#' 两类不确定、分歧清单、轮次变化、敏感性和补充指标的限制说明。
#'
#' @examples
#' \donttest{
#' ram <- simulate_ram_classic()
#' res <- run_rand(ram, report = FALSE)
#' table(res$last_classification$class_detail)
#' f <- run_rand(ram, report = TRUE, format = "docx",
#'               file = tempfile(fileext = ".docx"))
#' attr(f, "report_file")
#' }
#'
#' @export
run_rand <- function(study, round = "last", report = FALSE,
                     format = c("docx", "html", "word", "both"),
                     file = NULL, open = FALSE, loo = TRUE, ...) {
  format <- match.arg(format)
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  .stop_if(
    identical(study$protocol$scenario, "delphi_ram"),
    "\u8fd9\u662f\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM\u3002\u8bf7\u4f7f\u7528 run_delphi_ram()\uff0c\u4e0d\u8981\u4f7f\u7528 run_rand()\u3002"
  )
  .stop_if(!identical(study$protocol$scenario, "rand_ucla"),
           "run_rand() \u53ea\u7528\u4e8e\u573a\u666f rand_ucla\u3002")
  up <- NULL
  if (!identical(round, "last")) {
    up <- as.integer(round)[1]
    .stop_if(is.na(up) || up < 1L, "round \u5fc5\u987b\u662f \"last\" \u6216\u6b63\u6574\u6570\u3002")
  }
  result <- analyze_delphi(study, up_to_round = up, ...)
  result$rand_meta <- .rand_meta(result$study)
  result$round_comparison <- tryCatch(
    rand_compare_rounds(result),
    error = function(e) list(items = NULL, group = NULL, note = conditionMessage(e))
  )
  result$sensitivity <- tryCatch(
    rand_sensitivity(result, round = result$last_round, loo = loo),
    error = function(e) list(error = conditionMessage(e))
  )
  if (is.null(result$necessity)) {
    has_nec <- any(grepl("\u5fc5\u8981", result$study$ratings$dimension %||% ""))
    if (isTRUE(has_nec)) {
      result$necessity <- tryCatch(classify_necessity(result$study), error = function(e) NULL)
    }
  }
  if (!isTRUE(report)) return(result)
  if (identical(format, "html")) format <- "both"
  if (is.null(file)) {
    if (isTRUE(interactive())) {
      message("\u8bf7\u9009\u62e9\u62a5\u544a\u4fdd\u5b58\u4f4d\u7f6e\uff08docx\uff09\u3002")
      file <- tryCatch(utils::file.choose(), error = function(e) NULL)
    }
    if (is.null(file) || !nzchar(file[1])) {
      file <- tempfile("rand-report-", fileext = ".docx")
      message("\u672a\u6307\u5b9a file\uff0c\u62a5\u544a\u5199\u5165: ", file)
    }
  }
  files <- delphi_report(result, file = file, format = format, template = "rand_ucla", open = open)
  attr(result, "report_file") <- files
  result
}

.ram_dict <- function(lang = c("zh", "en")) {
  lang <- match.arg(lang)
  if (identical(lang, "en")) {
    list(
      class = "Class", item = "Indication", median = "Median",
      appropriate = "Appropriate", uncertain = "Uncertain", inappropriate = "Inappropriate",
      procedure = "Procedure", round = "Round", deviation = "Deviation from panel median",
      expert = "Panelist", rule = "Rule", share = "Share",
      heatmap = "Appropriateness matrix",
      deviation_title = "Anonymous deviation from the item median",
      progress = "Classification across rounds",
      sensitivity = "Class counts under alternative rules"
    )
  } else {
    list(
      class = "\u5206\u7c7b", item = "\u60c5\u5883", median = "\u4e2d\u4f4d\u6570",
      appropriate = "\u9002\u5b9c", uncertain = "\u4e0d\u786e\u5b9a", inappropriate = "\u4e0d\u9002\u5b9c",
      procedure = "\u5e72\u9884", round = "\u8f6e\u6b21", deviation = "\u4e0e\u6761\u76ee\u4e2d\u4f4d\u6570\u7684\u5dee",
      expert = "\u4e13\u5bb6\uff08\u533f\u540d\uff09", rule = "\u89c4\u5219", share = "\u6bd4\u4f8b",
      heatmap = "\u9002\u5b9c\u6027\u77e9\u9635",
      deviation_title = "\u4e13\u5bb6\u8bc4\u5206\u4e0e\u6761\u76ee\u4e2d\u4f4d\u6570\u7684\u504f\u5dee\uff08\u533f\u540d\uff09",
      progress = "\u5404\u8f6e\u5206\u7c7b\u6784\u6210",
      sensitivity = "\u4e0d\u540c\u89c4\u5219\u4e0b\u7684\u5206\u7c7b\u4eba\u6570"
    )
  }
}

.ram_colours <- function() {
  c(appropriate = "#009E73", uncertain = "#E69F00", inappropriate = "#D55E00",
    other = "#0072B2")
}

#' RAND 图形的统一主题
#'
#' @param lang `"zh"` 或 `"en"`。主题本身不含文字，供其他 `plot_rand_*` 使用；
#'   也可单独加到 ggplot 对象上。
#'
#' @return ggplot2 主题。
#'
#' @section 输入输出:
#' **输入**语言代码。**输出**可与 ggplot 对象相加的主题。配色为 Okabe-Ito，
#' 不依赖额外调色板包。
#'
#' @examples
#' ram <- simulate_ram_classic()
#' plot_rand_progress(ram) + theme_ram("en")
#'
#' @export
theme_ram <- function(lang = c("zh", "en")) {
  lang <- match.arg(lang)
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

.ram_class_data <- function(study, round = NULL) {
  if (inherits(study, "delphi_rand")) return(study)
  if (inherits(study, "delphi_result")) {
    d <- study$classification
    if (!is.null(round)) d <- d[d$round == round, , drop = FALSE]
    return(d)
  }
  classify_rand(.match_study(study), round = round)
}

#' 情境 × 干预的适宜性热图
#'
#' @param study RAND 研究、分析结果或 [classify_rand()] 结果。
#' @param round 轮次。
#' @param lang `"zh"` 或 `"en"`。
#'
#' @return ggplot 对象。
#'
#' @section 输入输出:
#' **输入**条目表若有 `indication` 与 `procedure`，分别作为纵轴和横轴。
#' 否则用条目编号。
#'
#' **输出**按适宜 / 不确定 / 不适宜着色。分歧条目在格内标出。
#'
#' @examples
#' plot_rand_heatmap(simulate_ram_classic(), round = 2)
#'
#' @export
plot_rand_heatmap <- function(study, round = NULL, lang = c("zh", "en")) {
  lang <- match.arg(lang)
  lab <- .ram_dict(lang)
  d <- .ram_class_data(study, round)
  src <- if (inherits(study, "delphi_result")) study$study else if (inherits(study, "delphi_study")) study else NULL
  if (!is.null(src) && all(c("indication", "procedure") %in% names(src$items))) {
    d$indication <- src$items$indication[match(d$item_id, src$items$item_id)]
    d$procedure <- src$items$procedure[match(d$item_id, src$items$item_id)]
  } else {
    d$indication <- d$item_id
    d$procedure <- if ("dimension" %in% names(d)) d$dimension else lab$class
  }
  d$fill <- ifelse(d$class %in% names(.ram_colours()), d$class, "other")
  d$mark <- ifelse(d$disagreement %in% TRUE, "\u00b7", "")
  ggplot2::ggplot(d, ggplot2::aes(.data$procedure, .data$indication, fill = .data$fill)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$mark), size = 6) +
    ggplot2::scale_fill_manual(values = .ram_colours(), breaks = c("appropriate", "uncertain", "inappropriate"),
                               labels = c(lab$appropriate, lab$uncertain, lab$inappropriate)) +
    ggplot2::labs(title = lab$heatmap, x = lab$procedure, y = lab$item, fill = lab$class) +
    theme_ram(lang)
}

#' 专家相对条目中位数的匿名偏差
#'
#' @param study RAND 研究。
#' @param round 轮次，默认最后一轮适宜性评分。
#' @param lang `"zh"` 或 `"en"`。
#'
#' @return ggplot 对象。横轴是匿名编号，不是姓名。
#'
#' @section 输入输出:
#' **输入**长表评分。**输出**每位专家各条目「分数 − 该条目中位数」的分布。
#' 系统性偏在一侧的专家应在讨论中注意，但不要据此删数据，除非方案事先写明。
#'
#' @examples
#' plot_rand_deviation(simulate_ram_classic(), round = 2)
#'
#' @export
plot_rand_deviation <- function(study, round = NULL, lang = c("zh", "en")) {
  lang <- match.arg(lang)
  lab <- .ram_dict(lang)
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[!grepl("\u5fc5\u8981", r$dimension) & !r$unable & !is.na(r$score), , drop = FALSE]
  if (is.null(round)) round <- max(r$round)
  r <- r[r$round == round, , drop = FALSE]
  med <- tapply(r$score, r$item_id, stats::median)
  r$dev <- r$score - med[as.character(r$item_id)]
  ids <- sort(unique(as.character(r$expert_id)))
  r$anon <- sprintf("P%02d", match(as.character(r$expert_id), ids))
  ggplot2::ggplot(r, ggplot2::aes(.data$anon, .data$dev)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_boxplot(fill = "#56B4E9", outlier.shape = NA, width = 0.6) +
    ggplot2::geom_jitter(width = 0.12, height = 0, size = 1.4, alpha = 0.7) +
    ggplot2::labs(title = lab$deviation_title, x = lab$expert, y = lab$deviation) +
    theme_ram(lang)
}

#' 各轮适宜、不确定、不适宜的比例
#'
#' @param study RAND 研究或分析结果。
#' @param lang `"zh"` 或 `"en"`。
#'
#' @return ggplot 对象。
#'
#' @section 输入输出:
#' **输入**一轮或多轮分类。**输出**堆叠条形图。比例变化描述收敛，
#' 不表示必须继续加轮直到没有不确定。
#'
#' @examples
#' plot_rand_progress(simulate_ram_classic())
#'
#' @export
plot_rand_progress <- function(study, lang = c("zh", "en")) {
  lang <- match.arg(lang)
  lab <- .ram_dict(lang)
  d <- .ram_class_data(study, round = NULL)
  d <- d[!is.na(d$class), , drop = FALSE]
  tab <- as.data.frame(table(d$round, d$class), stringsAsFactors = FALSE)
  names(tab) <- c("round", "class", "n")
  tab$round <- as.integer(as.character(tab$round))
  totals <- tapply(tab$n, tab$round, sum)
  tab$share <- tab$n / totals[as.character(tab$round)]
  tab$class <- factor(tab$class, c("appropriate", "uncertain", "inappropriate"))
  ggplot2::ggplot(tab, ggplot2::aes(factor(.data$round), .data$share, fill = .data$class)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_fill_manual(values = .ram_colours(),
                               labels = c(lab$appropriate, lab$uncertain, lab$inappropriate)) +
    ggplot2::labs(title = lab$progress, x = lab$round, y = lab$share, fill = lab$class) +
    theme_ram(lang)
}

#' 不同判定规则下的分类计数
#'
#' @param study RAND 研究，或 [rand_sensitivity()] 的结果。
#' @param round 轮次。
#' @param lang `"zh"` 或 `"en"`。
#'
#' @return ggplot 对象。
#'
#' @section 输入输出:
#' **输入**研究或已经算好的敏感性结果。传入研究时不逐一剔除专家，只比较规则。
#'
#' **输出**分组条形图。它展示规则选择会不会改变结论，不替代锁定规则。
#'
#' @examples
#' plot_rand_sensitivity(simulate_ram_classic(), round = 2)
#'
#' @export
plot_rand_sensitivity <- function(study, round = NULL, lang = c("zh", "en")) {
  lang <- match.arg(lang)
  lab <- .ram_dict(lang)
  sens <- if (inherits(study, "rand_sensitivity")) {
    study
  } else {
    rand_sensitivity(study, round = round, loo = FALSE)
  }
  cols <- grep("^class_", names(sens$by_item), value = TRUE)
  long <- do.call(rbind, lapply(cols, function(col) {
    data.frame(
      rule = sub("^class_", "", col),
      class = sens$by_item[[col]],
      stringsAsFactors = FALSE
    )
  }))
  long <- long[!is.na(long$class), , drop = FALSE]
  tab <- as.data.frame(table(long$rule, long$class), stringsAsFactors = FALSE)
  names(tab) <- c("rule", "class", "n")
  tab$class <- factor(tab$class, c("appropriate", "uncertain", "inappropriate"))
  ggplot2::ggplot(tab, ggplot2::aes(.data$rule, .data$n, fill = .data$class)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(values = .ram_colours(),
                               labels = c(lab$appropriate, lab$uncertain, lab$inappropriate)) +
    ggplot2::labs(title = lab$sensitivity, x = lab$rule, y = "n", fill = lab$class) +
    theme_ram(lang)
}

.ram_score_row <- function(item_id, round, scores, panel) {
  data.frame(
    expert_id = panel$expert_id,
    item_id = item_id,
    round = round,
    score = scores,
    dimension = "\u9002\u5b9c\u6027",
    unable = FALSE,
    stakeholder = panel$stakeholder,
    stringsAsFactors = FALSE
  )
}

#' 经典两轮适宜性的固定模拟数据
#'
#' 9 名专家、4 个临床情境 × 2 种干预。分数是事先排好的，用来保证适宜、
#' 不适宜、中位数居中的不确定、以及因分歧导致的不确定都会出现。
#' 不是随机抽样，也不能当作真实指南结论。
#'
#' @return `delphi_study`。方案为 `rand_ucla`，规则为表 4，最多 2 轮。
#'
#' @section 输入输出:
#' **输入**无。**输出**可直接生成第 1 轮问卷、分析，再生成第 2 轮反馈问卷。
#'
#' @examples
#' ram <- simulate_ram_classic()
#' table(classify_rand(ram, round = 2)$class_detail)
#'
#' @export
simulate_ram_classic <- function() {
  panel <- data.frame(
    expert_id = sprintf("P%d", 1:9),
    stakeholder = "\u4e34\u5e8a\u4e13\u5bb6",
    specialty = c("\u5fc3\u5185\u79d1", "\u5fc3\u5185\u79d1", "\u5fc3\u810f\u5916\u79d1", "\u5fc3\u810f\u5916\u79d1",
                  "\u4ecb\u5165", "\u5f71\u50cf", "\u5168\u79d1", "\u5168\u79d1", "\u65b9\u6cd5\u5b66"),
    region = c("\u5317\u533a", "\u5357\u533a", "\u4e1c\u533a", "\u5317\u533a", "\u5357\u533a", "\u4e1c\u533a", "\u5317\u533a", "\u5357\u533a", "\u4e1c\u533a"),
    title = "\u4e3b\u4efb\u533b\u5e08",
    years = c(18, 22, 15, 27, 12, 16, 20, 14, 19),
    invited = TRUE,
    stringsAsFactors = FALSE
  )
  indications <- c(
    "\u7a33\u5b9a\u578b\u5fc3\u7ede\u75db\uff0c\u5355\u652f\u75c5\u53d8",
    "\u7a33\u5b9a\u578b\u5fc3\u7ede\u75db\uff0c\u4e09\u652f\u75c5\u53d8",
    "\u65e0\u4fdd\u62a4\u5de6\u4e3b\u5e72\u75c5\u53d8",
    "\u6025\u6027 ST \u6bb5\u62ac\u9ad8\u578b\u5fc3\u808c\u6897\u6b7b"
  )
  procedures <- c("\u7ecf\u76ae\u51a0\u72b6\u52a8\u8109\u4ecb\u5165", "\u51a0\u72b6\u52a8\u8109\u65c1\u8def\u79fb\u690d")
  items <- expand.grid(indication = indications, procedure = procedures, stringsAsFactors = FALSE)
  items$item_id <- sprintf("IND%02d", seq_len(nrow(items)))
  items$item_text <- paste(items$indication, items$procedure, sep = "\uff1a")
  items$domain <- "\u9002\u5b9c\u6027"
  items$evidence_level <- c("A", "B", "B", "A", "C", "B", "A", "B")
  items$pattern <- c("appropriate", "inappropriate", "equipoise", "disagreement",
                     "shift", "inappropriate", "appropriate_outlier", "polar")
  # 9 scores, fixed so the four result types are present at round 2.
  bank <- list(
    IND01 = list(c(8, 8, 9, 9, 8, 7, 9, 8, 8), c(8, 9, 9, 8, 8, 8, 9, 8, 9)),
    IND02 = list(c(1, 2, 2, 1, 3, 2, 1, 2, 3), c(1, 1, 2, 2, 2, 1, 3, 2, 2)),
    IND03 = list(c(4, 5, 5, 5, 6, 4, 5, 5, 6), c(4, 5, 5, 6, 5, 5, 4, 5, 6)),
    IND04 = list(c(2, 3, 3, 7, 8, 8, 8, 9, 9), c(2, 3, 3, 7, 8, 8, 8, 9, 9)),
    IND05 = list(c(4, 5, 5, 6, 6, 7, 7, 8, 5), c(7, 7, 8, 8, 8, 9, 7, 8, 8)),
    IND06 = list(c(1, 1, 2, 2, 2, 3, 1, 2, 3), c(1, 2, 1, 2, 2, 1, 3, 2, 1)),
    IND07 = list(c(7, 8, 8, 8, 9, 9, 7, 8, 2), c(7, 8, 8, 9, 8, 8, 7, 9, 3)),
    IND08 = list(c(1, 1, 2, 2, 5, 8, 9, 9, 9), c(1, 2, 2, 1, 5, 8, 9, 9, 8))
  )
  rows <- lapply(names(bank), function(id) {
    rbind(
      .ram_score_row(id, 1L, bank[[id]][[1]], panel),
      .ram_score_row(id, 2L, bank[[id]][[2]], panel)
    )
  })
  ratings <- do.call(rbind, rows)
  comments <- data.frame(
    expert_id = c("P1", "P3", "P5"),
    item_id = c("IND04", "IND08", "IND03"),
    round = c(1L, 1L, 1L),
    comment = c(
      "\u4e09\u652f\u75c5\u53d8\u65f6\u4ecb\u5165\u4e0e\u5916\u79d1\u7684\u83b7\u76ca\u5224\u65ad\u4e0d\u4e00\u81f4\u3002",
      "\u6025\u6027\u671f\u5916\u79d1\u642d\u6865\u7684\u98ce\u9669\u4e0e\u83b7\u76ca\u4ecd\u6709\u5206\u6b67\u3002",
      "\u5355\u652f\u75c5\u53d8\u505a\u5916\u79d1\u642d\u6865\uff0c\u83b7\u76ca\u4e0e\u4f24\u5bb3\u63a5\u8fd1\u3002"
    ),
    theme = c("\u5206\u6b67", "\u5206\u6b67", "\u8bc1\u636e\u4e0d\u8db3"),
    stringsAsFactors = FALSE
  )
  meta <- list(
    title = "\u7a33\u5b9a\u578b\u51a0\u5fc3\u75c5\u8840\u8fd0\u91cd\u5efa\u9002\u5b9c\u6027\uff08\u6559\u5b66\u6a21\u62df\uff09",
    objective = "\u5224\u65ad\u7279\u5b9a\u60c5\u5883\u4e0b\u4ecb\u5165\u6216\u5916\u79d1\u8840\u8fd0\u91cd\u5efa\u662f\u5426\u9002\u5b9c\u3002\u4e0d\u786e\u5b9a\u4e0e\u5206\u6b67\u4f5c\u4e3a\u6b63\u5f0f\u7ed3\u679c\u4fdd\u7559\u3002",
    users = "\u5fc3\u8840\u7ba1\u5185\u79d1\u3001\u5916\u79d1\u4e0e\u5168\u79d1",
    geography = "\u6559\u5b66\u793a\u4f8b\uff0c\u4e0d\u662f\u771f\u5b9e\u6307\u5357",
    evidence_note = "\u7b2c 1 \u8f6e\u524d\u5df2\u5411\u9762\u677f\u63d0\u4f9b\u6a21\u62df\u8bc1\u636e\u6458\u8981\u3002",
    anonymity = "\u7b2c 1 \u8f6e\u72ec\u7acb\u533f\u540d\u8bc4\u5206\uff1b\u7b2c 2 \u8f6e\u9762\u5bf9\u9762\u4f1a\u8bae\u540e\u518d\u6b21\u72ec\u7acb\u8bc4\u5206\u3002",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\uff0c\u65e0\u771f\u5b9e\u53d7\u8bd5\u8005\u3002"
  )
  delphi_study(
    protocol = preset_protocol("rand_ucla"),
    panel = panel,
    items = items[, c("item_id", "item_text", "domain", "indication", "procedure", "evidence_level", "pattern")],
    ratings = ratings,
    comments = comments,
    meta = meta,
    validate = TRUE
  )
}

#' 在经典适宜性之后追加必要性评分
#'
#' 只给第 2 轮「适宜且无分歧」的条目填必要性分数，使「必要」和
#' 「适宜但非必要」都会出现。其余条目没有必要性分数。
#'
#' @return `delphi_study`。必要性在第 3 轮、维度为「必要性」。
#'
#' @section 输入输出:
#' **输入**无。**输出**可用 [classify_necessity()] 直接分类。
#' 第 3 轮不是新的适宜性评分。
#'
#' @examples
#' nec <- simulate_rand_necessity()
#' table(classify_necessity(nec)$necessity_zh, useNA = "ifany")
#'
#' @export
simulate_rand_necessity <- function() {
  study <- simulate_ram_classic()
  study$protocol <- preset_protocol(
    "rand_ucla",
    rand_necessity = TRUE,
    rationale = "\u4ec5\u5bf9\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u7684\u6761\u76ee\u53e6\u505a\u5fc5\u8981\u6027\u8bc4\u4ef7\uff0c\u4e0d\u52a0\u7b2c 3 \u8f6e\u9002\u5b9c\u6027\u8bc4\u5206\u3002"
  )
  panel <- study$panel
  # IND01 necessary; IND05 appropriate but not necessary; IND07 necessity disagreement.
  bank <- list(
    IND01 = c(8, 8, 9, 9, 8, 7, 9, 8, 8),
    IND05 = c(5, 5, 6, 6, 5, 4, 5, 6, 5),
    IND07 = c(1, 2, 8, 8, 9, 9, 7, 8, 3)
  )
  extra <- do.call(rbind, lapply(names(bank), function(id) {
    data.frame(
      expert_id = panel$expert_id,
      item_id = id,
      round = 3L,
      score = bank[[id]],
      dimension = "\u5fc5\u8981\u6027",
      unable = FALSE,
      stakeholder = panel$stakeholder,
      stringsAsFactors = FALSE
    )
  }))
  study$ratings <- rbind(study$ratings, extra)
  study$meta$objective <- paste(
    study$meta$objective,
    "\u5bf9\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u7684\u6307\u5f81\u518d\u8bc4\u4ef7\u5fc5\u8981\u6027\u3002"
  )
  study
}

#' 推荐意见的百分比共识（RAM 修改版）
#'
#' 12 名专家对推荐意见做两轮 1–9 分评分。方案锁定百分比规则，而不是表 4。
#' 报告中必须写成修改版，不能写成经典适宜性评价。
#'
#' @return `delphi_study`。
#'
#' @section 输入输出:
#' **输入**无。**输出**的 `protocol$rand$rule` 为 `percent`，
#' `purpose` 为 `recommendation`。
#'
#' @examples
#' rec <- simulate_rand_recommendation()
#' rec$protocol$rand$rule
#' table(classify_rand(rec, round = 2)$class_zh)
#'
#' @export
simulate_rand_recommendation <- function() {
  protocol <- preset_protocol(
    "rand_ucla",
    rand_disagreement = "percent",
    rand_purpose = "recommendation",
    rand_percent_high = 0.70,
    rand_percent_low = 0.15,
    rand_round2_mode = "online",
    rationale = "\u63a8\u8350\u610f\u89c1\u4f7f\u7528\u767e\u5206\u6bd4\u89c4\u5219\uff0c\u5c5e\u4e8e RAM \u4fee\u6539\u7248\uff0c\u4e0d\u628a IPRAS \u5e38\u6570\u5957\u5230\u672c\u573a\u666f\u3002"
  )
  panel <- data.frame(
    expert_id = sprintf("R%02d", 1:12),
    stakeholder = rep(c("\u4e34\u5e8a", "\u65b9\u6cd5\u5b66"), each = 6),
    specialty = rep(c("\u5fc3\u5185\u79d1", "\u5168\u79d1", "\u62a4\u7406", "\u836f\u5b66", "\u7edf\u8ba1", "\u60a3\u8005\u4ee3\u8868"), 2),
    title = "\u59d4\u5458",
    years = 10 + seq_len(12),
    invited = TRUE,
    stringsAsFactors = FALSE
  )
  texts <- c(
    "\u5bf9\u9ad8\u5371\u51fa\u8840\u60a3\u8005\uff0c\u5efa\u8bae\u5728\u542f\u52a8\u6297\u51dd\u524d\u5b8c\u6210\u57fa\u7ebf\u5b9e\u9a8c\u5ba4\u68c0\u67e5\u3002",
    "\u5bf9\u4f4e\u5371\u62e9\u671f\u624b\u672f\uff0c\u5efa\u8bae\u628a\u672f\u524d\u68c0\u67e5\u7f29\u51cf\u4e3a\u4ec5\u8840\u5e38\u89c4\u3002",
    "\u5efa\u8bae\u628a\u60a3\u8005\u62a5\u544a\u7684\u51fa\u8840\u4e8b\u4ef6\u5217\u5165\u4e3b\u8981\u5b89\u5168\u7ec8\u70b9\u3002",
    "\u5efa\u8bae\u5728\u8bc1\u636e\u4e0d\u8db3\u65f6\u4ecd\u5199\u51fa\u5f3a\u63a8\u8350\u3002",
    "\u5efa\u8bae\u5411\u60a3\u8005\u8bf4\u660e\u6297\u51dd\u7684\u83b7\u76ca\u4e0e\u51fa\u8840\u98ce\u9669\u540e\u518d\u51b3\u5b9a\u3002",
    "\u5efa\u8bae\u7528\u5355\u4e00\u4e2d\u5fc3\u7ecf\u9a8c\u66ff\u4ee3\u591a\u4e2d\u5fc3\u8bc1\u636e\u3002",
    "\u5efa\u8bae\u5728\u51fa\u9662\u524d\u590d\u6838\u80be\u529f\u80fd\u4e0e\u5408\u5e76\u7528\u836f\u3002",
    "\u5efa\u8bae\u5bf9\u6240\u6709\u60a3\u8005\u4e00\u5f8b\u505c\u7528\u963f\u53f8\u5339\u6797\uff0c\u4e0d\u5206\u5371\u9669\u3002"
  )
  items <- data.frame(
    item_id = sprintf("REC%02d", seq_along(texts)),
    item_text = texts,
    domain = "\u63a8\u8350\u610f\u89c1",
    evidence_level = c("B", "C", "B", "C", "A", "C", "B", "C"),
    stringsAsFactors = FALSE
  )
  # 12 scores. Consensus needs >= 70% in 7-9 and < 15% in 1-3, i.e. at least 9 high and at most 1 low.
  bank <- list(
    REC01 = list(c(8, 8, 9, 7, 8, 9, 8, 7, 9, 6, 8, 7), c(8, 9, 9, 8, 8, 9, 8, 8, 9, 7, 8, 8)),
    REC02 = list(c(3, 4, 5, 6, 7, 8, 2, 5, 6, 7, 4, 9), c(3, 4, 5, 5, 6, 7, 2, 5, 6, 7, 4, 8)),
    REC03 = list(c(7, 8, 8, 9, 7, 8, 9, 8, 7, 6, 8, 9), c(8, 8, 8, 9, 7, 8, 9, 8, 7, 7, 8, 9)),
    REC04 = list(c(1, 2, 2, 3, 4, 8, 1, 2, 3, 7, 2, 1), c(1, 2, 1, 2, 3, 6, 1, 2, 2, 4, 2, 1)),
    REC05 = list(c(8, 7, 9, 8, 8, 7, 9, 8, 6, 8, 9, 7), c(8, 8, 9, 8, 8, 8, 9, 8, 7, 8, 9, 8)),
    REC06 = list(c(2, 3, 4, 5, 6, 7, 8, 2, 3, 4, 9, 1), c(2, 3, 4, 5, 5, 6, 7, 2, 3, 4, 8, 2)),
    REC07 = list(c(7, 7, 8, 8, 9, 7, 8, 8, 9, 7, 6, 8), c(7, 8, 8, 8, 9, 7, 8, 8, 9, 8, 7, 8)),
    REC08 = list(c(1, 1, 2, 8, 9, 2, 1, 7, 3, 2, 1, 8), c(1, 2, 2, 8, 9, 2, 1, 7, 2, 1, 2, 8))
  )
  rows <- lapply(names(bank), function(id) {
    rbind(
      data.frame(expert_id = panel$expert_id, item_id = id, round = 1L, score = bank[[id]][[1]],
                 dimension = "\u540c\u610f\u7a0b\u5ea6", unable = FALSE, stakeholder = panel$stakeholder,
                 stringsAsFactors = FALSE),
      data.frame(expert_id = panel$expert_id, item_id = id, round = 2L, score = bank[[id]][[2]],
                 dimension = "\u540c\u610f\u7a0b\u5ea6", unable = FALSE, stakeholder = panel$stakeholder,
                 stringsAsFactors = FALSE)
    )
  })
  meta <- list(
    title = "\u56f4\u672f\u671f\u6297\u51dd\u63a8\u8350\u610f\u89c1\uff08\u6559\u5b66\u6a21\u62df\uff0cRAM \u4fee\u6539\u7248\uff09",
    objective = "\u7528\u9884\u5148\u89c4\u5b9a\u7684\u767e\u5206\u6bd4\u89c4\u5219\u5224\u65ad\u63a8\u8350\u610f\u89c1\u662f\u5426\u8fbe\u6210\u5171\u8bc6\u3002\u8fd9\u4e0d\u662f\u7ecf\u5178\u9002\u5b9c\u6027\u8bc4\u4ef7\u3002",
    users = "\u56f4\u672f\u671f\u5de5\u4f5c\u7ec4",
    geography = "\u6559\u5b66\u793a\u4f8b",
    evidence_note = "\u6761\u76ee\u6765\u81ea\u6a21\u62df\u7684\u8bc1\u636e\u6458\u8981\uff0c\u4e0d\u662f GRADE \u63a8\u8350\u3002",
    anonymity = "\u4e24\u8f6e\u5747\u4e3a\u5728\u7ebf\u533f\u540d\u8bc4\u5206\uff1b\u7b2c 2 \u8f6e\u4e3a\u7ebf\u4e0a\u4f1a\u8bae\u540e\u590d\u8bc4\u3002",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\u3002"
  )
  delphi_study(
    protocol = protocol,
    panel = panel,
    items = items,
    ratings = do.call(rbind, rows),
    meta = meta,
    validate = TRUE
  )
}
