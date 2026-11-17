#' 界值敏感性（不替换锁定规则）
#'
#' 每次只移动一个预先列出的界值，重算同一轮分类，看有多少条目相对锁定规则改变。
#' 排序场景比较 Top-k 名单；Policy 只比较两极分化条目数。正式分类仍用方案里锁定的规则。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。不用于 `rand_ucla` 与 `delphi_ram`。
#' @param round 轮次。默认最后一轮。
#'
#' @return 列表：`summary` 为每次挪动的条目变化数，`by_item` 为条目级对照，
#'   `note` 说明探索性性质。不改写 `study$protocol`。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 5)
#' sens <- threshold_sensitivity(study, round = 1)
#' sens$summary
#'
#' @export
threshold_sensitivity <- function(study, round = NULL) {
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  p <- study$protocol
  .stop_if(
    p$scenario %in% c("rand_ucla", "delphi_ram"),
    "RAND \u4e0e\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM \u8bf7\u4f7f\u7528 rand_sensitivity() \u6216 delphi_ram_sensitivity()\u3002"
  )
  summary <- summarize_items(study)
  if (is.null(round)) round <- max(summary$round)
  base <- summary[summary$round == round, , drop = FALSE]
  .stop_if(!nrow(base), sprintf("\u7b2c %s \u8f6e\u6ca1\u6709\u8bc4\u5206\u3002", round))
  dim0 <- .consensus_dimension(base$dimension)
  if (identical(p$scenario, "standard_element")) {
    imp <- grepl("\u91cd\u8981", base$dimension)
    if (any(imp)) base <- base[imp, , drop = FALSE]
  } else if (!is.na(dim0)) {
    hit <- base$dimension == dim0
    if (any(hit)) base <- base[hit, , drop = FALSE]
  }
  if (identical(p$scenario, "ranking")) {
    return(.sensitivity_ranking(study, p, round, dim0))
  }
  if (identical(p$scenario, "policy")) {
    return(.sensitivity_policy(base, p, round))
  }
  locked <- classify_consensus(base, p)
  specs <- .sensitivity_specs(p)
  summary_rows <- list()
  by_item <- data.frame(
    item_id = locked$item_id,
    dimension = locked$dimension,
    class_locked = locked$class,
    stringsAsFactors = FALSE
  )
  for (spec in specs) {
    for (val in spec$alts) {
      alt <- p
      alt$rules <- utils::modifyList(p$rules, stats::setNames(list(val), spec$parameter))
      cls <- classify_consensus(base, alt)
      key <- paste(by_item$item_id, by_item$dimension)
      src <- paste(cls$item_id, cls$dimension)
      alt_class <- cls$class[match(key, src)]
      changed <- .class_changed_vec(by_item$class_locked, alt_class)
      col <- .sens_col(spec$parameter, val)
      by_item[[col]] <- alt_class
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        parameter = spec$parameter,
        locked_value = spec$locked,
        alt_value = val,
        n_items = nrow(by_item),
        n_changed = sum(changed),
        changed_items = .fmt_ids(by_item$item_id[changed]),
        exploratory = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }
  summ <- if (length(summary_rows)) do.call(rbind, summary_rows) else NULL
  if (!is.null(summ)) rownames(summ) <- NULL
  .sensitivity_result(
    round = round,
    scenario = p$scenario,
    by_item = by_item,
    summary = summ,
    note = paste0(
      "\u654f\u611f\u6027\u5206\u6790\u4e0d\u66ff\u6362\u5df2\u9501\u5b9a\u89c4\u5219\u3002\u6bcf\u6b21\u53ea\u79fb\u52a8\u4e00\u4e2a\u754c\u503c\uff0c",
      "\u7528\u6765\u770b\u7ed3\u8bba\u662f\u5426\u9760\u5728\u754c\u503c\u8fb9\u4e0a\u3002\u6b63\u5f0f\u5206\u7c7b\u4ecd\u6309\u7b2c 1 \u8f6e\u524d\u5199\u5165\u65b9\u6848\u7684\u90a3\u4e00\u5957\u3002",
      "\u4e0d\u80fd\u5728\u770b\u5230\u7ed3\u679c\u540e\u6539\u754c\u503c\u3002\u672c\u5206\u6790\u662f\u63a2\u7d22\u6027\u7684\u3002"
    )
  )
}

.analysis_sensitivity <- function(study, round) {
  sc <- study$protocol$scenario
  if (identical(sc, "rand_ucla")) {
    return(rand_sensitivity(study, round = round, loo = FALSE))
  }
  if (identical(sc, "delphi_ram")) {
    return(delphi_ram_sensitivity(study, round = round))
  }
  threshold_sensitivity(study, round = round)
}

.sensitivity_result <- function(round, scenario, by_item, summary, note) {
  structure(
    list(
      round = round,
      scenario = scenario,
      by_item = by_item,
      summary = summary,
      note = note
    ),
    class = "delphi_sensitivity"
  )
}

.sensitivity_specs <- function(protocol) {
  rules <- protocol$rules
  sc <- protocol$scenario
  scale <- c(protocol$scale_min, protocol$scale_max)
  specs <- list()
  add_num <- function(parameter, deltas, clamp = NULL, floor_at = NULL) {
    locked <- rules[[parameter]]
    if (length(locked) != 1L || !is.finite(locked)) return(invisible(NULL))
    vals <- unique(c(locked, locked + deltas))
    vals <- vals[is.finite(vals)]
    if (!is.null(clamp)) vals <- vals[vals >= clamp[1] & vals <= clamp[2]]
    if (length(floor_at) == 1L && is.finite(floor_at)) vals <- vals[vals + 1e-8 >= floor_at]
    alts <- vals[abs(vals - locked) > 1e-8]
    if (!length(alts)) return(invisible(NULL))
    specs[[length(specs) + 1L]] <<- list(parameter = parameter, locked = locked, alts = alts)
  }
  add_set <- function(parameter, candidates) {
    locked <- rules[[parameter]]
    if (length(locked) != 1L || !is.finite(locked)) return(invisible(NULL))
    vals <- unique(c(locked, candidates))
    vals <- vals[is.finite(vals) & vals >= 0 & vals <= 1]
    alts <- vals[abs(vals - locked) > 1e-8]
    if (!length(alts)) return(invisible(NULL))
    specs[[length(specs) + 1L]] <<- list(parameter = parameter, locked = locked, alts = alts)
  }
  if (sc %in% c("clinical_consensus", "definition")) {
    add_num(
      "high_pct", c(-0.05, 0.05), clamp = c(0, 1),
      floor_at = if (identical(sc, "clinical_consensus")) rules$basic_pct else NULL
    )
    add_num("iqr_max", c(-1, 1), clamp = c(0, Inf))
    add_num("median_min", c(-1, 1), clamp = scale)
  } else if (identical(sc, "comet_cos")) {
    add_set("comet_in_high", c(0.70, 0.75, 0.80))
    add_set("comet_in_low_max", c(0.10, 0.15, 0.20))
  } else if (identical(sc, "indicator")) {
    add_num("basic_pct", c(-0.05, 0.05), clamp = c(0, 1))
    add_num("iqr_max", c(-1, 1), clamp = c(0, Inf))
    add_num("median_min", c(-1, 1), clamp = scale)
    add_num("cv_max", c(-0.05, 0.05), clamp = c(0, Inf))
  } else if (identical(sc, "standard_element")) {
    add_num("mean_min", c(-0.25, 0.25), clamp = scale)
    add_num("cv_max", c(-0.05, 0.05), clamp = c(0, Inf))
  }
  specs
}

.sensitivity_ranking <- function(study, protocol, round, dim0) {
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[r$round == round & !r$unable & !is.na(r$score), , drop = FALSE]
  if (!is.na(dim0)) r <- r[r$dimension == dim0, , drop = FALSE]
  .stop_if(!nrow(r), "\u6ca1\u6709\u53ef\u7528\u4e8e\u6392\u5e8f\u654f\u611f\u6027\u7684\u8bc4\u5206\u3002")
  locked_k <- as.integer(protocol$rules$ranking_topk %||% 5L)
  ks <- unique(as.integer(c(locked_k, 3L, 5L, 10L)))
  ks <- ks[is.finite(ks) & ks >= 1L]
  sets <- lapply(ks, function(k) .topk_ids(r, k))
  names(sets) <- paste0("k", ks)
  locked_ids <- sets[[paste0("k", locked_k)]]
  by_item <- data.frame(item_id = sort(unique(r$item_id)), stringsAsFactors = FALSE)
  by_item$in_locked <- by_item$item_id %in% locked_ids
  rows <- list()
  for (k in ks[ks != locked_k]) {
    alt_ids <- sets[[paste0("k", k)]]
    by_item[[paste0("in_top_", k)]] <- by_item$item_id %in% alt_ids
    changed_ids <- setdiff(union(locked_ids, alt_ids), intersect(locked_ids, alt_ids))
    rows[[length(rows) + 1L]] <- data.frame(
      parameter = "ranking_topk",
      locked_value = locked_k,
      alt_value = k,
      n_items = length(unique(r$item_id)),
      n_in_topk = length(alt_ids),
      n_changed = length(changed_ids),
      changed_items = .fmt_ids(changed_ids),
      exploratory = TRUE,
      stringsAsFactors = FALSE
    )
  }
  summ <- if (length(rows)) do.call(rbind, rows) else NULL
  if (!is.null(summ)) rownames(summ) <- NULL
  .sensitivity_result(
    round, protocol$scenario, by_item, summ,
    note = paste0(
      "\u6392\u5e8f\u573a\u666f\u6bd4\u8f83\u7684\u662f Top-k \u540d\u5355\u662f\u5426\u53d8\u5316\uff0c\u4e0d\u662f\u5171\u8bc6\u5206\u7c7b\u3002",
      "\u654f\u611f\u6027\u5206\u6790\u4e0d\u66ff\u6362\u9501\u5b9a\u7684 k\u3002\u672c\u5206\u6790\u662f\u63a2\u7d22\u6027\u7684\u3002"
    )
  )
}

.sensitivity_policy <- function(base, protocol, round) {
  locked <- protocol$rules$polarization_threshold %||% 0.25
  vals <- unique(c(locked, 0.20, 0.25, 0.30))
  vals <- vals[is.finite(vals) & vals >= 0 & vals <= 1]
  locked_flag <- detect_polarization(base, threshold = locked)
  by_item <- data.frame(
    item_id = base$item_id,
    dimension = base$dimension,
    polarized_locked = locked_flag,
    stringsAsFactors = FALSE
  )
  rows <- list()
  for (val in vals[abs(vals - locked) > 1e-8]) {
    flag <- detect_polarization(base, threshold = val)
    changed <- .flag_changed(locked_flag, flag)
    by_item[[.sens_col("polarization", val)]] <- flag
    rows[[length(rows) + 1L]] <- data.frame(
      parameter = "polarization_threshold",
      locked_value = locked,
      alt_value = val,
      n_items = nrow(base),
      n_polarized = sum(flag, na.rm = TRUE),
      n_changed = sum(changed),
      changed_items = .fmt_ids(base$item_id[changed]),
      exploratory = TRUE,
      stringsAsFactors = FALSE
    )
  }
  summ <- if (length(rows)) do.call(rbind, rows) else NULL
  if (!is.null(summ)) rownames(summ) <- NULL
  .sensitivity_result(
    round, protocol$scenario, by_item, summ,
    note = paste0(
      "\u672c\u573a\u666f\u4e0d\u4ee5\u8fbe\u6210\u5171\u8bc6\u4e3a\u6210\u529f\u6807\u51c6\u3002",
      "\u8fd9\u91cc\u53ea\u6bd4\u8f83\u4e0d\u540c\u4e24\u6781\u5206\u5316\u9608\u503c\u4e0b\u7684\u6761\u76ee\u6570\u3002",
      "\u654f\u611f\u6027\u5206\u6790\u4e0d\u66ff\u6362\u9501\u5b9a\u9608\u503c\uff0c\u4e14\u662f\u63a2\u7d22\u6027\u7684\u3002"
    )
  )
}

.topk_ids <- function(ratings, k) {
  tab <- rank_priorities(ratings = ratings, k = k, score_is_rank = TRUE)
  if (!nrow(tab)) return(character())
  head(as.character(tab$item_id), min(as.integer(k), nrow(tab)))
}

.sens_col <- function(parameter, value) {
  tag <- gsub("\\.", "p", format(round(value, 4), scientific = FALSE, trim = TRUE))
  make.names(paste0(parameter, "_", tag))
}

.class_changed_vec <- function(a, b) {
  a <- as.character(a)
  b <- as.character(b)
  !is.na(a) & !is.na(b) & a != b
}

.flag_changed <- function(a, b) {
  out <- a != b
  out[is.na(out)] <- !(is.na(a) & is.na(b))[is.na(out)]
  out & !is.na(out)
}

.fmt_ids <- function(ids) {
  ids <- unique(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return("")
  if (length(ids) > 30L) return(paste(c(ids[1:30], "..."), collapse = ", "))
  paste(ids, collapse = ", ")
}
