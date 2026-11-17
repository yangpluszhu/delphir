#' 比较各轮中位数、IPR 与分类
#'
#' 条目级给出相邻轮的中位数、IPR、分类和是否转变。组层面给出达成共识条目比例、
#' 平均 IPR 和 Kendall's W 的变化。Wilcoxon 符号秩检验只作探索性描述。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。
#' @param classification 可选，已算好的分类表。缺省时按场景现算。
#' @param summary 保留参数，便于和 [analyze_delphi()] 对齐；当前不单独使用。
#' @param wilcoxon 是否计算配对 Wilcoxon。默认 `TRUE`。人数不足或前后分数完全相同时为 `NA`。
#'
#' @return 列表：`items` 为条目变化，`group` 为各轮汇总，`group_change` 为相邻轮组指标变化，
#'   `note` 声明 Wilcoxon 的探索性性质。只有一轮时 `items` 为空，并在 `note` 中说明。
#'
#' @section 共识口径:
#' 临床为高度或基本共识，COMET 为纳入，定义/指标体系/标准要素为保留或纳入。
#' RAND 以及非百分比的改良德尔菲结合 RAM 报告「适宜且无分歧」，并写明这不是百分比共识。
#' 百分比规则报告已达成共识（含赞同与不赞同）。排序和 Policy 不把达成共识当作成功标准。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 2, seed = 4)
#' cmp <- compare_rounds(study)
#' cmp$group_change
#'
#' @export
compare_rounds <- function(study, classification = NULL, summary = NULL, wilcoxon = TRUE) {
  if (inherits(study, "delphi_result")) {
    if (is.null(classification)) classification <- study$classification
    study <- study$study
  }
  study <- .match_study(study)
  if (identical(study$protocol$scenario, "rand_ucla")) {
    return(rand_compare_rounds(study, wilcoxon = wilcoxon))
  }
  if (is.null(classification)) {
    classification <- .classification_for_rounds(study)
  }
  .stop_if(is.null(classification) || !nrow(classification), "\u6ca1\u6709\u53ef\u6bd4\u8f83\u7684\u5206\u7c7b\u3002")
  dim0 <- .consensus_dimension(classification$dimension)
  rounds <- .unique_sorted(classification$round)
  group <- .round_group_table(classification, study, dim0)
  items <- NULL
  if (length(rounds) >= 2L) {
    items <- .round_item_table(classification, study, rounds, wilcoxon)
  }
  note <- if (length(rounds) < 2L) {
    paste0("\u53ea\u6709\u4e00\u8f6e\uff0c\u65e0\u6cd5\u6bd4\u8f83\u53d8\u5316\u3002", .wilcoxon_note(FALSE))
  } else {
    .wilcoxon_note(isTRUE(wilcoxon))
  }
  structure(
    list(
      items = items,
      group = group,
      group_change = .group_change_from(group),
      consensus_definition = if (nrow(group)) group$consensus_definition[1] else NA_character_,
      note = note
    ),
    class = "delphi_round_comparison"
  )
}

.wilcoxon_note <- function(computed = TRUE) {
  base <- paste0(
    "Wilcoxon \u7b26\u53f7\u79e9\u68c0\u9a8c\u53ea\u63cf\u8ff0\u540c\u4e00\u6279\u4e13\u5bb6\u7684\u5206\u6570\u662f\u5426\u6574\u4f53\u79fb\u52a8\u3002",
    "\u5b83\u662f\u63a2\u7d22\u6027\u7684\uff1aP \u503c\u4e0d\u5b9a\u4e49\u7a33\u5b9a\u3001\u9002\u5b9c\u6216\u5171\u8bc6\uff0c\u4e5f\u4e0d\u80fd\u51b3\u5b9a\u662f\u5426\u518d\u5f00\u4e00\u8f6e\u3002",
    "\u914d\u5bf9\u4eba\u6570\u8fc7\u5c11\u6216\u524d\u540e\u5206\u6570\u5b8c\u5168\u76f8\u540c\u65f6\uff0cP \u503c\u4e3a\u7a7a\u3002"
  )
  if (isTRUE(computed)) base else paste0(base, "\u672c\u6b21\u672a\u8ba1\u7b97 Wilcoxon\u3002")
}

.classification_for_rounds <- function(study) {
  p <- study$protocol
  if (identical(p$scenario, "delphi_ram")) {
    r <- .ensure_dimension(.ensure_unable(study$ratings))
    r <- r[!grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
    rounds <- .unique_sorted(r$round)
    out <- do.call(rbind, lapply(rounds, function(rnd) {
      classify_delphi_ram(study, round = rnd, warn = FALSE)
    }))
    rownames(out) <- NULL
    return(out)
  }
  classify_consensus(summarize_items(study), p)
}

.round_group_table <- function(classification, study, dim0) {
  sub0 <- classification
  if (!is.na(dim0) && "dimension" %in% names(sub0)) {
    keep <- sub0$dimension == dim0
    if (any(keep)) sub0 <- sub0[keep, , drop = FALSE]
  }
  rounds <- .unique_sorted(sub0$round)
  group <- do.call(rbind, lapply(rounds, function(rnd) {
    sub <- sub0[sub0$round == rnd, , drop = FALSE]
    iprs <- if (nrow(sub)) vapply(seq_len(nrow(sub)), function(i) .row_ipr(sub[i, , drop = FALSE]), numeric(1)) else numeric()
    data.frame(
      round = rnd,
      dimension = if (!is.na(dim0)) dim0 else NA_character_,
      n_items = nrow(sub),
      mean_ipr = if (any(is.finite(iprs))) mean(iprs[is.finite(iprs)]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  .annotate_round_group(group, sub0, study, if (!is.na(dim0)) dim0 else .consensus_dimension(sub0$dimension))
}

.annotate_round_group <- function(group, detail, study, kendall_dimension) {
  spec <- .consensus_spec(study$protocol)
  extra <- lapply(seq_len(nrow(group)), function(i) {
    rnd <- group$round[i]
    sub <- detail[detail$round == rnd, , drop = FALSE]
    kw <- .kendall_round_stats(study, rnd, kendall_dimension)
    if (isTRUE(spec$defined) && nrow(sub)) {
      hit <- .consensus_hit(sub, spec)
      n_cons <- sum(hit, na.rm = TRUE)
      pct <- .pct(n_cons, nrow(sub))
    } else {
      n_cons <- NA_integer_
      pct <- NA_real_
    }
    data.frame(
      n_consensus = n_cons,
      consensus_pct = pct,
      consensus_definition = spec$label,
      kendall_w = kw$w,
      kendall_chi_sq = kw$chi_sq,
      kendall_df = kw$df,
      kendall_p = kw$p,
      tie_correction = kw$tie,
      stringsAsFactors = FALSE
    )
  })
  cbind(group, do.call(rbind, extra))
}

.group_change_from <- function(group) {
  if (is.null(group) || nrow(group) < 2L) return(NULL)
  out <- do.call(rbind, lapply(seq_len(nrow(group) - 1L), function(i) {
    a <- group[i, ]
    b <- group[i + 1L, ]
    data.frame(
      from_round = a$round,
      to_round = b$round,
      consensus_pct_from = a$consensus_pct,
      consensus_pct_to = b$consensus_pct,
      consensus_pct_delta = b$consensus_pct - a$consensus_pct,
      mean_ipr_from = a$mean_ipr,
      mean_ipr_to = b$mean_ipr,
      mean_ipr_delta = b$mean_ipr - a$mean_ipr,
      kendall_w_from = a$kendall_w,
      kendall_w_to = b$kendall_w,
      kendall_w_delta = b$kendall_w - a$kendall_w,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.round_item_table <- function(classification, study, rounds, wilcoxon) {
  dims <- unique(classification$dimension)
  rows <- list()
  for (d in dims) {
    for (i in seq_len(length(rounds) - 1L)) {
      a <- classification[classification$round == rounds[i] & classification$dimension == d, , drop = FALSE]
      b <- classification[classification$round == rounds[i + 1L] & classification$dimension == d, , drop = FALSE]
      common <- intersect(a$item_id, b$item_id)
      for (id in common) {
        ra <- a[a$item_id == id, , drop = FALSE][1, ]
        rb <- b[b$item_id == id, , drop = FALSE][1, ]
        wp <- NA_real_
        if (isTRUE(wilcoxon)) wp <- .item_wilcoxon(study$ratings, id, rounds[i], rounds[i + 1L], d)
        rows[[length(rows) + 1L]] <- data.frame(
          item_id = id,
          dimension = d,
          from_round = rounds[i],
          to_round = rounds[i + 1L],
          median_from = ra$median,
          median_to = rb$median,
          ipr_from = .row_ipr(ra),
          ipr_to = .row_ipr(rb),
          class_from = ra$class %||% NA_character_,
          class_to = rb$class %||% NA_character_,
          class_zh_from = .class_zh_of(ra),
          class_zh_to = .class_zh_of(rb),
          changed = .class_changed(ra$class, rb$class),
          wilcoxon_p = wp,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.row_ipr <- function(row) {
  if ("ipr" %in% names(row) && length(row$ipr) && is.finite(row$ipr[1])) return(as.numeric(row$ipr[1]))
  if (all(c("p70", "p30") %in% names(row))) return(as.numeric(row$p70[1] - row$p30[1]))
  NA_real_
}

.class_zh_of <- function(row) {
  if ("class_zh" %in% names(row) && length(row$class_zh) && !is.na(row$class_zh[1]) && nzchar(row$class_zh[1])) {
    return(as.character(row$class_zh[1]))
  }
  .label_zh(row$class[1])
}

.class_changed <- function(a, b) {
  a <- as.character(a)[1]
  b <- as.character(b)[1]
  !is.na(a) && !is.na(b) && !identical(a, b)
}

.item_wilcoxon <- function(ratings, item_id, r1, r2, dimension, min_n = 6L) {
  pair <- .paired_scores(ratings, item_id, r1, r2, dimension)
  if (nrow(pair) < min_n || !any(pair$diff != 0) || !is.finite(stats::sd(pair$diff)) || stats::sd(pair$diff) == 0) {
    return(NA_real_)
  }
  out <- tryCatch(
    stats::wilcox.test(pair$r1, pair$r2, paired = TRUE, exact = FALSE)$p.value,
    error = function(e) NA_real_
  )
  as.numeric(out)
}

.kendall_round_stats <- function(study, round, dimension) {
  empty <- list(w = NA_real_, chi_sq = NA_real_, df = NA_integer_, p = NA_real_, tie = NA_real_)
  kw <- tryCatch(
    kendall_w(study, round = round, dimension = dimension),
    error = function(e) NULL
  )
  if (is.null(kw) || is.null(kw$w)) return(empty)
  list(
    w = kw$w,
    chi_sq = kw$chi_sq,
    df = kw$df,
    p = kw$p_value,
    tie = kw$tie_correction %||% NA_real_
  )
}

.consensus_spec <- function(protocol) {
  sc <- protocol$scenario
  rule <- protocol$rand$rule %||% protocol$rand$disagreement
  if (identical(sc, "clinical_consensus")) {
    list(label = "\u9ad8\u5ea6\u5171\u8bc6\u6216\u57fa\u672c\u5171\u8bc6", kind = "class",
         values = c("high_consensus", "basic_consensus"), defined = TRUE)
  } else if (identical(sc, "comet_cos")) {
    list(label = "\u7eb3\u5165\u6838\u5fc3\u7ed3\u5c40\u96c6", kind = "class",
         values = "consensus_in", defined = TRUE)
  } else if (identical(sc, "definition")) {
    list(label = "\u9ad8\u5ea6\u5171\u8bc6", kind = "class",
         values = "high_consensus", defined = TRUE)
  } else if (identical(sc, "indicator")) {
    list(label = "\u4fdd\u7559", kind = "class", values = "keep", defined = TRUE)
  } else if (identical(sc, "standard_element")) {
    list(label = "\u7eb3\u5165\u6838\u5fc3\u6280\u672f\u8981\u7d20", kind = "class",
         values = "retain", defined = TRUE)
  } else if (identical(sc, "rand_ucla") || (identical(sc, "delphi_ram") && !identical(rule, "percent"))) {
    list(
      label = "\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\uff08\u4e0d\u662f\u767e\u5206\u6bd4\u5171\u8bc6\uff09",
      kind = "appropriate_agree", values = NULL, defined = TRUE
    )
  } else if (identical(sc, "delphi_ram") && identical(rule, "percent")) {
    list(
      label = "\u5df2\u8fbe\u6210\u5171\u8bc6\uff08\u767e\u5206\u6bd4\u89c4\u5219\uff0c\u542b\u8d5e\u540c\u4e0e\u4e0d\u8d5e\u540c\uff09",
      kind = "ram_percent",
      values = c("consensus_endorse", "consensus_reject"),
      defined = TRUE
    )
  } else {
    list(
      label = "\u672c\u573a\u666f\u4e0d\u628a\u8fbe\u6210\u5171\u8bc6\u5f53\u4f5c\u6210\u529f\u6807\u51c6",
      kind = "none", values = NULL, defined = FALSE
    )
  }
}

.consensus_hit <- function(df, spec) {
  n <- nrow(df)
  if (!n) return(logical())
  if (identical(spec$kind, "class")) return(df$class %in% spec$values)
  if (identical(spec$kind, "appropriate_agree")) {
    dis <- if ("disagreement" %in% names(df)) df$disagreement %in% TRUE else rep(FALSE, n)
    return(df$class == "appropriate" & !dis)
  }
  if (identical(spec$kind, "ram_percent")) {
    if (!"consensus" %in% names(df)) return(rep(FALSE, n))
    return(df$consensus %in% spec$values)
  }
  rep(FALSE, n)
}
