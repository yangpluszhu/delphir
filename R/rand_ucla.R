# RAND/UCLA appropriateness rules (Fitch et al., 2001) --------------------

#' 按应答人数查 RAND 手册表 4 的阈值
#'
#' 一致：落在「包含中位数的三分区」之外的人数不超过该值。
#' 分歧：1–3 与 7–9 两端人数同时达到该值。
#' 8–10 人为区外不超过 2、两端各至少 3；此后每增加 3 人，两个阈值各加 1。
#' 7 人沿用 8–10 的阈值。超过 16 人按同一规律外推，并标明手册未正式表列。
#'
#' @param n 实际应答人数（不是名义邀请人数）。
#'
#' @return 一行数据框：`n`、`agreement_max`、`disagreement_min`、`band`、
#'   `extrapolated`、`small_panel`、`note`。
#'
#' @section 输入输出:
#' **输入**为单个正整数。
#'
#' **输出**一行。`agreement_max` 是区外人数上限（小于等于即一致）；
#' `disagreement_min` 是每一端至少要达到的人数。
#'
#' @section 文献:
#' Fitch K, et al. (2001) *The RAND/UCLA Appropriateness Method User's Manual*,
#' 表 4。
#'
#' @examples
#' rand_panel_thresholds(c(7, 9, 12, 15, 17))
#'
#' @export
rand_panel_thresholds <- function(n) {
  n <- as.integer(n)
  out <- lapply(n, .rand_threshold_one)
  do.call(rbind, out)
}

.rand_threshold_one <- function(n) {
  empty <- data.frame(
    n = n, agreement_max = NA_integer_, disagreement_min = NA_integer_,
    band = NA_character_, extrapolated = FALSE, small_panel = TRUE,
    note = "\u4eba\u6570\u65e0\u6548", stringsAsFactors = FALSE
  )
  if (length(n) != 1L || is.na(n) || n < 1L) return(empty)
  if (n < 8L) {
    return(data.frame(
      n = n, agreement_max = 2L, disagreement_min = 3L,
      band = if (n == 7L) "7_as_8-10" else "below_7",
      extrapolated = FALSE, small_panel = TRUE,
      note = if (n == 7L) {
        "7 \u4eba\u6cbf\u7528 8\u201310 \u4eba\u9608\u503c\uff08\u624b\u518c\u672a\u5355\u5217\uff09"
      } else {
        "\u5e94\u7b54\u4eba\u6570\u4e0d\u8db3 7\uff0c\u9608\u503c\u6cbf\u7528 8\u201310 \u4eba\uff0c\u7ed3\u679c\u4ec5\u4f9b\u63cf\u8ff0"
      },
      stringsAsFactors = FALSE
    ))
  }
  k <- (n - 8L) %/% 3L
  lo <- 8L + 3L * k
  hi <- lo + 2L
  extrapolated <- n > 16L
  data.frame(
    n = n,
    agreement_max = 2L + k,
    disagreement_min = 3L + k,
    band = sprintf("%s-%s", lo, hi),
    extrapolated = extrapolated,
    small_panel = FALSE,
    note = if (extrapolated) {
      "\u8d85\u8fc7 16 \u4eba\uff0c\u6309\u6bcf\u589e\u52a0 3 \u4eba\u9608\u503c\u52a0 1 \u5916\u63a8\uff0c\u624b\u518c\u672a\u6b63\u5f0f\u8868\u5217"
    } else {
      "\u624b\u518c\u8868 4"
    },
    stringsAsFactors = FALSE
  )
}

.rand_settings <- function(protocol) {
  r <- if (is.null(protocol$rand)) list() else protocol$rand
  rule <- r$rule %||% r$disagreement %||% "table4"
  if (identical(rule, "classic")) rule <- "table4"
  list(
    rule = rule,
    di_threshold = r$di_threshold %||% 1,
    boundary = r$boundary %||% "uncertain",
    iprr = r$iprr %||% 2.35,
    cfa = r$cfa %||% 1.5,
    qtype = as.integer(r$qtype %||% 7L),
    percent_high = r$percent_high %||% 0.70,
    percent_low = r$percent_low %||% 0.15,
    purpose = r$purpose %||% "appropriateness",
    round2_mode = r$round2_mode %||% "in_person",
    necessity = isTRUE(r$necessity),
    low = protocol$low_range %||% c(1, 3),
    mid = protocol$mid_range %||% c(4, 6),
    high = protocol$high_range %||% c(7, 9),
    scale = c(protocol$scale_min %||% 1, protocol$scale_max %||% 9)
  )
}

.rand_median_region <- function(med, low, mid, high, boundary) {
  if (is.na(med)) return(NA_character_)
  bounds <- c(low[2] + 0.5, high[1] - 0.5)
  on_bound <- any(abs(med - bounds) < 1e-8)
  if (isTRUE(on_bound) && identical(boundary, "uncertain")) return("boundary")
  if (isTRUE(on_bound) && identical(boundary, "round_half_down")) med <- med - 0.5
  if (isTRUE(on_bound) && identical(boundary, "round_half_up")) med <- med + 0.5
  if (med >= low[1] && med <= low[2]) return("low")
  if (med >= high[1] && med <= high[2]) return("high")
  if (med >= mid[1] && med <= mid[2]) return("mid")
  "mid"
}

.rand_in_band <- function(x, band) {
  !is.na(x) & x >= band[1] & x <= band[2]
}

.drop_one_extreme <- function(x) {
  x <- sort(x)
  if (length(x) <= 2L) return(x)
  x[-c(1L, length(x))]
}

.rand_empty_disagreement <- function() {
  data.frame(
    n_valid = 0L, median = NA_real_, p30 = NA_real_, p70 = NA_real_,
    ipr = NA_real_, iprcp = NA_real_, ai = NA_real_, ipras = NA_real_,
    di = NA_real_, n_low = 0L, n_mid = 0L, n_high = 0L, n_out = NA_integer_,
    agreement_max = NA_integer_, disagreement_min = NA_integer_,
    threshold_note = NA_character_, extrapolated = FALSE, small_panel = TRUE,
    table4_disagreement = NA, classic_relaxed_disagreement = NA,
    classic_strict_disagreement = NA, classic_disagreement = NA,
    ipras_disagreement = NA, agreement = NA, disagreement = NA,
    agreement_status = NA_character_, median_region = NA_character_,
    class = NA_character_, class_detail = NA_character_, class_zh = NA_character_,
    modified_ram = FALSE, nonstandard_ipras = FALSE, rule_used = NA_character_,
    stringsAsFactors = FALSE
  )
}

.rand_class_zh <- function(detail, class, method) {
  if (identical(method, "percent")) {
    return(ifelse(
      class == "appropriate", "\u8fbe\u6210\u5171\u8bc6\uff08RAM \u4fee\u6539\u7248\uff09",
      ifelse(class == "inappropriate", "\u503e\u5411\u4e0d\u91c7\u7eb3\uff08RAM \u4fee\u6539\u7248\uff09",
             "\u672a\u8fbe\u6210\u5171\u8bc6\uff08RAM \u4fee\u6539\u7248\uff09")
    ))
  }
  ifelse(
    detail == "uncertain_equipoise", "\u4e0d\u786e\u5b9a\uff08\u4e2d\u4f4d\u6570 4\u20136\uff0c\u65e0\u5206\u6b67\uff09",
    ifelse(detail == "uncertain_disagreement", "\u4e0d\u786e\u5b9a\uff08\u5b58\u5728\u5206\u6b67\uff09",
           ifelse(detail == "uncertain_boundary", "\u4e0d\u786e\u5b9a\uff08\u8fb9\u754c\u4e2d\u4f4d\u6570\uff09",
                  .label_zh(class)))
  )
}

#' 计算一条 RAND/UCLA 评分的分歧、一致与适宜性
#'
#' 同时给出手册表 4、9 人严格/宽松定义和 IPRAS。`method` 决定哪一套进入
#' `disagreement` 与 `class`。默认 `table4`。旧名 `classic` 与 `table4` 相同，
#' 不再使用 0.5.0 的 `ceiling(n/3)`。
#'
#' IPRAS：`IPR = P70 - P30`，`IPRAS = IPRr + CFA * |5 - (P30+P70)/2|`，
#' 默认 `IPRr = 2.35`、`CFA = 1.5`、分位数 `type = 7`。分歧为 **IPR > IPRAS**
#' （相等不算分歧）。IPRAS 本身不定义「一致」，一致状态沿用表 4 的区外人数。
#'
#' @param scores 同一条目的 1–9 评分向量。无法判断不要放进这个向量。
#' @param method `"table4"`（默认）、`"ipras"`、`"classic_relaxed"`、
#'   `"classic_strict"`、`"percent"`、`"both"` 或 `"classic"`（表 4 的别名）。
#'   `"both"` 表示表 4 与 IPRAS 任一判为分歧即分歧。
#' @param di_threshold 仅用于 IPRAS。分歧条件为 `IPR > IPRAS * di_threshold`，
#'   默认 1，即 IPR 严格大于 IPRAS。
#' @param warn 人数不足、外推或把 9 人定义用到其他人数时是否警告。
#' @param low,mid,high 三分区，默认 1–3、4–6、7–9。
#' @param iprr,cfa,qtype IPRAS 常数与分位数算法。偏离 2.35、1.5 或 type 7
#'   时记为非标准参数。
#' @param percent_high,percent_low 百分比规则：高区比例至少为 `percent_high`
#'   且低区比例低于 `percent_low` 才算一致。这是 RAM 修改版。
#' @param boundary 中位数恰为两个三分区的分界（默认 3.5 或 6.5）时如何处理。
#'   `"uncertain"` 归入不确定。
#' @param scale 量表端点。不是 1–9 时不得套用默认 IPRAS 常数。
#'
#' @return 一行数据框。主分类在 `class`（appropriate / uncertain / inappropriate）。
#'   `class_detail` 把不确定拆成中位数居中、存在分歧或边界中位数。
#'   `agreement_status` 为 agreement / indeterminate / disagreement。
#'
#' @section 输入输出:
#' **输入**为数值向量，一个条目、一轮、一个量表。缺失值会被去掉。
#'
#' **输出**始终包含 P30、P70、IPR、IPRAS、DI、两端人数、区外人数、所用阈值、
#' 各规则的分歧标记和最终分类，便于逐条复核。
#'
#' @section 文献:
#' Fitch 等（2001）第 8 章；9 人严格/宽松定义见手册表 3（早期研究与
#' Scott & Black, 1991）。百分比规则仅用于推荐意见，必须标明修改版。
#'
#' @examples
#' # 手册示例：高度一致，表 4 为一致，IPRAS 无分歧
#' rand_disagreement(c(6, 7, 7, 8, 8, 8, 9, 9, 9), method = "table4")
#' # 两端对拉：表 4 与 IPRAS 都判分歧
#' rand_disagreement(c(1, 2, 2, 3, 5, 7, 8, 9, 9), method = "both")
#' # 表 4 判分歧、IPRAS 不判分歧
#' d <- rand_disagreement(c(2, 3, 3, 7, 8, 8, 8, 9, 9), method = "table4")
#' d$table4_disagreement
#' d$ipras_disagreement
#'
#' @export
rand_disagreement <- function(scores,
                              method = c("table4", "ipras", "classic_relaxed",
                                         "classic_strict", "percent", "both", "classic"),
                              di_threshold = 1,
                              warn = TRUE,
                              low = c(1, 3),
                              mid = c(4, 6),
                              high = c(7, 9),
                              iprr = 2.35,
                              cfa = 1.5,
                              qtype = 7L,
                              percent_high = 0.70,
                              percent_low = 0.15,
                              boundary = c("uncertain", "round_half_down", "round_half_up"),
                              scale = c(1, 9)) {
  method <- match.arg(method)
  boundary <- match.arg(boundary)
  if (identical(method, "classic")) method <- "table4"
  qtype <- as.integer(qtype)[1]
  xs <- as.numeric(scores[!is.na(scores)])
  if (!length(xs)) return(.rand_empty_disagreement())

  standard_19 <- isTRUE(all.equal(scale, c(1, 9))) &&
    isTRUE(all.equal(low, c(1, 3))) &&
    isTRUE(all.equal(mid, c(4, 6))) &&
    isTRUE(all.equal(high, c(7, 9)))
  nonstandard <- !isTRUE(all.equal(iprr, 2.35)) ||
    !isTRUE(all.equal(cfa, 1.5)) || !identical(qtype, 7L)
  if (method %in% c("ipras", "both") && !standard_19 && !nonstandard) {
    stop(
      "IPRAS \u5e38\u6570 2.35 \u4e0e 1.5 \u53ea\u9002\u7528\u4e8e 1\u20139 \u5206\u91cf\u8868\u3002\u8bf7\u6539\u7528 percent\uff0c\u6216\u5728\u65b9\u6848\u4e2d\u5199\u660e\u975e\u6807\u51c6\u5e38\u6570\u5e76\u586b\u5199 rationale\u3002",
      call. = FALSE
    )
  }
  modified <- identical(method, "percent") || !standard_19 ||
    (method %in% c("ipras", "both") && nonstandard)
  if (isTRUE(warn) && !standard_19 && method %in% c("table4", "classic_relaxed", "classic_strict", "both")) {
    warning(
      "\u91cf\u8868\u4e0d\u662f\u6807\u51c6 1\u20139 \u4e09\u5206\u533a\u3002\u8868 4 / 9 \u4eba\u5b9a\u4e49\u5c06\u6309\u4f60\u7ed9\u51fa\u7684\u533a\u95f4\u8ba1\u7b97\uff0c\u5e76\u6807\u4e3a RAM \u4fee\u6539\u7248\u3002",
      call. = FALSE
    )
  }

  n <- length(xs)
  thr <- .rand_threshold_one(n)
  if (isTRUE(warn) && n < 7L) {
    warning("\u5e94\u7b54\u4eba\u6570\u4e0d\u8db3 7\uff0cRAND \u5206\u7c7b\u4e0d\u7a33\u5b9a\uff0c\u4ec5\u4f9b\u63cf\u8ff0\u3002", call. = FALSE)
  } else if (isTRUE(warn) && n == 7L) {
    warning("7 \u4eba\u5c0f\u7ec4\u6cbf\u7528 8\u201310 \u4eba\u7684\u8868 4 \u9608\u503c\uff0c\u62a5\u544a\u4e2d\u987b\u8bf4\u660e\u3002", call. = FALSE)
  } else if (isTRUE(warn) && isTRUE(thr$extrapolated)) {
    warning(thr$note, call. = FALSE)
  }
  if (isTRUE(warn) && method %in% c("classic_relaxed", "classic_strict") && n != 9L) {
    warning(
      "\u4e25\u683c/\u5bbd\u677e\u5b9a\u4e49\u662f 9 \u4eba\u5c0f\u7ec4\u7684\u624b\u518c\u8868 3\u3002\u5f53\u524d\u4eba\u6570\u4e0d\u662f 9\uff0c\u4ecd\u6309\u5b57\u9762\u89c4\u5219\u8ba1\u7b97\u5e76\u8b66\u544a\uff1b\u975e 9 \u4eba\u8bf7\u4f18\u5148\u4f7f\u7528 table4\u3002",
      call. = FALSE
    )
  }

  med <- as.numeric(stats::median(xs))
  q <- as.numeric(stats::quantile(xs, probs = c(0.30, 0.70), names = FALSE, type = qtype))
  p30 <- q[1]
  p70 <- q[2]
  ipr <- p70 - p30
  iprcp <- (p30 + p70) / 2
  ai <- abs(5 - iprcp)
  ipras <- iprr + cfa * ai
  di <- if (!is.finite(ipras) || ipras <= 0) Inf else ipr / ipras
  n_low <- sum(.rand_in_band(xs, low))
  n_mid <- sum(.rand_in_band(xs, mid))
  n_high <- sum(.rand_in_band(xs, high))
  region <- .rand_median_region(med, low, mid, high, boundary)
  band <- switch(region, low = low, mid = mid, high = high, NULL)
  n_out <- if (is.null(band)) NA_integer_ else sum(!.rand_in_band(xs, band))

  table4_dis <- n_low >= thr$disagreement_min && n_high >= thr$disagreement_min
  relaxed_dis <- n_low >= 3L && n_high >= 3L
  strict_dis <- n_low >= 1L && n_high >= 1L
  ipras_dis <- is.finite(ipr) && is.finite(ipras) && ipr > ipras * di_threshold
  percent_dis <- (n_low / n) >= percent_low && (n_high / n) >= percent_low
  table4_agr <- !is.na(n_out) && n_out <= thr$agreement_max
  strict_agr <- (max(xs) - min(xs)) <= 2
  y <- .drop_one_extreme(xs)
  relaxed_agr <- length(y) > 0L && (max(y) - min(y)) <= 2
  percent_agr <- (n_high / n) >= percent_high && (n_low / n) < percent_low

  disagreement <- switch(
    method,
    table4 = table4_dis,
    ipras = ipras_dis,
    classic_relaxed = relaxed_dis,
    classic_strict = strict_dis,
    percent = percent_dis,
    both = table4_dis || ipras_dis
  )
  agreement <- switch(
    method,
    table4 = table4_agr && !isTRUE(disagreement),
    ipras = table4_agr && !isTRUE(disagreement),
    classic_relaxed = relaxed_agr && !isTRUE(disagreement),
    classic_strict = strict_agr && !isTRUE(disagreement),
    percent = percent_agr && !isTRUE(disagreement),
    both = table4_agr && !isTRUE(disagreement)
  )
  status <- if (isTRUE(disagreement)) {
    "disagreement"
  } else if (isTRUE(agreement)) {
    "agreement"
  } else {
    "indeterminate"
  }

  if (is.na(med)) {
    class <- NA_character_
    detail <- NA_character_
  } else if (identical(region, "boundary")) {
    class <- "uncertain"
    detail <- "uncertain_boundary"
  } else if (isTRUE(disagreement)) {
    class <- "uncertain"
    detail <- "uncertain_disagreement"
  } else if (identical(region, "high")) {
    class <- "appropriate"
    detail <- "appropriate"
  } else if (identical(region, "low")) {
    class <- "inappropriate"
    detail <- "inappropriate"
  } else {
    class <- "uncertain"
    detail <- "uncertain_equipoise"
  }

  data.frame(
    n_valid = n,
    median = med,
    p30 = p30,
    p70 = p70,
    ipr = ipr,
    iprcp = iprcp,
    ai = ai,
    ipras = ipras,
    di = di,
    n_low = n_low,
    n_mid = n_mid,
    n_high = n_high,
    n_out = n_out,
    agreement_max = thr$agreement_max,
    disagreement_min = thr$disagreement_min,
    threshold_note = thr$note,
    extrapolated = thr$extrapolated,
    small_panel = thr$small_panel,
    table4_disagreement = table4_dis,
    classic_relaxed_disagreement = relaxed_dis,
    classic_strict_disagreement = strict_dis,
    classic_disagreement = table4_dis,
    ipras_disagreement = ipras_dis,
    agreement = agreement,
    disagreement = disagreement,
    agreement_status = status,
    median_region = region,
    class = class,
    class_detail = detail,
    class_zh = .rand_class_zh(detail, class, method),
    modified_ram = modified,
    nonstandard_ipras = nonstandard && method %in% c("ipras", "both"),
    rule_used = method,
    stringsAsFactors = FALSE
  )
}

#' 按锁定的 RAND 规则判定一轮适宜性
#'
#' 中位数 7–9 且无分歧为适宜；1–3 且无分歧为不适宜；中位数 4–6，或任何分歧，
#' 均为不确定。不确定会写成「中位数居中」或「存在分歧」，二者都是正式结果。
#' 必要性评分（维度名含「必要」）不在这里分类。
#'
#' @param study [delphi_study()] 对象。
#' @param round 轮次。默认该研究适宜性评分的最后一轮。
#' @param method 覆盖方案中的规则，仅用于敏感性分析，不改写已锁定方案。
#' @param summary 可选的 [summarize_items()] 结果。
#'
#' @return 带类 `delphi_rand` 的数据框。除描述统计外，含 `class`、
#'   `class_detail`、`class_zh`、`agreement_status`、`di`、`ipr`、`ipras`。
#'
#' @section 输入输出:
#' **输入**研究的 `ratings` 须含 `expert_id`、`item_id`、`round`、`score`。
#' 无法判断用 `unable = TRUE` 且 `score` 为缺失，不进入分母。
#'
#' **输出**每个条目（及维度）一行。`action` 为 `keep`（适宜或不适宜，作为结果保留）
#' 或 `discuss`（不确定）。
#'
#' @examples
#' ram <- simulate_ram_classic()
#' cls <- classify_rand(ram, round = 2)
#' table(cls$class_detail)
#'
#' @export
classify_rand <- function(study, round = NULL, method = NULL, summary = NULL) {
  study <- .match_study(study)
  p <- study$protocol
  settings <- .rand_settings(p)
  if (is.null(method)) {
    method <- settings$rule
  } else {
    method <- match.arg(
      method,
      c("table4", "ipras", "classic_relaxed", "classic_strict", "percent", "both", "classic")
    )
    if (identical(method, "classic")) method <- "table4"
  }
  if (is.null(summary)) summary <- summarize_items(study)
  if ("dimension" %in% names(summary)) {
    summary <- summary[!grepl("\u5fc5\u8981", summary$dimension), , drop = FALSE]
  }
  .stop_if(!nrow(summary), "\u6ca1\u6709\u53ef\u7528\u4e8e\u9002\u5b9c\u6027\u5206\u7c7b\u7684\u8bc4\u5206\u3002")
  if (is.null(round)) round <- max(summary$round)
  summary <- summary[summary$round == round, , drop = FALSE]
  .stop_if(!nrow(summary), sprintf("\u7b2c %s \u8f6e\u6ca1\u6709\u9002\u5b9c\u6027\u8bc4\u5206\u3002", round))
  if (round >= 3L && identical(settings$purpose, "appropriateness") &&
      identical(method, settings$rule)) {
    warning(
      "\u7ecf\u5178 RAM \u7684\u9002\u5b9c\u6027\u8bc4\u5206\u8d85\u8fc7 2 \u8f6e\u3002\u8fd9\u662f\u5bf9\u7ecf\u5178\u6d41\u7a0b\u7684\u504f\u79bb\uff0c\u62a5\u544a\u4e2d\u5fc5\u987b\u8bf4\u660e\u7406\u7531\u3002\u5fc5\u8981\u6027\u5e94\u4f7f\u7528\u5355\u72ec\u91cf\u8868\uff0c\u800c\u4e0d\u662f\u518d\u52a0\u4e00\u8f6e\u9002\u5b9c\u6027\u8bc4\u5206\u3002",
      call. = FALSE
    )
  }

  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[r$round == round & !grepl("\u5fc5\u8981", r$dimension), , drop = FALSE]
  extra <- lapply(seq_len(nrow(summary)), function(i) {
    sub <- r[r$item_id == summary$item_id[i] & r$dimension == summary$dimension[i], , drop = FALSE]
    xs <- .valid_scores(sub$score, sub$unable)
    rand_disagreement(
      xs, method = method, di_threshold = settings$di_threshold, warn = FALSE,
      low = settings$low, mid = settings$mid, high = settings$high,
      iprr = settings$iprr, cfa = settings$cfa, qtype = settings$qtype,
      percent_high = settings$percent_high, percent_low = settings$percent_low,
      boundary = settings$boundary, scale = settings$scale
    )
  })
  extra <- do.call(rbind, extra)
  if (any(extra$n_valid < 7L)) {
    warning(
      "\u81f3\u5c11\u4e00\u4e2a\u6761\u76ee\u7684\u6709\u6548\u5e94\u7b54\u4e0d\u8db3 7 \u4eba\u3002\u8868 4 \u9608\u503c\u4ecd\u6309 8\u201310 \u4eba\u6cbf\u7528\uff0c\u5206\u7c7b\u4ec5\u4f9b\u63cf\u8ff0\u3002",
      call. = FALSE
    )
  } else if (any(extra$n_valid == 7L)) {
    warning("\u5b58\u5728 7 \u4eba\u5e94\u7b54\u7684\u6761\u76ee\uff0c\u5df2\u6cbf\u7528 8\u201310 \u4eba\u9608\u503c\u3002", call. = FALSE)
  }
  if (any(extra$extrapolated)) {
    warning("\u6709\u6761\u76ee\u5e94\u7b54\u4eba\u6570\u8d85\u8fc7 16\uff0c\u9608\u503c\u4e3a\u5916\u63a8\uff0c\u62a5\u544a\u4e2d\u987b\u6807\u660e\u3002", call. = FALSE)
  }
  if (method %in% c("classic_relaxed", "classic_strict") && any(extra$n_valid != 9L)) {
    warning(
      "\u4e25\u683c\u6216\u5bbd\u677e\u5b9a\u4e49\u662f\u4e3a 9 \u4eba\u5c0f\u7ec4\u5199\u7684\u3002\u5f53\u524d\u6709\u6761\u76ee\u4eba\u6570\u4e0d\u662f 9\u3002",
      call. = FALSE
    )
  }

  keep <- setdiff(names(extra), names(summary))
  out <- cbind(summary, extra[, keep, drop = FALSE])
  if ("ipr" %in% names(extra)) out$ipr <- extra$ipr
  if (!"median" %in% names(out) && "median" %in% names(extra)) out$median <- extra$median
  out$action <- ifelse(out$class %in% c("appropriate", "inappropriate"), "keep", "discuss")
  out$action[is.na(out$class)] <- NA_character_
  out$rule_set <- "rand_ucla"
  rownames(out) <- NULL
  .add_class(out, "delphi_rand")
}
