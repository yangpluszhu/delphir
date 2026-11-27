#' 5、7、9 分李克特量表的自动预设
#'
#' 默认研究仍是 1–9。点数为 5、7 或 9 时，低/中/高区、共识中位数下限、
#' IQR 上限、稳定性中位数变化，以及 IPRAS 常数都由这一函数给出。
#' 其他点数返回 `auto = FALSE`，不另造区间。
#'
#' 区间以量表中点为中区中心，中区半径为 `floor((点数 - 1) / 6)`。
#' 因此 9 分为 1–3 / 4–6 / 7–9，7 分为 1–2 / 3–5 / 6–7，
#' 5 分为 1–2 / 3 / 4–5。后一档与核心技术要素的原有 5 分区间相同。
#'
#' 随跨度线性换算的量，在 9 分上与手册和本包原预设一致：
#' `iqr_max = 2 * span / 8`，`stability_median_delta = span / 8`，
#' `IPRr = 2.35 * span / 8`，`CFA = 1.5 * span / 8`。
#' IPRAS 的中心是量表中点，9 分仍是 5。
#' 9 人严格/宽松定义里的一致跨度是 `agree_width = 2 * span / 8`
#' （9 分时极差不超过 2，即任意 3 分）。
#' 核心技术要素的均数下限是「距下端 62.5% 跨度」，5 分仍是 3.5。
#'
#' 比例类阈值（高区比例、COMET 70/15、CV、极化）不在这里换算。
#'
#' @param scale_min,scale_max 量表端点。须 `scale_min < scale_max`。
#'
#' @return 列表。`auto` 为 TRUE 时 `points` 是 5、7 或 9，并含
#'   `low_range`、`mid_range`、`high_range`、`median_min`、`iqr_max`、
#'   `stability_median_delta`、`rand_iprr`、`rand_cfa`、`ipras_center`、
#'   `agree_width`、`mean_min`、`mean_delta_max`、`boundary`。
#'
#' @examples
#' likert_scale_preset(1, 9)$high_range
#' likert_scale_preset(1, 7)$low_range
#' likert_scale_preset(1, 5)$median_min
#' likert_scale_preset(1, 5)$rand_iprr
#'
#' @export
likert_scale_preset <- function(scale_min = 1, scale_max = 9) {
  scale_min <- as.numeric(scale_min)[1]
  scale_max <- as.numeric(scale_max)[1]
  .stop_if(
    length(scale_min) != 1L || length(scale_max) != 1L ||
      !is.finite(scale_min) || !is.finite(scale_max) || scale_min >= scale_max,
    "scale_min \u5fc5\u987b\u5c0f\u4e8e scale_max\u3002"
  )
  n <- .likert_point_count(scale_min, scale_max)
  auto <- !is.na(n) && n %in% c(5L, 7L, 9L)
  span <- scale_max - scale_min
  center <- (scale_min + scale_max) / 2
  if (auto) {
    mid_radius <- floor((n - 1) / 6)
    mid <- c(center - mid_radius, center + mid_radius)
    low <- c(scale_min, mid[1] - 1)
    high <- c(mid[2] + 1, scale_max)
  } else {
    low <- mid <- high <- c(NA_real_, NA_real_)
  }
  list(
    scale_min = scale_min,
    scale_max = scale_max,
    points = if (auto) n else NA_integer_,
    auto = auto,
    low_range = low,
    mid_range = mid,
    high_range = high,
    median_min = if (auto) high[1] else NA_real_,
    iqr_max = 2 * span / 8,
    stability_median_delta = span / 8,
    rand_iprr = 2.35 * span / 8,
    rand_cfa = 1.5 * span / 8,
    ipras_center = center,
    agree_width = 2 * span / 8,
    mean_min = scale_min + 0.625 * span,
    mean_delta_max = 0.25 * span / 4,
    boundary = if (auto) c(low[2] + 0.5, high[1] - 0.5) else c(NA_real_, NA_real_)
  )
}

.likert_point_count <- function(scale_min, scale_max) {
  n <- as.numeric(scale_max)[1] - as.numeric(scale_min)[1] + 1
  if (!is.finite(n) || abs(n - round(n)) > 1e-8) return(NA_integer_)
  as.integer(round(n))
}

.likert_num <- function(x) {
  x <- as.numeric(x)[1]
  if (length(x) != 1L || is.na(x)) return("NA")
  if (abs(x - round(x)) < 1e-8) as.character(as.integer(round(x))) else as.character(x)
}

.likert_band_label <- function(rng) {
  if (length(rng) < 2L || anyNA(rng)) return("\u2014")
  a <- .likert_num(rng[1])
  b <- .likert_num(rng[2])
  if (identical(a, b)) a else paste0(a, "\u2013", b)
}

.likert_zone_phrase <- function(protocol) {
  paste(
    .likert_band_label(protocol$low_range),
    .likert_band_label(protocol$mid_range),
    .likert_band_label(protocol$high_range),
    sep = " / "
  )
}

.collapse_likert_probs <- function(probs, points) {
  probs <- as.numeric(probs)
  .stop_if(length(probs) != 9L, "\u5185\u90e8\u6982\u7387\u5fc5\u987b\u662f 9 \u683c\u3002")
  points <- as.integer(points)[1]
  probs <- probs / sum(probs)
  if (points == 9L) return(probs)
  span <- points - 1
  to <- 1L + as.integer(floor((seq_len(9) - 1) / 8 * span + 0.5))
  to <- pmin(pmax(to, 1L), points)
  out <- numeric(points)
  for (i in seq_along(probs)) out[to[i]] <- out[to[i]] + probs[i]
  out / sum(out)
}

.likert_feedback_header <- function(protocol) {
  sprintf(
    "| \u60a8\u7684\u4e0a\u8f6e\u8bc4\u5206 | \u4e2d\u4f4d\u6570 | %s | %s | %s | \u5206\u6b67 | \u5206\u7c7b |\n",
    .likert_band_label(protocol$low_range),
    .likert_band_label(protocol$mid_range),
    .likert_band_label(protocol$high_range)
  )
}

.ipras_center <- function(scale_min, scale_max) {
  (as.numeric(scale_min)[1] + as.numeric(scale_max)[1]) / 2
}

.scale_is_19 <- function(scale_min, scale_max) {
  isTRUE(all.equal(as.numeric(c(scale_min, scale_max)), c(1, 9)))
}

.bands_are_19 <- function(protocol) {
  isTRUE(all.equal(as.numeric(protocol$low_range), c(1, 3))) &&
    isTRUE(all.equal(as.numeric(protocol$mid_range), c(4, 6))) &&
    isTRUE(all.equal(as.numeric(protocol$high_range), c(7, 9)))
}

.appropriateness_rule_line <- function(protocol, ram = FALSE) {
  if (isTRUE(.bands_are_19(protocol)) && !isTRUE(ram)) {
    return("\u002d **\u9002\u5b9c\u6027\uff1a** \u4e2d\u4f4d\u6570 7\u20139 \u4e14\u65e0\u5206\u6b67\u4e3a\u9002\u5b9c\uff1b1\u20133 \u4e14\u65e0\u5206\u6b67\u4e3a\u4e0d\u9002\u5b9c\uff1b\u4e2d\u4f4d\u6570 4\u20136\uff0c\u6216\u4efb\u4f55\u5206\u6b67\uff0c\u4e3a\u4e0d\u786e\u5b9a\u3002\u4e0d\u786e\u5b9a\u4f1a\u533a\u5206\u300c\u4e2d\u4f4d\u6570\u5c45\u4e2d\u300d\u4e0e\u300c\u5b58\u5728\u5206\u6b67\u300d\u3002")
  }
  if (isTRUE(.bands_are_19(protocol)) && isTRUE(ram)) {
    return("\u002d **\u9002\u5b9c\u6027\uff1a** \u4e2d\u4f4d\u6570 7\u20139 \u4e14\u65e0\u5206\u6b67\u4e3a\u9002\u5b9c\uff1b1\u20133 \u4e14\u65e0\u5206\u6b67\u4e3a\u4e0d\u9002\u5b9c\uff1b\u4e2d\u4f4d\u6570 4\u20136\uff0c\u6216\u4efb\u4f55\u5206\u6b67\uff0c\u4e3a\u4e0d\u786e\u5b9a\u3002\u4e2d\u4f4d\u6570 3.5 \u6216 6.5 \u9ed8\u8ba4\u4e3a\u4e0d\u786e\u5b9a\u3002\u5171\u8bc6\u7528\u8bed\u53ea\u662f\u8fd9\u4e9b\u7ed3\u679c\u7684\u62a5\u544a\u540d\u79f0\u3002\u201c\u65e0\u5206\u6b67\u201d\u4e0d\u7b49\u4e8e\u201c\u4e00\u81f4\u201d\u3002")
  }
  lo <- .likert_band_label(protocol$low_range)
  mid <- .likert_band_label(protocol$mid_range)
  hi <- .likert_band_label(protocol$high_range)
  b1 <- .likert_num(protocol$low_range[2] + 0.5)
  b2 <- .likert_num(protocol$high_range[1] - 0.5)
  sprintf(
    "- **\u9002\u5b9c\u6027\uff1a** \u4e2d\u4f4d\u6570 %s \u4e14\u65e0\u5206\u6b67\u4e3a\u9002\u5b9c\uff1b%s \u4e14\u65e0\u5206\u6b67\u4e3a\u4e0d\u9002\u5b9c\uff1b\u4e2d\u4f4d\u6570 %s\uff0c\u6216\u4efb\u4f55\u5206\u6b67\uff0c\u4e3a\u4e0d\u786e\u5b9a\u3002\u4e2d\u4f4d\u6570 %s \u6216 %s \u9ed8\u8ba4\u4e3a\u4e0d\u786e\u5b9a\u3002\u8fd9\u4e00\u5957\u533a\u95f4\u6309\u91cf\u8868\u81ea\u52a8\u6362\u7b97\uff0c\u6807\u4e3a RAM \u4fee\u6539\u7248\u3002",
    hi, lo, mid, b1, b2
  )
}

.necessity_rule_line <- function(protocol) {
  cut <- .likert_num(protocol$high_range[1])
  if (identical(cut, "7")) {
    return("\u002d **\u5fc5\u8981\u6027\uff1a** \u53ea\u8bc4\u4ef7\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u7684\u6761\u76ee\u3002\u5fc5\u8981\u6027\u4e2d\u4f4d\u6570 \u2265 7 \u4e14\u65e0\u5206\u6b67\u4e3a\u5fc5\u8981\uff0c\u5426\u5219\u4e3a\u9002\u5b9c\u4f46\u975e\u5fc5\u8981\u3002")
  }
  sprintf(
    "- **\u5fc5\u8981\u6027\uff1a** \u53ea\u8bc4\u4ef7\u9002\u5b9c\u4e14\u65e0\u5206\u6b67\u7684\u6761\u76ee\u3002\u5fc5\u8981\u6027\u4e2d\u4f4d\u6570 \u2265 %s \u4e14\u65e0\u5206\u6b67\u4e3a\u5fc5\u8981\uff0c\u5426\u5219\u4e3a\u9002\u5b9c\u4f46\u975e\u5fc5\u8981\u3002",
    cut
  )
}

.classic_agreement_text <- function(protocol, which = c("relaxed", "strict")) {
  which <- match.arg(which)
  if (isTRUE(.scale_is_19(protocol$scale_min, protocol$scale_max)) && identical(which, "relaxed")) {
    return("9 \u4eba\u5bbd\u677e\u5b9a\u4e49\uff1a\u4e24\u7aef\u5404\u81f3\u5c11 3 \u4eba\u4e3a\u5206\u6b67\uff1b\u5254\u9664\u4e00\u4e2a\u6700\u9ad8\u5206\u548c\u4e00\u4e2a\u6700\u4f4e\u5206\u540e\uff0c\u5176\u4f59\u5206\u6570\u843d\u5728\u4efb\u610f 3 \u5206\u8de8\u5ea6\u5185\u4e3a\u4e00\u81f4\u3002")
  }
  if (isTRUE(.scale_is_19(protocol$scale_min, protocol$scale_max)) && identical(which, "strict")) {
    return("9 \u4eba\u4e25\u683c\u5b9a\u4e49\uff1a\u4e24\u7aef\u5404\u81f3\u5c11 1 \u4eba\u4e3a\u5206\u6b67\uff1b\u5168\u90e8\u8bc4\u5206\u843d\u5728\u4efb\u610f 3 \u5206\u8de8\u5ea6\u5185\u4e3a\u4e00\u81f4\u3002")
  }
  width <- .likert_num(likert_scale_preset(protocol$scale_min, protocol$scale_max)$agree_width)
  if (identical(which, "relaxed")) {
    sprintf("9 \u4eba\u5bbd\u677e\u5b9a\u4e49\uff08\u6309\u91cf\u8868\u8de8\u5ea6\u6362\u7b97\uff0cRAM \u4fee\u6539\u7248\uff09\uff1a\u4e24\u7aef\u5404\u81f3\u5c11 3 \u4eba\u4e3a\u5206\u6b67\uff1b\u53bb\u6389\u4e00\u4e2a\u6700\u9ad8\u5206\u548c\u4e00\u4e2a\u6700\u4f4e\u5206\u540e\uff0c\u5176\u4f59\u5206\u6570\u6781\u5dee\u4e0d\u8d85\u8fc7 %s \u4e3a\u4e00\u81f4\u3002", width)
  } else {
    sprintf("9 \u4eba\u4e25\u683c\u5b9a\u4e49\uff08\u6309\u91cf\u8868\u8de8\u5ea6\u6362\u7b97\uff0cRAM \u4fee\u6539\u7248\uff09\uff1a\u4e24\u7aef\u5404\u81f3\u5c11 1 \u4eba\u4e3a\u5206\u6b67\uff1b\u5168\u90e8\u8bc4\u5206\u6781\u5dee\u4e0d\u8d85\u8fc7 %s \u4e3a\u4e00\u81f4\u3002", width)
  }
}

.ipras_rule_text <- function(protocol, iprr, cfa, qtype, ram = FALSE) {
  center <- .likert_num(.ipras_center(protocol$scale_min, protocol$scale_max))
  txt <- sprintf(
    "IPRAS\uff1aIPR = P70 - P30\uff0cIPRAS = %s + %s \u00d7 |%s - IPR \u4e2d\u5fc3|\u3002IPR \u4e25\u683c\u5927\u4e8e IPRAS \u624d\u7b97\u5206\u6b67\u3002\u5206\u4f4d\u6570 type = %s\u3002",
    iprr, cfa, center, qtype
  )
  if (isTRUE(ram) && isTRUE(.scale_is_19(protocol$scale_min, protocol$scale_max))) {
    txt <- paste0(txt, "\u6807\u51c6\u5e38\u6570\u53ea\u7528\u4e8e 1\u20139 \u5206\u3002")
  }
  if (isTRUE(protocol$rand$ipras_rescaled)) {
    txt <- paste0(
      txt,
      "\u5e38\u6570\u5df2\u6309\u91cf\u8868\u8de8\u5ea6\u4ece 1\u20139 \u5206\u7684 2.35 \u4e0e 1.5 \u7ebf\u6027\u6362\u7b97\uff0c\u4e2d\u5fc3\u6539\u4e3a\u91cf\u8868\u4e2d\u70b9\u3002\u8fd9\u4e0d\u662f Fitch \u624b\u518c\u539f\u503c\u3002"
    )
  }
  txt
}

.standard_scale_blurb <- function(protocol) {
  if (isTRUE(all.equal(c(protocol$scale_min, protocol$scale_max), c(1, 5)))) {
    paste0(
      sprintf("- **\u91cf\u8868\uff1a** %s\u2013%s \u5206\u3002\u91cd\u8981\u7a0b\u5ea6\uff1a1 \u5f88\u4e0d\u91cd\u8981\uff0c2 \u4e0d\u91cd\u8981\uff0c3 \u4e00\u822c\uff0c4 \u6bd4\u8f83\u91cd\u8981\uff0c5 \u5f88\u91cd\u8981\u3002",
              protocol$scale_min, protocol$scale_max),
      "\n\u719f\u6089\u7a0b\u5ea6\uff1a1 \u5f88\u4e0d\u719f\u6089\uff0c2 \u4e0d\u719f\u6089\uff0c3 \u4e00\u822c\uff0c4 \u6bd4\u8f83\u719f\u6089\uff0c5 \u5f88\u719f\u6089\u3002"
    )
  } else {
    sprintf(
      "- **\u91cf\u8868\uff1a** %s\u2013%s \u5206\u3002\u91cd\u8981\u7a0b\u5ea6\u4e0e\u719f\u6089\u7a0b\u5ea6\u5206\u5217\uff0c\u4e24\u7aef\u4e3a\u6781\u7aef\uff0c\u4e2d\u70b9\u4e3a\u4e00\u822c\u3002",
      protocol$scale_min, protocol$scale_max
    )
  }
}

.ipras_rescale_note <- function(protocol) {
  if (!isTRUE(protocol$rand$ipras_rescaled)) return(NULL)
  "\u002d **IPRAS \u5e38\u6570\uff1a** \u5df2\u6309\u91cf\u8868\u8de8\u5ea6\u4ece 1\u20139 \u5206\u7684 2.35 \u4e0e 1.5 \u7ebf\u6027\u6362\u7b97\uff0c\u4e2d\u5fc3\u6539\u4e3a\u91cf\u8868\u4e2d\u70b9\u3002\u8fd9\u4e0d\u662f Fitch \u624b\u518c\u539f\u503c\uff0c\u9002\u5b9c\u6027\u5224\u5b9a\u6807\u4e3a RAM \u4fee\u6539\u7248\u3002"
}
