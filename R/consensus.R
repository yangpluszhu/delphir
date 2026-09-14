#' 按预设方案判定条目共识类别
#'
#' 只执行 [delphi_protocol()] 中已锁定的规则，不会根据当前结果改阈值。
#'
#' @param summary [summarize_items()] 结果。
#' @param protocol [delphi_protocol()] 对象。若 `summary` 来自完整分析可省略。
#' @param study 可选，用于取出 protocol。
#'
#' @return 在 `summary` 上增加 `class`、`class_zh`、`action` 列。
#'
#' @section 判定规则:
#' * 医学共识：高度 = P(高区)≥80% 且 Median≥7 且 IQR≤2；
#'   基本 = 75%≤P(高区)<80% 且 Median≥7；排除 = P(低区)≥75%；
#'   两极分化单独标记。
#' * COS：COMET 70/15 规则。
#' * 定义共识：默认 P(高区)≥80% 且 IQR≤2。
#' * 指标体系：高区达基本阈值且 IQR、CV 可接受则保留。
#' * Policy：不判定“必须共识”，只描述分布。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 14, n_items = 8, seed = 5)
#' s <- summarize_items(study, rounds = 2)
#' cls <- classify_consensus(s, study$protocol)
#' table(cls$class_zh)
#'
#' @export
classify_consensus <- function(summary, protocol = NULL, study = NULL) {
  if (is.null(protocol)) {
    if (!is.null(study)) protocol <- .match_protocol(study)
    else stop("\u8bf7\u63d0\u4f9b protocol \u6216 study\u3002", call. = FALSE)
  }
  .stop_if(!inherits(protocol, "delphi_protocol"), "protocol \u7c7b\u578b\u4e0d\u6b63\u786e\u3002")
  .stop_if(!"p_high" %in% names(summary), "summary \u7f3a\u5c11 p_high\uff0c\u8bf7\u5148\u8c03\u7528 summarize_items()\u3002")

  scenario <- protocol$scenario
  cls <- switch(
    scenario,
    clinical_consensus = .class_clinical(summary, protocol),
    definition = .class_definition(summary, protocol),
    comet_cos = .class_comet(summary, protocol),
    indicator = .class_indicator(summary, protocol),
    policy = .class_policy(summary, protocol),
    ranking = .class_policy(summary, protocol),
    rand_ucla = {
      warning("RAND/UCLA \u8bf7\u4f7f\u7528 classify_rand()\u3002\u6b64\u5904\u6309 Median \u533a\u95f4\u7ed9\u51fa\u521d\u6b65\u6807\u7b7e\uff0c\u672a\u7eb3\u5165\u5206\u6b67\u3002",
              call. = FALSE)
      .class_rand_median_only(summary)
    },
    .class_clinical(summary, protocol)
  )
  summary$class <- cls$class
  summary$class_zh <- .label_zh(cls$class)
  summary$action <- cls$action
  summary$rule_set <- scenario
  .add_class(summary, "delphi_classification")
}

.class_clinical <- function(s, p) {
  r <- p$rules
  n <- nrow(s)
  class <- rep("no_consensus", n)
  action <- rep("discuss", n)
  ph <- s$p_high / 100
  pl <- s$p_low / 100
  for (i in seq_len(n)) {
    if (isTRUE(s$polarized[i])) {
      class[i] <- "polarized"
      action[i] <- "discuss"
    } else if (!is.na(pl[i]) && pl[i] >= r$reject_pct) {
      class[i] <- "reject"
      action[i] <- "drop"
    } else if (!is.na(ph[i]) && ph[i] >= r$high_pct &&
               !is.na(s$median[i]) && s$median[i] >= r$median_min &&
               !is.na(s$iqr[i]) && s$iqr[i] <= r$iqr_max) {
      class[i] <- "high_consensus"
      action[i] <- "keep"
    } else if (!is.na(ph[i]) && ph[i] >= r$basic_pct &&
               !is.na(s$median[i]) && s$median[i] >= r$median_min) {
      class[i] <- "basic_consensus"
      action[i] <- "keep"
    } else if (!is.na(ph[i]) && ph[i] >= 0.70 && ph[i] < r$basic_pct) {
      class[i] <- "borderline"
      action[i] <- "discuss"
    } else {
      class[i] <- "no_consensus"
      action[i] <- "discuss"
    }
  }
  list(class = class, action = action)
}

.class_definition <- function(s, p) {
  r <- p$rules
  class <- ifelse(
    !is.na(s$p_high) & s$p_high / 100 >= r$high_pct &
      !is.na(s$median) & s$median >= r$median_min &
      !is.na(s$iqr) & s$iqr <= r$iqr_max,
    "high_consensus",
    "no_consensus"
  )
  class[isTRUE(s$polarized)] <- "polarized"
  action <- ifelse(class == "high_consensus", "keep", "discuss")
  list(class = class, action = action)
}

.class_comet <- function(s, p) {
  r <- p$rules
  ph <- s$p_high / 100
  pl <- s$p_low / 100
  class <- rep("no_consensus", nrow(s))
  in_ok <- !is.na(ph) & !is.na(pl) & ph >= r$comet_in_high & pl < r$comet_in_low_max
  out_ok <- !is.na(ph) & !is.na(pl) & pl >= r$comet_out_low & ph < r$comet_out_high_max
  class[in_ok] <- "consensus_in"
  class[out_ok] <- "consensus_out"
  action <- ifelse(class == "consensus_in", "keep",
                   ifelse(class == "consensus_out", "drop", "discuss"))
  list(class = class, action = action)
}

.class_indicator <- function(s, p) {
  r <- p$rules
  keep <- !is.na(s$p_high) & s$p_high / 100 >= r$basic_pct &
    !is.na(s$median) & s$median >= r$median_min &
    !is.na(s$iqr) & s$iqr <= r$iqr_max &
    (is.na(s$cv) | s$cv <= r$cv_max)
  class <- ifelse(keep, "keep", "discuss")
  class[isTRUE(s$polarized)] <- "polarized"
  action <- ifelse(class == "keep", "keep", "discuss")
  list(class = class, action = action)
}

.class_policy <- function(s, p) {
  list(class = rep("no_consensus", nrow(s)), action = rep("discuss", nrow(s)))
}

.class_rand_median_only <- function(s) {
  class <- ifelse(is.na(s$median), NA_character_,
                  ifelse(s$median <= 3, "inappropriate",
                         ifelse(s$median >= 7, "appropriate", "uncertain")))
  list(class = class, action = rep("discuss", nrow(s)))
}
