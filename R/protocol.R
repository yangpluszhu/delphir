#' 创建并锁定 Delphi 统计分析方案
#'
#' 在第一轮调查开始前规定评分尺度、共识阈值、异议标准、稳定性标准和终止规则。
#' 分析函数只执行本对象中的规则，不会根据结果回改阈值。
#'
#' @param scenario 场景标识。可选 `"clinical_consensus"`、`"comet_cos"`、
#'   `"rand_ucla"`、`"indicator"`、`"ranking"`、`"definition"`、`"policy"`。
#' @param scale_min,scale_max 评分尺度端点，默认 1 与 9。
#' @param low_range,mid_range,high_range 长度为 2 的数值向量，分别表示低、中、高区。
#' @param high_pct 高度共识所需的高区比例下限（0–1），默认 0.80。
#' @param basic_pct 基本共识所需的高区比例下限（0–1），默认 0.75。
#' @param median_min 共识所需的中位数下限，默认 7。
#' @param iqr_max 高度共识允许的最大四分位距，默认 2。
#' @param reject_pct 倾向排除所需的低区比例下限，默认 0.75。
#' @param comet_in_high COMET 纳入规则：高区比例下限，默认 0.70。
#' @param comet_in_low_max COMET 纳入规则：低区比例上限，默认 0.15。
#' @param comet_out_low COMET 排除规则：低区比例下限，默认 0.70。
#' @param comet_out_high_max COMET 排除规则：高区比例上限，默认 0.15。
#' @param rand_disagreement RAND 分歧定义：`"both"`（同时报告经典规则与 IPRAS）、
#'   `"classic"` 或 `"ipras"`。
#' @param rand_di_threshold 分歧指数阈值，默认 1。
#' @param max_rounds 最多轮次，默认 3。
#' @param stability_class_pct 相邻轮分类不变条目的比例阈值，默认 0.80。
#' @param stability_median_delta 中位数变化不超过该值视为稳定，默认 1。
#' @param stability_high_pct_delta 高区比例变化不超过该值视为稳定，默认 0.10。
#' @param polarization_threshold 两端比例同时达到该值视为两极分化，默认 0.25。
#' @param cv_max 指标体系筛选时允许的最大变异系数，默认 0.25。
#' @param ranking_topk 优先级分析中的 Top-k，默认 5。
#' @param rationale 若修改了预设阈值，必须说明理由；报告会打印“方案偏离预设”。
#' @param locked 是否锁定阈值。默认 `TRUE`。
#'
#' @return `delphi_protocol` 对象。
#'
#' @section 输入输出:
#' **输入**均为标量或长度为 2 的数值向量，见参数说明。
#'
#' **输出**是带类 `"delphi_protocol"` 的列表，主要字段：
#' * `scenario`, `scale_min`, `scale_max`, `low_range`, `mid_range`, `high_range`
#' * `rules`：共识判定阈值
#' * `stability`：停止规则
#' * `rand`：RAND/UCLA 分歧设定
#' * `modified_from_preset`, `rationale`, `locked`
#'
#' @examples
#' p <- preset_protocol("clinical_consensus")
#' p
#' p2 <- delphi_protocol(
#'   scenario = "clinical_consensus",
#'   high_pct = 0.85,
#'   rationale = "高风险诊疗推荐，将高度共识阈值提高到 85%。"
#' )
#' p2$modified_from_preset
#'
#' @export
delphi_protocol <- function(
    scenario = c(
      "clinical_consensus", "comet_cos", "rand_ucla",
      "indicator", "ranking", "definition", "policy"
    ),
    scale_min = 1,
    scale_max = 9,
    low_range = c(1, 3),
    mid_range = c(4, 6),
    high_range = c(7, 9),
    high_pct = 0.80,
    basic_pct = 0.75,
    median_min = 7,
    iqr_max = 2,
    reject_pct = 0.75,
    comet_in_high = 0.70,
    comet_in_low_max = 0.15,
    comet_out_low = 0.70,
    comet_out_high_max = 0.15,
    rand_disagreement = c("both", "classic", "ipras"),
    rand_di_threshold = 1,
    max_rounds = 3,
    stability_class_pct = 0.80,
    stability_median_delta = 1,
    stability_high_pct_delta = 0.10,
    polarization_threshold = 0.25,
    cv_max = 0.25,
    ranking_topk = 5,
    rationale = NULL,
    locked = TRUE
) {
  scenario <- match.arg(scenario)
  rand_disagreement <- match.arg(rand_disagreement)
  .stop_if(scale_min >= scale_max, "scale_min \u5fc5\u987b\u5c0f\u4e8e scale_max\u3002")
  .stop_if(length(low_range) != 2L || length(high_range) != 2L, "\u533a\u95f4\u53c2\u6570\u5fc5\u987b\u662f\u957f\u5ea6 2 \u7684\u5411\u91cf\u3002")
  .stop_if(any(c(high_pct, basic_pct, reject_pct) < 0 | c(high_pct, basic_pct, reject_pct) > 1),
           "\u6bd4\u4f8b\u9608\u503c\u5fc5\u987b\u5728 0 \u4e0e 1 \u4e4b\u95f4\u3002")
  if (scenario %in% c("clinical_consensus", "definition") && is.null(rationale)) {
    defaults <- .preset_defaults(scenario)
    if (!isTRUE(all.equal(high_pct, defaults$high_pct)) ||
        !isTRUE(all.equal(basic_pct, defaults$basic_pct)) ||
        !isTRUE(all.equal(median_min, defaults$median_min)) ||
        !isTRUE(all.equal(iqr_max, defaults$iqr_max))) {
      warning(
        "\u4fee\u6539\u4e86\u9884\u8bbe\u9608\u503c\u4f46\u672a\u586b\u5199 rationale\u3002\u62a5\u544a\u5c06\u6807\u8bb0\u4e3a\u201c\u65b9\u6848\u504f\u79bb\u9884\u8bbe\u4e14\u7406\u7531\u7f3a\u5931\u201d\u3002",
        call. = FALSE
      )
    }
  }

  defaults <- .preset_defaults(scenario)
  modified <- !isTRUE(all.equal(
    list(high_pct, basic_pct, median_min, iqr_max, reject_pct,
         comet_in_high, comet_in_low_max, comet_out_low, comet_out_high_max,
         rand_di_threshold, cv_max),
    list(defaults$high_pct, defaults$basic_pct, defaults$median_min, defaults$iqr_max,
         defaults$reject_pct, defaults$comet_in_high, defaults$comet_in_low_max,
         defaults$comet_out_low, defaults$comet_out_high_max,
         defaults$rand_di_threshold, defaults$cv_max)
  ))

  structure(
    list(
      scenario = scenario,
      scale_min = as.numeric(scale_min),
      scale_max = as.numeric(scale_max),
      low_range = as.numeric(low_range),
      mid_range = as.numeric(mid_range),
      high_range = as.numeric(high_range),
      rules = list(
        high_pct = high_pct,
        basic_pct = basic_pct,
        median_min = median_min,
        iqr_max = iqr_max,
        reject_pct = reject_pct,
        comet_in_high = comet_in_high,
        comet_in_low_max = comet_in_low_max,
        comet_out_low = comet_out_low,
        comet_out_high_max = comet_out_high_max,
        polarization_threshold = polarization_threshold,
        cv_max = cv_max,
        ranking_topk = as.integer(ranking_topk)
      ),
      stability = list(
        max_rounds = as.integer(max_rounds),
        class_pct = stability_class_pct,
        median_delta = stability_median_delta,
        high_pct_delta = stability_high_pct_delta
      ),
      rand = list(
        disagreement = rand_disagreement,
        di_threshold = rand_di_threshold
      ),
      rationale = rationale,
      modified_from_preset = modified,
      locked = isTRUE(locked)
    ),
    class = "delphi_protocol"
  )
}

.preset_defaults <- function(scenario) {
  switch(
    scenario,
    clinical_consensus = list(
      high_pct = 0.80, basic_pct = 0.75, median_min = 7, iqr_max = 2,
      reject_pct = 0.75, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    ),
    comet_cos = list(
      high_pct = 0.70, basic_pct = 0.70, median_min = 7, iqr_max = 2,
      reject_pct = 0.70, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    ),
    rand_ucla = list(
      high_pct = NA_real_, basic_pct = NA_real_, median_min = 7, iqr_max = 2,
      reject_pct = NA_real_, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    ),
    indicator = list(
      high_pct = 0.75, basic_pct = 0.70, median_min = 7, iqr_max = 2,
      reject_pct = 0.75, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    ),
    ranking = list(
      high_pct = NA_real_, basic_pct = NA_real_, median_min = NA_real_, iqr_max = NA_real_,
      reject_pct = NA_real_, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    ),
    definition = list(
      high_pct = 0.80, basic_pct = 0.80, median_min = 7, iqr_max = 2,
      reject_pct = 0.75, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    ),
    policy = list(
      high_pct = NA_real_, basic_pct = NA_real_, median_min = NA_real_, iqr_max = NA_real_,
      reject_pct = NA_real_, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    )
  )
}

#' 按场景取出预设分析方案
#'
#' 返回与场景匹配的默认 `delphi_protocol`。医学专家共识使用复合标准
#' （高区比例 + 中位数 + IQR）；COS 使用 COMET 70/15 规则；RAND/UCLA
#' 使用 Median 区间加分歧指数。
#'
#' @param scenario 场景标识，见 [delphi_protocol()]。
#' @param ... 覆盖预设的参数，若改变阈值请同时提供 `rationale`。
#'
#' @return `delphi_protocol` 对象。
#'
#' @examples
#' preset_protocol("comet_cos")
#' preset_protocol("rand_ucla")
#'
#' @export
preset_protocol <- function(scenario = "clinical_consensus", ...) {
  dots <- list(...)
  args <- c(list(scenario = scenario), dots)
  do.call(delphi_protocol, args)
}

#' @export
print.delphi_protocol <- function(x, ...) {
  cat("Delphi \u5206\u6790\u65b9\u6848\n")
  cat(sprintf("  \u573a\u666f: %s (%s)\n", x$scenario, .scenario_label(x$scenario)))
  cat(sprintf("  \u91cf\u8868: %s\u2013%s  (\u4f4e %s\u2013%s / \u4e2d %s\u2013%s / \u9ad8 %s\u2013%s)\n",
              x$scale_min, x$scale_max,
              x$low_range[1], x$low_range[2],
              x$mid_range[1], x$mid_range[2],
              x$high_range[1], x$high_range[2]))
  cat(sprintf("  \u9501\u5b9a: %s\n", if (x$locked) "\u662f" else "\u5426"))
  if (isTRUE(x$modified_from_preset)) {
    cat("  \u5df2\u504f\u79bb\u9884\u8bbe")
    if (!is.null(x$rationale)) cat(sprintf("\uff1b\u7406\u7531: %s", x$rationale))
    cat("\n")
  }
  invisible(x)
}
