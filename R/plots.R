.theme_delphi <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

.ratings_df <- function(study, round = NULL, dimension = NULL) {
  study <- .match_study(study)
  r <- .ensure_unable(.ensure_dimension(study$ratings))
  r <- r[!r$unable & !is.na(r$score), ]
  if (!is.null(round)) r <- r[r$round %in% round, ]
  if (!is.null(dimension)) r <- r[r$dimension %in% dimension, ]
  r$item_text <- study$items$item_text[match(r$item_id, study$items$item_id)]
  r
}

#' 条目评分分布图
#'
#' @param study [delphi_study()] 或分析结果。
#' @param item_id 条目 ID，缺省时画全部条目的箱线图。
#' @param round,dimension 筛选。
#'
#' @return ggplot 对象。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 12, n_items = 6, seed = 18)
#' plot_item_distribution(study, round = 1)
#'
#' @export
plot_item_distribution <- function(study, item_id = NULL, round = NULL,
                                   dimension = NULL) {
  r <- .ratings_df(study, round, dimension)
  if (!is.null(item_id)) {
    r <- r[r$item_id %in% item_id, ]
    ggplot2::ggplot(r, ggplot2::aes(.data$score)) +
      ggplot2::geom_bar(width = 0.7, fill = "#3C6E71") +
      ggplot2::facet_wrap(~item_id, scales = "free_y") +
      ggplot2::scale_x_continuous(breaks = 1:9) +
      ggplot2::labs(title = "\u6761\u76ee\u8bc4\u5206\u5206\u5e03", x = "\u8bc4\u5206", y = "\u4eba\u6570") +
      .theme_delphi()
  } else {
    r$item_id <- factor(r$item_id, levels = unique(r$item_id))
    ggplot2::ggplot(r, ggplot2::aes(.data$item_id, .data$score)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.4, fill = "#D9E4DD") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "\u5404\u6761\u76ee\u8bc4\u5206\u5206\u5e03", x = NULL, y = "\u8bc4\u5206") +
      .theme_delphi()
  }
}

#' 轮次间中位数或高区比例变化
#'
#' @param study 研究或分析结果。
#' @param metric `"median"` 或 `"p_high"`。
#'
#' @return ggplot 对象。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 14, n_items = 8, n_rounds = 2, seed = 19)
#' plot_round_shift(study)
#'
#' @export
plot_round_shift <- function(study, metric = c("median", "p_high")) {
  metric <- match.arg(metric)
  if (inherits(study, "delphi_result")) {
    s <- study$item_summary
  } else {
    s <- summarize_items(study)
  }
  dim0 <- unique(s$dimension)[1]
  s <- s[s$dimension == dim0, ]
  y <- if (metric == "median") s$median else s$p_high
  ylab <- if (metric == "median") "Median" else "\u9ad8\u533a\u6bd4\u4f8b (%)"
  s$y <- y
  ggplot2::ggplot(s, ggplot2::aes(.data$round, .data$y, group = .data$item_id,
                                  colour = .data$item_id)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "\u8f6e\u6b21\u95f4\u53d8\u5316", x = "\u8f6e\u6b21", y = ylab, colour = "\u6761\u76ee") +
    .theme_delphi() +
    ggplot2::theme(legend.position = "none")
}

#' 利益相关者高区比例对比
#'
#' @param study 研究或 [stakeholder_stratify()] 结果。
#' @param round 轮次。
#'
#' @return ggplot 对象。
#'
#' @examples
#' cos <- simulate_cos(n_clinicians = 10, n_patients = 8, n_researchers = 6,
#'                     n_items = 6, seed = 20)
#' plot_stakeholder(cos)
#'
#' @export
plot_stakeholder <- function(study, round = NULL) {
  if (inherits(study, "delphi_stakeholder")) {
    st <- study
  } else {
    st <- stakeholder_stratify(study, round = round)
  }
  ggplot2::ggplot(st, ggplot2::aes(.data$item_id, .data$p_high, fill = .data$group_level)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "\u5404\u5229\u76ca\u76f8\u5173\u8005\u9ad8\u533a\u6bd4\u4f8b", x = NULL, y = "\u9ad8\u533a\u6bd4\u4f8b (%)",
                  fill = "\u7fa4\u4f53") +
    .theme_delphi()
}

#' RAND/UCLA 面板：Median 与分歧指数
#'
#' @param study RAND 研究或 [classify_rand()] 结果。
#' @param round 轮次。
#'
#' @return ggplot 对象。
#'
#' @examples
#' ram <- simulate_rand_ucla(n_items = 12, seed = 21)
#' plot_rand_panel(ram)
#'
#' @export
plot_rand_panel <- function(study, round = NULL) {
  if (inherits(study, "delphi_rand")) {
    d <- study
  } else {
    d <- classify_rand(study, round = round)
  }
  d$item_id <- factor(d$item_id, levels = rev(unique(d$item_id)))
  ggplot2::ggplot(d, ggplot2::aes(.data$median, .data$item_id, colour = .data$class_zh)) +
    ggplot2::geom_vline(xintercept = c(3.5, 6.5), linetype = "dashed", colour = "grey60") +
    ggplot2::geom_point(ggplot2::aes(size = pmax(d$di, 0.05))) +
    ggplot2::scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
    ggplot2::labs(title = "RAND/UCLA \u9002\u5b9c\u6027\uff08\u70b9\u5927\u5c0f\u53cd\u6620 DI\uff09",
                  x = "Median", y = NULL, colour = "\u5206\u7c7b", size = "DI") +
    .theme_delphi()
}

#' 专家参与/脱落图
#'
#' @param study 研究或分析结果。
#'
#' @return ggplot 对象。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 18, n_items = 5, n_rounds = 2, seed = 22)
#' plot_attrition(study)
#'
#' @export
plot_attrition <- function(study) {
  if (inherits(study, "delphi_result")) {
    rr <- study$participation$by_round
  } else {
    rr <- response_summary(study)$by_round
  }
  ggplot2::ggplot(rr, ggplot2::aes(factor(.data$round), .data$n_responded)) +
    ggplot2::geom_col(fill = "#284B63", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$response_rate)),
                       vjust = -0.4, size = 3.5) +
    ggplot2::labs(title = "\u5404\u8f6e\u5e94\u7b54\u4eba\u6570\u4e0e\u56de\u6536\u7387", x = "\u8f6e\u6b21", y = "\u5e94\u7b54\u4eba\u6570") +
    .theme_delphi()
}
