# 标准核心技术要素（5 分量表）-------------------------------------------

.is_standard_scenario <- function(protocol) {
  !is.null(protocol) && identical(protocol$scenario, "standard_element")
}

.is_open_round <- function(protocol, round) {
  .is_standard_scenario(protocol) &&
    isTRUE(protocol$open_first) &&
    as.integer(round) == 1L
}

.is_first_scoring_round <- function(protocol, round) {
  round <- as.integer(round)
  if (.is_standard_scenario(protocol) && isTRUE(protocol$open_first)) {
    return(round == 2L)
  }
  round == 1L
}

#' 满分比
#'
#' 满分比 = 给出量表最高分的人数 / 有效评分人数。5 分量表上即打 5 分的比例。
#' 「无法判断」和缺失不进入分母。返回值与 [summarize_items()] 的 `p_high`
#' 一样，是 0–100 的百分数，不是 0–1 的比例。
#'
#' @param x 数值向量，一名专家一行的评分；或已经是单个条目的分数。
#' @param scale_max 满分。5 分量表为 5。也可传入 [delphi_protocol()]，
#'   此时改用其 `scale_max`。
#'
#' @return 一个百分数。没有有效评分时为 `NA`。
#'
#' @examples
#' full_score_ratio(c(5, 5, 4, 5, 3), scale_max = 5)
#'
#' @export
full_score_ratio <- function(x, scale_max = 5) {
  if (inherits(scale_max, "delphi_protocol")) scale_max <- scale_max$scale_max
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  scale_max <- as.numeric(scale_max)[1]
  .stop_if(length(scale_max) != 1L || is.na(scale_max), "scale_max \u5fc5\u987b\u662f\u4e00\u4e2a\u6570\u5b57\u3002")
  if (!length(x)) return(NA_real_)
  .pct(sum(x == scale_max), length(x))
}

.freq_table <- function(x, label) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "\u672a\u586b\u5199"
  tab <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(tab) <- c(label, "n")
  tab$percent <- round(.pct(tab$n, sum(tab$n)), 1)
  tab[order(-tab$n), , drop = FALSE]
}

.age_band <- function(age) {
  cut(
    as.numeric(age),
    breaks = c(-Inf, 40, 50, 60, Inf),
    right = FALSE,
    labels = c("40\u5c81\u4ee5\u4e0b", "40\u201349\u5c81", "50\u201359\u5c81", "60\u5c81\u53ca\u4ee5\u4e0a")
  )
}

#' 专家水平的描述性分析
#'
#' 汇总性别、年龄、学历、专业、职称、工作年限，以及对评价领域的熟悉程度。
#' 这是技术要求里的「调查专家水平」，不是条目共识。
#'
#' @param panel [delphi_panel()] 或专家数据框。若给出 `study`，则改用其中的专家表。
#' @param study 可选的 [delphi_study()]。
#'
#' @return 列表。`tables` 为各类频数表；`age`、`years` 为均数和标准差；
#'   `missing` 列出没有收集到的项目。
#'
#' @examples
#' study <- simulate_standard_delphi(seed = 2026)
#' prof <- expert_profile(study = study)
#' prof$age
#' prof$tables$sex
#'
#' @export
expert_profile <- function(panel = NULL, study = NULL) {
  if (!is.null(study)) panel <- .match_study(study)$panel
  .stop_if(is.null(panel) || !nrow(panel), "\u6ca0\u6709\u4e13\u5bb6\u8868\u3002")
  panel <- .as_df(panel)
  take <- function(col, label) {
    if (!col %in% names(panel)) return(NULL)
    .freq_table(panel[[col]], label)
  }
  num_summ <- function(col) {
    if (!col %in% names(panel)) return(NULL)
    x <- as.numeric(panel[[col]])
    x <- x[!is.na(x)]
    if (!length(x)) return(NULL)
    data.frame(
      n = length(x),
      mean = mean(x),
      sd = stats::sd(x),
      min = min(x),
      max = max(x),
      stringsAsFactors = FALSE
    )
  }
  age_bands <- NULL
  if ("age" %in% names(panel) && any(!is.na(panel$age))) {
    age_bands <- .freq_table(.age_band(panel$age), "age_band")
  }
  wanted <- c(
    sex = "\u6027\u522b", age = "\u5e74\u9f84", education = "\u5b66\u5386",
    specialty = "\u4e13\u4e1a", title = "\u804c\u79f0", title_level = "\u804c\u79f0\u7ea7\u522b",
    years = "\u5de5\u4f5c\u5e74\u9650", department = "\u79d1\u5ba4",
    region = "\u5730\u57df", familiarity = "\u5bf9\u8bc4\u4ef7\u9886\u57df\u7684\u719f\u6089\u7a0b\u5ea6"
  )
  missing <- wanted[!names(wanted) %in% names(panel)]
  list(
    n = nrow(panel),
    tables = list(
      sex = take("sex", "\u6027\u522b"),
      education = take("education", "\u5b66\u5386"),
      specialty = take("specialty", "\u4e13\u4e1a"),
      title = take("title", "\u804c\u79f0"),
      title_level = take("title_level", "\u804c\u79f0\u7ea7\u522b"),
      department = take("department", "\u79d1\u5ba4"),
      region = take("region", "\u5730\u57df"),
      familiarity = take("familiarity", "\u719f\u6089\u7a0b\u5ea6"),
      in_expert_group = if ("in_expert_group" %in% names(panel)) {
        .freq_table(ifelse(panel$in_expert_group, "\u4e13\u5bb6\u7ec4\u5185", "\u4e13\u5bb6\u7ec4\u5916"), "\u6765\u6e90")
      } else NULL,
      age_band = age_bands
    ),
    age = num_summ("age"),
    years = num_summ("years"),
    missing = unname(missing),
    note = paste0(
      "\u8fd9\u4e9b\u6570\u5b57\u63cf\u8ff0\u7684\u662f\u300c\u8c01\u5728\u8bc4\u300d\uff0c\u4e0d\u662f\u300c\u54ea\u4e00\u6761\u8981\u7d20\u88ab\u8ba4\u53ef\u300d\u3002",
      "\u719f\u6089\u7a0b\u5ea6\u5728\u8fd9\u91cc\u6307\u4e13\u5bb6\u5bf9\u6574\u4e2a\u8bc4\u4ef7\u9886\u57df\u7684\u81ea\u8bc4\uff1b",
      "\u5404\u8981\u7d20\u4e0a\u7684 5 \u5206\u719f\u6089\u7a0b\u5ea6\u89c1\u6761\u76ee\u6c47\u603b\u3002"
    )
  )
}

.is_senior_title <- function(panel) {
  if ("title_level" %in% names(panel)) {
    return(as.character(panel$title_level) %in% c("\u6b63\u9ad8", "\u526f\u9ad8"))
  }
  if (!"title" %in% names(panel)) return(rep(NA, nrow(panel)))
  t <- as.character(panel$title)
  grepl("\u4e3b\u4efb\u533b\u5e08|\u6559\u6388|\u4e3b\u4efb\u836f\u5e08|\u4e3b\u4efb\u62a4\u5e08", t) &
    !grepl("\u4e3b\u6cbb", t)
}

.share_max <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_real_)
  max(table(x)) / length(x)
}

#' 对照技术要求检查专家组
#'
#' 检查人数是否不少于参考下限（默认 30）、是否具有高级职称、
#' 科室和地域是否有某一类超过一半。组外专家「可以包括」，不是必须。
#' 未通过时给出警告信息，不删除专家，也不停止计算。
#'
#' @param study [delphi_study()]；或与 `protocol` 一起提供的专家表。
#' @param panel 专家表。给出 `study` 时忽略。
#' @param protocol [delphi_protocol()]。给出 `study` 时忽略。
#'
#' @return 数据框，一行一条检查：`check`、`value`、`threshold`、`ok`、`level`、`message`。
#'
#' @examples
#' study <- simulate_standard_delphi(seed = 2026)
#' panel_compliance(study)
#'
#' @export
panel_compliance <- function(study = NULL, panel = NULL, protocol = NULL) {
  if (!is.null(study)) {
    study <- .match_study(study)
    panel <- study$panel
    protocol <- study$protocol
  }
  .stop_if(is.null(panel) || !nrow(panel), "\u6ca0\u6709\u4e13\u5bb6\u8868\u3002")
  panel <- .as_df(panel)
  min_n <- 30
  if (!is.null(protocol) && !is.null(protocol$rules$min_experts) &&
      !is.na(protocol$rules$min_experts)) {
    min_n <- protocol$rules$min_experts
  }
  n <- if ("invited" %in% names(panel)) sum(panel$invited %in% TRUE) else nrow(panel)
  if (n == 0L) n <- nrow(panel)
  senior <- .is_senior_title(panel)
  n_senior <- sum(senior %in% TRUE)
  n_known_title <- sum(!is.na(senior))
  dept_share <- if ("department" %in% names(panel)) .share_max(panel$department) else NA_real_
  region_share <- if ("region" %in% names(panel)) .share_max(panel$region) else NA_real_
  n_out <- if ("in_expert_group" %in% names(panel)) {
    sum(panel$in_expert_group %in% FALSE)
  } else NA_integer_

  row <- function(check, value, threshold, ok, level, message) {
    data.frame(
      check = check, value = value, threshold = threshold,
      ok = ok, level = level, message = message,
      stringsAsFactors = FALSE
    )
  }
  rows <- list(
    row(
      "n_experts", as.character(n), sprintf("\u2265 %s", min_n),
      n >= min_n, "warn",
      sprintf("\u8c03\u67e5\u5bf9\u8c61 %s \u4eba\uff1b\u6280\u672f\u8981\u6c42\u4e3a\u4e0d\u5b9c\u5c11\u4e8e %s \u4eba\u3002", n, min_n)
    ),
    row(
      "senior_title",
      if (n_known_title == 0L) "\u672a\u586b\u5199" else sprintf("%s/%s", n_senior, nrow(panel)),
      "\u5168\u90e8\u4e3a\u6b63\u9ad8\u6216\u526f\u9ad8",
      n_known_title == nrow(panel) && n_senior == nrow(panel),
      "warn",
      if (n_known_title < nrow(panel)) {
        "\u804c\u79f0\u7ea7\u522b\u672a\u586b\u5199\u5b8c\u6574\uff0c\u65e0\u6cd5\u786e\u8ba4\u662f\u5426\u5747\u4e3a\u9ad8\u7ea7\u804c\u79f0\u3002"
      } else if (n_senior < nrow(panel)) {
        sprintf("\u6709 %s \u4eba\u4e0d\u662f\u9ad8\u7ea7\u804c\u79f0\u3002\u6280\u672f\u8981\u6c42\u4e3a\u8c03\u67e5\u5bf9\u8c61\u5e94\u5177\u6709\u9ad8\u7ea7\u804c\u79f0\u3002", nrow(panel) - n_senior)
      } else {
        "\u5168\u90e8\u8c03\u67e5\u5bf9\u8c61\u4e3a\u9ad8\u7ea7\u804c\u79f0\u3002"
      }
    ),
    row(
      "department_balance",
      if (is.na(dept_share)) "\u672a\u586b\u5199" else sprintf("%.1f%%", 100 * dept_share),
      "\u5355\u4e00\u79d1\u5ba4\u5360\u6bd4 \u2264 50%",
      !is.na(dept_share) && dept_share <= 0.50,
      "warn",
      if (is.na(dept_share)) {
        "\u672a\u586b\u5199\u79d1\u5ba4\uff0c\u65e0\u6cd5\u5224\u65ad\u79d1\u5ba4\u5206\u5e03\u662f\u5426\u5747\u8861\u3002"
      } else if (dept_share > 0.50) {
        "\u6709\u79d1\u5ba4\u4eba\u6570\u8d85\u8fc7\u4e00\u534a\uff0c\u79d1\u5ba4\u5206\u5e03\u4e0d\u5747\u8861\u3002"
      } else {
        "\u6ca1\u6709\u79d1\u5ba4\u7684\u4eba\u6570\u8d85\u8fc7\u4e00\u534a\u3002"
      }
    ),
    row(
      "region_balance",
      if (is.na(region_share)) "\u672a\u586b\u5199" else sprintf("%.1f%%", 100 * region_share),
      "\u5355\u4e00\u5730\u57df\u5360\u6bd4 \u2264 50%",
      !is.na(region_share) && region_share <= 0.50,
      "warn",
      if (is.na(region_share)) {
        "\u672a\u586b\u5199\u5730\u57df\uff0c\u65e0\u6cd5\u5224\u65ad\u5730\u57df\u5206\u5e03\u662f\u5426\u5747\u8861\u3002"
      } else if (region_share > 0.50) {
        "\u6709\u5730\u57df\u4eba\u6570\u8d85\u8fc7\u4e00\u534a\uff0c\u5730\u57df\u5206\u5e03\u4e0d\u5747\u8861\u3002"
      } else {
        "\u6ca0\u6709\u5730\u57df\u7684\u4eba\u6570\u8d85\u8fc7\u4e00\u534a\u3002"
      }
    ),
    row(
      "outside_group",
      if (is.na(n_out)) "\u672a\u586b\u5199" else as.character(n_out),
      "\u53ef\u5305\u542b\u7ec4\u5916\u4e13\u5bb6",
      TRUE,
      "info",
      if (is.na(n_out)) {
        "\u672a\u6807\u8bb0\u662f\u5426\u4e13\u5bb6\u7ec4\u5185\u6210\u5458\u3002\u6280\u672f\u8981\u6c42\u5141\u8bb8\u7eb3\u5165\u7ec4\u5916\u4e13\u5bb6\uff0c\u4f46\u4e0d\u5f3a\u5236\u3002"
      } else {
        sprintf("\u4e13\u5bb6\u7ec4\u5916 %s \u4eba\u3002\u8fd9\u662f\u5141\u8bb8\u7684\uff0c\u4e0d\u662f\u5408\u683c\u7684\u5fc5\u8981\u6761\u4ef6\u3002", n_out)
      }
    )
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  .add_class(out, "delphi_panel_compliance")
}

.importance_means <- function(study, round, dimension = "\u91cd\u8981\u7a0b\u5ea6") {
  sm <- summarize_items(study, rounds = round, dimensions = dimension)
  sm <- sm[, c("item_id", "mean")]
  names(sm)[2] <- paste0("mean_", round)
  sm
}

#' 按协调程度判断能否结束调查
#'
#' 技术要求写的是：专家意见趋近一致、协调程度较好时可以结束，轮次宜为 2–4 轮。
#' 本函数把这句话写成首轮前锁定的三条规则，三条同时满足才建议结束：
#' 至少已有两轮相同的 5 分评分；重要程度的 Kendall W 不低于 `w_min` 且
#' 卡方检验 P 小于 `w_p_max`；各条目均数相对上一评分轮的平均绝对变化
#' 小于 `mean_delta_max`。
#'
#' 到达 `max_rounds` 但三条未同时满足时，`recommend_stop` 仍为 `FALSE`，
#' `reached_max` 为 `TRUE`。此时应如实报告未达预设协调程度，而不是改界值。
#'
#' @param study [delphi_study()] 或 [analyze_delphi()] 结果。
#' @param dimension 用于协调系数和均数变化的维度，默认「重要程度」。
#'
#' @return 列表。`by_round` 给出每一评分轮的 W、P 和均数变化；
#'   `recommend_stop` 表示协调程度是否已达到预设。
#'
#' @examples
#' study <- simulate_standard_delphi(seed = 2026)
#' coordination_decision(study)$recommend_stop
#'
#' @export
coordination_decision <- function(study, dimension = "\u91cd\u8981\u7a0b\u5ea6") {
  if (inherits(study, "delphi_result")) study <- study$study
  study <- .match_study(study)
  p <- study$protocol
  rules <- p$rules
  empty <- data.frame(
    round = integer(), w = numeric(), p_value = numeric(),
    n_expert = integer(), n_item = integer(),
    mean_abs_delta = numeric(), w_ok = logical(), p_ok = logical(),
    delta_ok = logical(), criteria_met = logical(),
    stringsAsFactors = FALSE
  )
  fail <- function(note) {
    list(
      by_round = empty,
      recommend_stop = FALSE,
      reached_max = FALSE,
      n_scoring_rounds = 0L,
      current_round = NA_integer_,
      dimension = dimension,
      thresholds = list(
        w_min = rules$w_min, w_p_max = rules$w_p_max,
        mean_delta_max = rules$mean_delta_max,
        min_scoring_rounds = 2L,
        max_rounds = p$stability$max_rounds
      ),
      note = note
    )
  }
  if (is.null(study$ratings) || !nrow(study$ratings)) {
    return(fail("\u5c1a\u65e0\u8bc4\u5206\u8f6e\u3002\u5f00\u653e\u5f0f\u5f81\u8be2\u4e0d\u8ba1\u5165\u534f\u8c03\u7cfb\u6570\u3002"))
  }
  r <- .ensure_dimension(.ensure_unable(study$ratings))
  r <- r[r$dimension == dimension & !r$unable & !is.na(r$score), , drop = FALSE]
  rounds <- .unique_sorted(r$round)
  if (!length(rounds)) return(fail("\u6ca0\u6709\u91cd\u8981\u7a0b\u5ea6\u8bc4\u5206\u3002"))

  means <- lapply(rounds, function(rd) .importance_means(study, rd, dimension))
  rows <- lapply(seq_along(rounds), function(i) {
    rd <- rounds[i]
    kw <- tryCatch(
      kendall_w(study, round = rd, dimension = dimension),
      error = function(e) list(w = NA_real_, p_value = NA_real_, n_expert = NA_integer_, n_item = NA_integer_)
    )
    delta <- NA_real_
    if (i >= 2L) {
      m <- merge(means[[i - 1L]], means[[i]], by = "item_id")
      if (nrow(m)) delta <- mean(abs(m[[3]] - m[[2]]), na.rm = TRUE)
    }
    w_ok <- !is.na(kw$w) && !is.na(rules$w_min) && kw$w >= rules$w_min
    p_ok <- !is.na(kw$p_value) && !is.na(rules$w_p_max) && kw$p_value < rules$w_p_max
    delta_ok <- i >= 2L && !is.na(delta) && !is.na(rules$mean_delta_max) &&
      delta < rules$mean_delta_max
    data.frame(
      round = rd,
      w = kw$w,
      p_value = kw$p_value,
      n_expert = kw$n_expert %||% NA_integer_,
      n_item = kw$n_item %||% NA_integer_,
      mean_abs_delta = delta,
      w_ok = w_ok,
      p_ok = p_ok,
      delta_ok = delta_ok,
      criteria_met = i >= 2L && w_ok && p_ok && delta_ok,
      stringsAsFactors = FALSE
    )
  })
  by_round <- do.call(rbind, rows)
  last <- by_round[nrow(by_round), ]
  current <- max(study$ratings$round)
  reached_max <- current >= p$stability$max_rounds
  recommend <- isTRUE(last$criteria_met)
  note <- if (recommend) {
    "\u91cd\u8981\u7a0b\u5ea6\u7684\u534f\u8c03\u7cfb\u6570\u3001\u5361\u65b9\u68c0\u9a8c\u548c\u5747\u6570\u53d8\u5316\u90fd\u5df2\u8fbe\u5230\u9996\u8f6e\u524d\u9501\u5b9a\u7684\u754c\u503c\uff0c\u534f\u8c03\u7a0b\u5ea6\u8f83\u597d\uff0c\u8c03\u67e5\u53ef\u4ee5\u7ed3\u675f\u3002"
  } else if (reached_max) {
    "\u5df2\u8fbe\u65b9\u6848\u89c4\u5b9a\u7684\u6700\u591a\u8f6e\u6b21\uff0c\u4f46\u534f\u8c03\u7a0b\u5ea6\u5c1a\u672a\u540c\u65f6\u6ee1\u8db3\u4e09\u6761\u754c\u503c\u3002\u5e94\u5982\u5b9e\u62a5\u544a\uff0c\u4e0d\u8981\u4e3a\u4e86\u7ed3\u675f\u800c\u6539\u754c\u503c\u3002"
  } else if (nrow(by_round) < 2L) {
    "\u76ee\u524d\u5c11\u4e8e\u4e24\u8f6e\u76f8\u540c\u7684 5 \u5206\u8bc4\u5206\uff0c\u8fd8\u4e0d\u80fd\u5224\u65ad\u534f\u8c03\u7a0b\u5ea6\u3002"
  } else {
    "\u534f\u8c03\u7a0b\u5ea6\u5c1a\u672a\u8fbe\u5230\u9884\u8bbe\u3002\u82e5\u672a\u6ee1\u6700\u591a\u8f6e\u6b21\uff0c\u5e94\u628a\u672c\u8f6e\u5747\u6570\u3001\u6ee1\u5206\u6bd4\u548c\u672c\u4eba\u4e0a\u8f6e\u8bc4\u5206\u53cd\u9988\u540e\u518d\u8bc4\u4e00\u8f6e\u3002"
  }
  list(
    by_round = by_round,
    recommend_stop = recommend,
    reached_max = reached_max,
    n_scoring_rounds = nrow(by_round),
    current_round = current,
    dimension = dimension,
    thresholds = list(
      w_min = rules$w_min,
      w_p_max = rules$w_p_max,
      mean_delta_max = rules$mean_delta_max,
      min_scoring_rounds = 2L,
      max_rounds = p$stability$max_rounds
    ),
    note = note
  )
}

#' 附录 B：核心技术要素的德尔菲结果
#'
#' 每个最终纳入的要素一行，对应标准附录中的呈现形式：标准内容、
#' 确定方法（德尔菲法）、调查人数、均值、满分比。调查人数是该要素
#' 在指定轮次给出有效重要程度评分的人数。
#'
#' @param result [analyze_delphi()] 结果。
#' @param round 采用哪一轮的均数和满分比，默认最后一轮。
#'
#' @return 数据框。只含纳入的要素；未纳入的要素不进入附录 B。
#'
#' @examples
#' study <- simulate_standard_delphi(seed = 2026)
#' res <- analyze_delphi(study)
#' appendix_b_table(res)
#'
#' @export
appendix_b_table <- function(result, round = NULL) {
  .stop_if(!inherits(result, "delphi_result"), "\u8bf7\u5148\u7528 analyze_delphi() \u5f97\u5230\u7ed3\u679c\u3002")
  cls <- result$classification
  if (is.null(round)) round <- result$last_round
  sub <- cls[cls$round == round, , drop = FALSE]
  if ("dimension" %in% names(sub)) {
    prefer <- intersect(c("\u91cd\u8981\u7a0b\u5ea6", "overall"), unique(sub$dimension))
    if (length(prefer)) sub <- sub[sub$dimension == prefer[1], , drop = FALSE]
  }
  sub <- sub[sub$action %in% "keep" | sub$class %in% "retain", , drop = FALSE]
  if (!nrow(sub)) {
    return(data.frame(
      item_id = character(),
      standard_text = character(),
      method = character(),
      n_respondents = integer(),
      mean = numeric(),
      full_rate = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  text <- if ("item_text" %in% names(sub)) sub$item_text else sub$item_id
  data.frame(
    item_id = sub$item_id,
    standard_text = text,
    method = "\u5fb7\u5c14\u83f2\u6cd5",
    n_respondents = sub$n_valid,
    mean = round(sub$mean, 2),
    full_rate = round(sub$full_rate, 1),
    stringsAsFactors = FALSE
  )
}

#' 整理开放式第 1 轮意见
#'
#' 开放式轮次不打分。本函数按研究组事先写好的 `theme` 计数，
#' 并把主题为「新增」的意见单独列出，供凝练成下一轮的核心技术要素。
#' 它不会自动把句子改写成标准条款。
#'
#' @param study [delphi_study()]，其中应有该轮的 [delphi_comments()]。
#' @param round 开放式轮次，默认 1。
#'
#' @return 列表：`by_theme`、`by_item`、`new_suggestions`、`n_experts`。
#'
#' @examples
#' study <- simulate_standard_delphi(seed = 2026)
#' open <- analyze_open_round(study, round = 1)
#' open$by_theme
#'
#' @export
analyze_open_round <- function(study, round = 1L) {
  study <- .match_study(study)
  round <- as.integer(round)[1]
  comments <- study$comments
  empty_theme <- data.frame(theme = character(), n = integer())
  if (is.null(comments) || !nrow(comments) || !any(comments$round == round)) {
    return(list(
      round = round,
      n_experts = 0L,
      n_comments = 0L,
      by_theme = empty_theme,
      by_item = data.frame(),
      examples = data.frame(),
      new_suggestions = data.frame(),
      note = "\u8be5\u8f6e\u6ca0\u6709\u610f\u89c1\u3002\u5f00\u653e\u5f0f\u8f6e\u6b21\u4e0d\u8ba1\u7b97\u5747\u6570\u6216\u6ee1\u5206\u6bd4\u3002"
    ))
  }
  comments <- comments[comments$round == round, , drop = FALSE]
  summed <- summarize_comments(comments)
  themes <- if ("theme" %in% names(comments)) as.character(comments$theme) else rep(NA_character_, nrow(comments))
  new_idx <- grepl("\u65b0\u589e|\u5efa\u8bae\u65b0\u589e|\u5f00\u653e\u5f0f", themes)
  list(
    round = round,
    n_experts = length(unique(comments$expert_id)),
    n_comments = nrow(comments),
    by_theme = summed$by_theme,
    by_item = summed$by_item,
    examples = summed$examples,
    new_suggestions = comments[new_idx, , drop = FALSE],
    note = paste0(
      "\u5f00\u653e\u5f0f\u8f6e\u6b21\u53ea\u6574\u7406\u610f\u89c1\uff0c\u4e0d\u8ba1\u7b97\u5747\u6570\u6216\u6ee1\u5206\u6bd4\u3002",
      "\u4e3b\u9898\u9700\u7531\u7814\u7a76\u7ec4\u4e8b\u5148\u7f16\u7801\u3002",
      "\u628a\u300c\u65b0\u589e\u300d\u610f\u89c1\u51dd\u7ec3\u6210\u6761\u6b3e\u540e\uff0c\u7528 update_delphi_study(items = ...) \u5e76\u5165\u4e0b\u4e00\u8f6e\u3002"
    )
  )
}

.std_item_bank <- function() {
  data.frame(
    item_id = sprintf("T%02d", 1:10),
    item_text = c(
      "\u8d34\u6577\u7a74\u4f4d\u5e94\u4e0e\u8fa8\u8bc1\u76f8\u7b26\u3002",
      "\u8d34\u6577\u836f\u7269\u5e94\u6807\u660e\u7ec4\u6210\u4e0e\u5355\u6b21\u7528\u91cf\u3002",
      "\u5355\u6b21\u8d34\u6577\u65f6\u95f4\u5e94\u89c4\u5b9a\u4e0a\u9650\u3002",
      "\u76ae\u80a4\u7834\u635f\u5904\u4e0d\u5b9c\u8d34\u6577\u3002",
      "\u8d34\u6577\u540e\u5e94\u89c2\u5bdf\u5c40\u90e8\u76ae\u80a4\u53cd\u5e94\u3002",
      "\u64cd\u4f5c\u8005\u5e94\u7ecf\u8fc7\u76f8\u5e94\u6280\u672f\u57f9\u8bad\u3002",
      "\u513f\u7ae5\u8d34\u6577\u5e94\u51cf\u5c0f\u836f\u7269\u7528\u91cf\u3002",
      "\u8d34\u6577\u671f\u95f4\u996e\u98df\u5b9c\u6e05\u6de1\u3002",
      "\u8d34\u6577\u524d\u5e94\u8be2\u95ee\u836f\u7269\u8fc7\u654f\u53f2\u3002",
      "\u53ef\u914d\u5408\u9999\u85b0\u4ee5\u589e\u5f3a\u8212\u9002\u611f\u3002"
    ),
    domain = c(
      "\u8fa8\u8bc1\u53d6\u7a74", "\u836f\u7269", "\u64cd\u4f5c\u65f6\u95f4", "\u7981\u5fcc",
      "\u89c2\u5bdf", "\u4eba\u5458", "\u7279\u6b8a\u4eba\u7fa4", "\u8c03\u62a4",
      "\u5b89\u5168", "\u8f85\u52a9\u63aa\u65bd"
    ),
    source_type = c(
      "\u53e4\u7c4d+\u73b0\u4ee3\u6587\u732e", "\u73b0\u4ee3\u6587\u732e", "\u73b0\u4ee3\u6587\u732e",
      "\u53e4\u7c4d+\u73b0\u4ee3\u6587\u732e", "\u73b0\u4ee3\u6587\u732e", "\u73b0\u4ee3\u6587\u732e",
      "\u53e4\u7c4d", "\u53e4\u7c4d", "\u5f00\u653e\u5f0f\u51dd\u7ec3", "\u5f00\u653e\u5f0f\u51dd\u7ec3"
    ),
    source_note = c(
      "\u300a\u7406\u6e1d\u9a88\u6587\u300b\u53d6\u7a74\u539f\u5219\u4e0e\u8fd1\u5e74\u8d34\u6577\u64cd\u4f5c\u7efc\u8ff0",
      "\u73b0\u4ee3\u4e2d\u836f\u5916\u6cbb\u64cd\u4f5c\u89c4\u8303\u5bf9\u836f\u7269\u7ec4\u6210\u7684\u8bb0\u8f7d\u8981\u6c42",
      "\u73b0\u4ee3\u6587\u732e\u4e2d\u5173\u4e8e\u8d34\u6577\u65f6\u95f4\u4e0e\u76ae\u80a4\u4e0d\u826f\u53cd\u5e94\u7684\u62a5\u544a",
      "\u53e4\u7c4d\u300c\u808c\u7834\u8005\u52ff\u7528\u300d\u4e0e\u73b0\u4ee3\u7981\u5fcc\u75c7\u4e00\u81f4",
      "\u73b0\u4ee3\u4e0d\u826f\u4e8b\u4ef6\u62a5\u544a\u8981\u6c42\u64cd\u4f5c\u540e\u89c2\u5bdf",
      "\u73b0\u4ee3\u6280\u672f\u89c4\u8303\u5bf9\u64cd\u4f5c\u4eba\u5458\u57f9\u8bad\u7684\u8981\u6c42",
      "\u513f\u79d1\u7528\u836f\u53e4\u7c4d\u4e2d\u300c\u91cf\u5c0f\u800c\u6b62\u300d\u7684\u8bb0\u8f7d",
      "\u517b\u751f\u53e4\u7c4d\u4e2d\u7684\u996e\u98df\u5b9c\u5fcc\uff0c\u7f3a\u4e4f\u4e0e\u8d34\u6577\u76f4\u63a5\u76f8\u5173\u7684\u73b0\u4ee3\u8bc1\u636e",
      "\u7b2c 1 \u8f6e\u5f00\u653e\u610f\u89c1\u4e2d\u591a\u4f4d\u4e13\u5bb6\u63d0\u51fa\u8fc7\u654f\u53f2\u8be2\u95ee",
      "\u7b2c 1 \u8f6e\u6709\u4e13\u5bb6\u5efa\u8bae\u589e\u52a0\u9999\u85b0\uff0c\u7f3a\u4e4f\u6587\u732e\u57fa\u7840"
    ),
    in_initial = c(rep(TRUE, 8), FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

.std_clamp_score <- function(x) {
  x <- round(x)
  x[x < 1] <- 1
  x[x > 5] <- 5
  x
}

#' 模拟一套符合技术要求的 5 分德尔菲研究
#'
#' 生成可复现的教学数据，不是真实临床推荐。36 名高级职称专家，
#' 科室和地域不过度集中，约两成来自专家组外。第 1 轮为开放式意见；
#' 第 2 至第 4 轮对重要程度和熟悉程度作 5 分评分。数据做成：
#' 前两轮评分的协调程度尚未达到预设，第 3 轮评分达到，因此走完 4 轮；
#' 最终至少有一条要素的重要程度均数低于 3.50。
#'
#' @param seed 随机种子，默认 2026。
#'
#' @return [delphi_study()]。`meta$initial_items` 是开放式轮次之前的 8 条要素。
#'
#' @examples
#' study <- simulate_standard_delphi(seed = 2026)
#' sort(unique(study$ratings$round))
#'
#' @export
simulate_standard_delphi <- function(seed = 2026) {
  set.seed(seed)
  protocol <- preset_protocol("standard_element")
  n_experts <- 36L
  depts <- c("\u80ba\u75c5\u79d1", "\u813e\u80c3\u75c5\u79d1", "\u9488\u7078\u79d1", "\u76ae\u80a4\u79d1", "\u513f\u79d1", "\u6cbb\u672a\u75c5\u79d1")
  regions <- c("\u534e\u5317", "\u534e\u4e1c", "\u534e\u5357", "\u897f\u5357")
  specs <- c("\u4e2d\u533b\u5185\u79d1", "\u4e2d\u533b\u5916\u79d1", "\u9488\u7078", "\u4e2d\u533b\u76ae\u80a4", "\u4e2d\u533b\u513f\u79d1", "\u4e2d\u533b\u517b\u751f")
  panel <- data.frame(
    expert_id = sprintf("E%02d", seq_len(n_experts)),
    sex = rep(c("\u7537", "\u5973"), length.out = n_experts),
    age = rep(c(38, 42, 45, 48, 52, 55, 58, 61, 44), length.out = n_experts),
    education = rep(c("\u535a\u58eb", "\u7855\u58eb", "\u7855\u58eb", "\u672c\u79d1"), length.out = n_experts),
    specialty = rep(specs, each = 6),
    department = rep(depts, each = 6),
    region = rep(regions, each = 9),
    title = rep(c("\u4e3b\u4efb\u4e2d\u533b\u5e08", "\u526f\u4e3b\u4efb\u4e2d\u533b\u5e08", "\u526f\u4e3b\u4efb\u4e2d\u533b\u5e08"), length.out = n_experts),
    title_level = rep(c("\u6b63\u9ad8", "\u526f\u9ad8", "\u526f\u9ad8"), length.out = n_experts),
    years = rep(c(12, 15, 18, 20, 22, 25, 28, 30, 16), length.out = n_experts),
    in_expert_group = rep(c(rep(TRUE, 4), FALSE), length.out = n_experts),
    stakeholder = "\u4e34\u5e8a\u4e13\u5bb6",
    invited = TRUE,
    stringsAsFactors = FALSE
  )
  auth <- data.frame(
    familiarity = rep(c("\u5f88\u719f\u6089", "\u8f83\u719f\u6089", "\u8f83\u719f\u6089", "\u4e00\u822c"), length.out = n_experts),
    practice = rep(c("\u5927", "\u5927", "\u4e2d"), length.out = n_experts),
    theory = rep(c("\u5927", "\u4e2d", "\u4e2d"), length.out = n_experts),
    literature = rep(c("\u4e2d", "\u5927", "\u4e2d"), length.out = n_experts),
    intuition = rep("\u5c0f", n_experts),
    stringsAsFactors = FALSE
  )
  panel <- cbind(panel, auth)
  scored <- score_authority(panel[, c("expert_id", "familiarity", "practice", "theory", "literature", "intuition")])
  panel$Ca <- scored$Ca
  panel$Cs <- scored$Cs

  bank <- .std_item_bank()
  items <- bank[, setdiff(names(bank), "in_initial")]
  initial_items <- items[bank$in_initial, , drop = FALSE]

  mu_imp <- rbind(
    c(3.40, 4.55, 3.15, 4.70, 3.45, 4.25, 2.95, 3.85, 3.05, 3.55),
    c(4.70, 4.55, 4.40, 4.28, 4.12, 3.98, 3.78, 3.42, 4.22, 2.58),
    c(4.82, 4.66, 4.50, 4.36, 4.20, 4.06, 3.88, 3.32, 4.32, 2.42)
  )
  mu_fam <- rbind(
    c(4.1, 4.3, 4.0, 4.4, 4.2, 4.0, 3.8, 3.6, 3.5, 3.0),
    c(4.3, 4.4, 4.2, 4.5, 4.3, 4.2, 4.0, 3.7, 4.0, 3.1),
    c(4.4, 4.5, 4.3, 4.6, 4.4, 4.3, 4.1, 3.8, 4.2, 3.2)
  )
  bias <- matrix(stats::rnorm(n_experts * 10, 0, 1.05), nrow = n_experts)
  shrink <- c(1, 0.28, 0.10)
  extra_sd <- c(0.25, 0.15, 0.42)
  respond <- list(
    panel$expert_id,
    setdiff(panel$expert_id, "E36"),
    setdiff(panel$expert_id, c("E35", "E36")),
    setdiff(panel$expert_id, c("E34", "E35", "E36"))
  )
  rating_rows <- list()
  for (k in 1:3) {
    rnd <- k + 1L
    ids <- respond[[k + 1L]]
    idx <- match(ids, panel$expert_id)
    for (j in seq_len(10)) {
      noise <- shrink[k] * bias[idx, j] + stats::rnorm(length(idx), 0, extra_sd[k])
      sc_imp <- .std_clamp_score(mu_imp[k, j] + noise)
      sc_fam <- .std_clamp_score(mu_fam[k, j] + stats::rnorm(length(idx), 0, 0.35))
      rating_rows[[length(rating_rows) + 1L]] <- data.frame(
        expert_id = ids,
        item_id = items$item_id[j],
        round = rnd,
        dimension = "\u91cd\u8981\u7a0b\u5ea6",
        score = sc_imp,
        unable = FALSE,
        stringsAsFactors = FALSE
      )
      rating_rows[[length(rating_rows) + 1L]] <- data.frame(
        expert_id = ids,
        item_id = items$item_id[j],
        round = rnd,
        dimension = "\u719f\u6089\u7a0b\u5ea6",
        score = sc_fam,
        unable = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  ratings <- do.call(rbind, rating_rows)
  unable_pos <- seq(1, nrow(ratings), length.out = 8)
  ratings$score[unable_pos] <- NA_real_
  ratings$unable[unable_pos] <- TRUE

  open_ids <- respond[[1]]
  comment_rows <- list(
    data.frame(
      expert_id = open_ids[1:18],
      item_id = "T01",
      round = 1L,
      theme = "\u4fdd\u7559",
      comment = "\u8fa8\u8bc1\u53d6\u7a74\u662f\u8d34\u6577\u7684\u524d\u63d0\uff0c\u5efa\u8bae\u4fdd\u7559\u3002",
      stringsAsFactors = FALSE
    ),
    data.frame(
      expert_id = open_ids[1:12],
      item_id = "T08",
      round = 1L,
      theme = "\u4fee\u6539\u8868\u8ff0",
      comment = "\u300c\u996e\u98df\u5b9c\u6e05\u6de1\u300d\u4e0e\u8d34\u6577\u64cd\u4f5c\u672c\u8eab\u5173\u7cfb\u4e0d\u76f4\u63a5\uff0c\u5efa\u8bae\u5199\u6e05\u662f\u5426\u4f5c\u4e3a\u5fc5\u987b\u9075\u5b88\u7684\u6280\u672f\u8981\u7d20\u3002",
      stringsAsFactors = FALSE
    ),
    data.frame(
      expert_id = open_ids[1:16],
      item_id = "T09",
      round = 1L,
      theme = "\u65b0\u589e",
      comment = "\u5efa\u8bae\u589e\u52a0\uff1a\u8d34\u6577\u524d\u5e94\u8be2\u95ee\u836f\u7269\u8fc7\u654f\u53f2\u3002",
      stringsAsFactors = FALSE
    ),
    data.frame(
      expert_id = open_ids[1:8],
      item_id = "T10",
      round = 1L,
      theme = "\u65b0\u589e",
      comment = "\u6709\u4e13\u5bb6\u5efa\u8bae\u628a\u9999\u85b0\u4f5c\u4e3a\u8f85\u52a9\u63aa\u65bd\u5199\u5165\u8981\u7d20\uff0c\u4f9b\u4e0b\u4e00\u8f6e\u8bc4\u5206\u5224\u65ad\u3002",
      stringsAsFactors = FALSE
    )
  )
  later <- data.frame(
    expert_id = c("E01", "E02", "E03"),
    item_id = c("T08", "T10", "T04"),
    round = c(2L, 3L, 4L),
    theme = c("\u4fee\u6539\u8868\u8ff0", "\u53cd\u5bf9", "\u4fdd\u7559"),
    comment = c(
      "\u672c\u8f6e\u4ecd\u8ba4\u4e3a\u996e\u98df\u5b9c\u5fcc\u4e0d\u5b9c\u4f5c\u4e3a\u5fc5\u987b\u8981\u7d20\u3002",
      "\u9999\u85b0\u7f3a\u4e4f\u4e0e\u8d34\u6577\u76f4\u63a5\u76f8\u5173\u7684\u4f9d\u636e\u3002",
      "\u76ae\u80a4\u7834\u635f\u5904\u4e0d\u5b9c\u8d34\u6577\uff0c\u540c\u610f\u4fdd\u7559\u3002"
    ),
    stringsAsFactors = FALSE
  )
  comments <- rbind(do.call(rbind, comment_rows), later)

  meta <- list(
    title = "\u4e2d\u533b\u7a74\u4f4d\u8d34\u6577\u6280\u672f\u64cd\u4f5c\u89c4\u8303\uff1a\u6838\u5fc3\u6280\u672f\u8981\u7d20\uff08\u6a21\u62df\u6570\u636e\uff09",
    objective = "\u5728\u53e4\u7c4d\u4e0e\u73b0\u4ee3\u6587\u732e\u57fa\u7840\u4e0a\uff0c\u7528\u5fb7\u5c14\u83f2\u6cd5\u786e\u5b9a\u7a74\u4f4d\u8d34\u6577\u7684\u6838\u5fc3\u6280\u672f\u8981\u7d20\u3002",
    users = "\u6807\u51c6\u8d77\u8349\u7ec4\u3001\u5ba1\u67e5\u4e13\u5bb6\u4e0e\u4e34\u5e8a\u64cd\u4f5c\u8005",
    geography = "\u4e2d\u56fd\u5927\u9646",
    evidence_note = "\u521d\u59cb 8 \u6761\u6765\u81ea\u53e4\u7c4d\u6216\u73b0\u4ee3\u6587\u732e\uff1b\u5176\u4e2d\u996e\u98df\u5b9c\u5fcc\u7f3a\u4e4f\u76f4\u63a5\u8bc1\u636e\u3002\u7b2c 1 \u8f6e\u5f00\u653e\u5f0f\u5f81\u8be2\u540e\u51dd\u7ec3\u51fa\u8fc7\u654f\u53f2\u4e0e\u9999\u85b0\u4e24\u6761\u3002",
    pilot_tested = TRUE,
    anonymity = "\u8bc4\u5206\u533f\u540d\u6c47\u603b\uff1b\u53cd\u9988\u4e0d\u51fa\u73b0\u4ed6\u4eba\u59d3\u540d\u3002",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\uff0c\u4e0d\u5f97\u4f5c\u4e3a\u771f\u5b9e\u6807\u51c6\u6761\u6b3e\u3002",
    initial_items = initial_items,
    numbering_note = paste0(
      "\u56e0\u90e8\u5206\u8981\u7d20\u7f3a\u4e4f\u524d\u671f\u57fa\u7840\uff0c\u7b2c 1 \u8f6e\u4e3a\u5f00\u653e\u5f0f\u8c03\u67e5\uff1b",
      "\u7b2c 2 \u8f6e\u8d77\u4e3a 5 \u5206\u91cf\u8868\u8bc4\u5206\uff0c\u5bf9\u5e94\u6280\u672f\u8981\u6c42\u4e2d\u7684\u8bc4\u5206\u8f6e\u6b21\u3002"
    )
  )
  delphi_study(protocol, panel, items, ratings, comments, meta = meta, validate = TRUE)
}
