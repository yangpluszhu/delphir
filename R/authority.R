#' 国内 Delphi 常用的熟悉程度 / 判断依据赋分表
#'
#' 默认采用曾光等专家咨询法中最常见的量化表。量表必须在第一轮前写入方案，
#' 不得根据结果更换。
#'
#' @param style 预设名称，目前仅 `"zeng"`。
#' @param cs 可选的命名数值向量，覆盖熟悉程度赋分，例如
#'   `c("很熟悉" = 0.9, "熟悉" = 0.7, "一般" = 0.5, "不熟悉" = 0.3, "很不熟悉" = 0.1)`。
#' @param ca 可选的判断依据表。数据框需含列 `source`、`large`、`medium`、`small`；
#'   或 4×3 数值矩阵，行依次为实践经验、理论分析、文献、直觉，列依次为大、中、小。
#'
#' @return 列表，含 `style`、`cs`（命名向量）、`ca`（数据框）、`note`。
#'
#' @section 默认表:
#' **Cs（熟悉程度）**：很熟悉 1.0，较熟悉 0.8，一般 0.6，不太熟悉 0.4，不熟悉 0.2。
#'
#' **Ca（判断依据）**：实践经验 0.5/0.4/0.3，理论分析 0.3/0.2/0.1，
#' 参考国内外文献 0.1/0.1/0.1，直观感觉 0.1/0.1/0.1。
#' 四项全选“大”时 Ca = 1.0，全“中” = 0.8，全“小” = 0.6。
#'
#' @examples
#' authority_scales()$cs
#' authority_scales()$ca
#'
#' @export
authority_scales <- function(style = "zeng", cs = NULL, ca = NULL) {
  style <- match.arg(style, c("zeng"))
  cs_default <- c(
    "\u5f88\u719f\u6089" = 1.0,
    "\u8f83\u719f\u6089" = 0.8,
    "\u4e00\u822c" = 0.6,
    "\u4e0d\u592a\u719f\u6089" = 0.4,
    "\u4e0d\u719f\u6089" = 0.2
  )
  ca_default <- data.frame(
    source = c(
      "\u5b9e\u8df5\u7ecf\u9a8c",
      "\u7406\u8bba\u5206\u6790",
      "\u53c2\u8003\u56fd\u5185\u5916\u6587\u732e",
      "\u76f4\u89c2\u611f\u89c9"
    ),
    key = c("practice", "theory", "literature", "intuition"),
    large = c(0.5, 0.3, 0.1, 0.1),
    medium = c(0.4, 0.2, 0.1, 0.1),
    small = c(0.3, 0.1, 0.1, 0.1),
    stringsAsFactors = FALSE
  )
  if (!is.null(cs)) {
    .stop_if(!is.numeric(cs) || is.null(names(cs)) || any(!nzchar(names(cs))),
             "cs \u5fc5\u987b\u662f\u547d\u540d\u6570\u503c\u5411\u91cf\u3002")
    .stop_if(any(cs < 0 | cs > 1, na.rm = TRUE), "Cs \u8d4b\u5206\u5e94\u5728 0 \u4e0e 1 \u4e4b\u95f4\u3002")
    cs_default <- cs
  }
  if (!is.null(ca)) {
    ca_default <- .coerce_ca_table(ca)
  }
  list(
    style = style,
    cs = cs_default,
    ca = ca_default,
    note = paste0(
      "Ca\u3001Cs \u662f\u4e13\u5bb6\u5bf9\u672c\u6b21\u54a8\u8be2\u4e3b\u9898\u7684\u4e00\u6b21\u81ea\u8bc4\uff0c",
      "\u4e0d\u662f\u6761\u76ee\u7ea7\u5171\u8bc6\u6307\u6807\u3002Cr = (Ca + Cs) / 2\u3002",
      "\u56fd\u5185\u5e38\u4ee5\u7ec4\u5747\u503c Cr \u2265 0.70 \u4e3a\u53c2\u8003\uff0c",
      "\u4f46 Cr \u4e0d\u662f DCAT \u6838\u5fc3\u8981\u6c42\uff0c\u4e5f\u4e0d\u5e94\u7528\u4e8e\u52a0\u6743\u8bc4\u5206\u3002"
    )
  )
}

#' 由熟悉程度自评计算 Cs
#'
#' @param familiarity 熟悉程度。可以是量表标签（如“较熟悉”）、1–5 分
#'   （5 = 很熟悉），或已经是 0–1 的系数。
#' @param scales [authority_scales()] 的返回值；缺省用曾光表。
#'
#' @return 与输入等长的数值 Cs。
#'
#' @examples
#' score_cs(c("很熟悉", "较熟悉", "一般", "不太熟悉", "不熟悉"))
#'
#' @export
score_cs <- function(familiarity, scales = NULL) {
  if (is.null(scales)) scales <- authority_scales()
  unname(vapply(familiarity, function(x) .map_cs_one(x, scales$cs), numeric(1)))
}

#' 由判断依据自评计算 Ca
#'
#' 四类依据各选“大 / 中 / 小”，按量化表加总。
#'
#' @param practice,theory,literature,intuition 实践经验、理论分析、
#'   参考国内外文献、直观感觉的影响程度（“大”“中”“小”或其别名）。
#' @param scales [authority_scales()] 的返回值。
#'
#' @return 与输入等长的数值 Ca。
#'
#' @examples
#' score_ca("大", "大", "大", "大")
#' score_ca("中", "中", "中", "中")
#' score_ca("小", "小", "小", "小")
#'
#' @export
score_ca <- function(practice, theory, literature, intuition, scales = NULL) {
  if (is.null(scales)) scales <- authority_scales()
  n <- max(length(practice), length(theory), length(literature), length(intuition))
  practice <- rep_len(practice, n)
  theory <- rep_len(theory, n)
  literature <- rep_len(literature, n)
  intuition <- rep_len(intuition, n)
  tab <- scales$ca
  vapply(seq_len(n), function(i) {
    .lookup_ca(tab, "practice", practice[i]) +
      .lookup_ca(tab, "theory", theory[i]) +
      .lookup_ca(tab, "literature", literature[i]) +
      .lookup_ca(tab, "intuition", intuition[i])
  }, numeric(1))
}

#' 从专家级自评表计算 Ca、Cs、Cr
#'
#' 每人一行。这是权威系数的正确粒度：描述专家相对**本次咨询主题**的权威程度，
#' 不是条目共识。条目级知识缺口应使用 `unable`（无法判断 / 超出专业领域）。
#'
#' @param data 数据框。必填列：`expert_id`、`familiarity`、`practice`、
#'   `theory`、`literature`、`intuition`。
#' @param scales [authority_scales()] 的返回值。
#' @param threshold 传给 [expert_authority()] 的 Cr 参考界值，默认 0.70。
#'
#' @return 专家级数据框，含 `Cs`、`Ca`、`Cr`、`cr_ok`；属性 `overall` 与
#'   `scales`。
#'
#' @section 方法学约束:
#' * 若存在 `item_id` 列，或 `expert_id` 重复，函数报错：这通常意味着误按条目填了权威系数。
#' * 不要用 Cr 对 Likert 评分加权；Delphi 以匿名等权为核心。
#' * 第二轮一般不必重评 Ca/Cs。
#'
#' @examples
#' raw <- data.frame(
#'   expert_id = c("E1", "E2"),
#'   familiarity = c("较熟悉", "很熟悉"),
#'   practice = c("大", "中"),
#'   theory = c("中", "大"),
#'   literature = c("大", "中"),
#'   intuition = c("小", "小"),
#'   stringsAsFactors = FALSE
#' )
#' score_authority(raw)
#'
#' @export
score_authority <- function(data, scales = NULL, threshold = 0.70) {
  data <- .as_df(data)
  if (is.null(scales)) scales <- authority_scales()
  .stop_if(
    "item_id" %in% names(data),
    paste0(
      "\u6743\u5a01\u7cfb\u6570\u6309\u4e13\u5bb6\u81ea\u8bc4\u4e00\u6b21\uff0c\u4e0d\u80fd\u6309\u6761\u76ee\u8ba1\u7b97\u3002",
      "\u6761\u76ee\u7ea7\u77e5\u8bc6\u7f3a\u53e3\u8bf7\u4f7f\u7528 unable\uff08\u65e0\u6cd5\u5224\u65ad/\u8d85\u51fa\u4e13\u4e1a\u9886\u57df\uff09\uff0c",
      "\u4e0d\u8981\u9010\u6761\u586b\u5199 Ca/Cs\u3002"
    )
  )
  .need_cols(
    data,
    c("expert_id", "familiarity", "practice", "theory", "literature", "intuition"),
    "\u6743\u5a01\u7cfb\u6570\u81ea\u8bc4\u8868"
  )
  data$expert_id <- as.character(data$expert_id)
  .stop_if(
    anyDuplicated(data$expert_id) > 0,
    paste0(
      "expert_id \u5fc5\u987b\u552f\u4e00\uff1aCa/Cs/Cr \u662f\u4e13\u5bb6\u7ea7\u6307\u6807\u3002",
      "\u82e5\u540c\u4e00\u4e13\u5bb6\u591a\u884c\uff0c\u8bf4\u660e\u8bef\u628a\u6761\u76ee\u8868\u5f53\u6210\u4e86\u6743\u5a01\u81ea\u8bc4\u8868\u3002"
    )
  )
  out <- data
  out$Cs <- score_cs(out$familiarity, scales = scales)
  out$Ca <- score_ca(
    out$practice, out$theory, out$literature, out$intuition,
    scales = scales
  )
  scored <- expert_authority(out, threshold = threshold)
  keep <- setdiff(names(out), c("Ca", "Cs", "Cr", "cr_ok"))
  out <- cbind(out[, keep, drop = FALSE], scored[, c("Ca", "Cs", "Cr", "cr_ok")])
  attr(out, "overall") <- attr(scored, "overall")
  attr(out, "scales") <- scales
  .add_class(out, "delphi_authority")
}

#' 计算国内文献常用的专家权威系数 Cr
#'
#' \deqn{Cr = (Ca + Cs) / 2}
#'
#' Ca、Cs 描述专家相对**本次咨询主题**的一次自评，不是条目共识。
#' 默认不按条目计算，不用于加权 Likert 分数，第二轮不必重评。
#' 国内研究常把组均数 Cr ≥ 0.70 视为权威性较好的参考；这不是国际 Delphi（DCAT）
#' 的核心要求。条目级“不熟 / 超出专业”应使用 `unable`，而不是再算一套 Cr。
#'
#' 若只有问卷勾选结果，请先用 [score_authority()] 由熟悉程度与判断依据赋分。
#'
#' @param panel [delphi_panel()] 或含 `Ca`、`Cs` 的数据框。一人一行，不得含 `item_id`。
#' @param threshold 参考界值，默认 0.70。
#'
#' @return 专家级数据框，含 `Cr` 与 `cr_ok`；属性 `overall` 为面板均数。
#'
#' @seealso [authority_scales()], [score_cs()], [score_ca()], [score_authority()]
#'
#' @examples
#' study <- simulate_delphi(n_experts = 10, n_items = 4, seed = 6)
#' auth <- expert_authority(study$panel)
#' attr(auth, "overall")
#'
#' @export
expert_authority <- function(panel, threshold = 0.70) {
  panel <- .as_df(panel)
  .stop_if(
    "item_id" %in% names(panel) && !"Ca" %in% names(panel),
    paste0(
      "\u8bf7\u4f20\u5165\u4e13\u5bb6\u8868\u800c\u4e0d\u662f\u6761\u76ee\u8bc4\u5206\u8868\u3002",
      "Ca/Cs \u4e0d\u6309\u6761\u76ee\u8ba1\u7b97\uff1b\u6761\u76ee\u7ea7\u8bf7\u4f7f\u7528 unable\u3002"
    )
  )
  .stop_if(
    "item_id" %in% names(panel),
    paste0(
      "\u4e13\u5bb6\u8868\u4e0d\u5e94\u542b item_id\u3002",
      "\u6743\u5a01\u7cfb\u6570\u6309\u4e13\u5bb6\u8ba1\u7b97\u4e00\u6b21\uff0c\u4e0d\u6309\u6761\u76ee\u8ba1\u7b97\u3002"
    )
  )
  .need_cols(panel, c("expert_id", "Ca", "Cs"), "\u6743\u5a01\u7cfb\u6570\u8ba1\u7b97")
  .stop_if(anyDuplicated(as.character(panel$expert_id)) > 0,
           "expert_id \u5fc5\u987b\u552f\u4e00\uff1aCr \u4e0d\u6309\u6761\u76ee\u91cd\u590d\u8ba1\u7b97\u3002")
  .stop_if(any(panel$Ca < 0 | panel$Ca > 1 | panel$Cs < 0 | panel$Cs > 1, na.rm = TRUE),
           "Ca\u3001Cs \u5e94\u5728 0 \u4e0e 1 \u4e4b\u95f4\u3002")
  out <- panel[, intersect(c("expert_id", "stakeholder", "Ca", "Cs"), names(panel)), drop = FALSE]
  out$Cr <- (as.numeric(out$Ca) + as.numeric(out$Cs)) / 2
  out$cr_ok <- !is.na(out$Cr) & out$Cr >= threshold
  attr(out, "overall") <- list(
    mean_ca = mean(out$Ca, na.rm = TRUE),
    mean_cs = mean(out$Cs, na.rm = TRUE),
    mean_cr = mean(out$Cr, na.rm = TRUE),
    pct_ok = .pct(sum(out$cr_ok), sum(!is.na(out$Cr))),
    threshold = threshold,
    note = paste0(
      "Cr \u4e0d\u662f\u56fd\u9645 Delphi\uff08DCAT\uff09\u6838\u5fc3\u6307\u6807\uff0c\u4ec5\u4f9b\u56fd\u5185\u53d1\u8868\u8865\u5145\u62a5\u544a\u3002",
      "\u5b83\u63cf\u8ff0\u4e13\u5bb6\u76f8\u5bf9\u672c\u6b21\u4e3b\u9898\u7684\u81ea\u8bc4\u6743\u5a01\u7a0b\u5ea6\uff0c",
      "\u4e0d\u5e94\u7528\u4e8e\u52a0\u6743 Likert \u8bc4\u5206\uff0c\u4e5f\u4e0d\u5fc5\u5728\u7b2c\u4e8c\u8f6e\u91cd\u8bc4\u3002"
    )
  )
  .add_class(out, "delphi_authority")
}

# internals ----------------------------------------------------------------

.coerce_ca_table <- function(ca) {
  if (is.matrix(ca)) {
    .stop_if(nrow(ca) != 4L || ncol(ca) != 3L, "\u81ea\u5b9a\u4e49 Ca \u77e9\u9635\u5e94\u4e3a 4 \u884c 3 \u5217\u3002")
    ca <- data.frame(
      source = c(
        "\u5b9e\u8df5\u7ecf\u9a8c", "\u7406\u8bba\u5206\u6790",
        "\u53c2\u8003\u56fd\u5185\u5916\u6587\u732e", "\u76f4\u89c2\u611f\u89c9"
      ),
      key = c("practice", "theory", "literature", "intuition"),
      large = as.numeric(ca[, 1]),
      medium = as.numeric(ca[, 2]),
      small = as.numeric(ca[, 3]),
      stringsAsFactors = FALSE
    )
  }
  ca <- .as_df(ca)
  .need_cols(ca, c("large", "medium", "small"), "Ca \u91cf\u5316\u8868")
  if (!"key" %in% names(ca)) {
    ca$key <- c("practice", "theory", "literature", "intuition")[seq_len(nrow(ca))]
  }
  if (!"source" %in% names(ca)) ca$source <- ca$key
  nums <- c(ca$large, ca$medium, ca$small)
  .stop_if(any(nums < 0 | nums > 1, na.rm = TRUE), "Ca \u8d4b\u5206\u5e94\u5728 0 \u4e0e 1 \u4e4b\u95f4\u3002")
  ca
}

.norm_label <- function(x) {
  if (length(x) != 1L) x <- x[1]
  if (is.null(x) || is.na(x)) return(NA_character_)
  x <- trimws(as.character(x))
  x <- gsub("\\s+", "", x)
  x
}

.map_cs_one <- function(x, cs_map) {
  if (length(x) == 0L || is.null(x) || (length(x) == 1L && is.na(x))) {
    return(NA_real_)
  }
  if (is.numeric(x) && length(x) == 1L) {
    if (x >= 0 && x <= 1) return(as.numeric(x))
    if (x %in% 1:5) {
      return(unname(c("1" = 0.2, "2" = 0.4, "3" = 0.6, "4" = 0.8, "5" = 1.0)[as.character(as.integer(x))]))
    }
    stop("\u65e0\u6cd5\u8bc6\u522b\u7684\u719f\u6089\u7a0b\u5ea6\u6570\u503c: ", x, call. = FALSE)
  }
  lab <- .norm_label(x)
  aliases <- c(
    "\u5f88\u719f\u6089" = "\u5f88\u719f\u6089",
    "\u975e\u5e38\u719f\u6089" = "\u5f88\u719f\u6089",
    "very" = "\u5f88\u719f\u6089",
    "veryfamiliar" = "\u5f88\u719f\u6089",
    "\u8f83\u719f\u6089" = "\u8f83\u719f\u6089",
    "\u719f\u6089" = "\u8f83\u719f\u6089",
    "familiar" = "\u8f83\u719f\u6089",
    "\u4e00\u822c" = "\u4e00\u822c",
    "\u4e00\u822c\u719f\u6089" = "\u4e00\u822c",
    "\u4e00\u822c\u4e86\u89e3" = "\u4e00\u822c",
    "moderate" = "\u4e00\u822c",
    "\u4e0d\u592a\u719f\u6089" = "\u4e0d\u592a\u719f\u6089",
    "\u4e0d\u5f88\u719f\u6089" = "\u4e0d\u592a\u719f\u6089",
    "slightly" = "\u4e0d\u592a\u719f\u6089",
    "\u4e0d\u719f\u6089" = "\u4e0d\u719f\u6089",
    "\u5f88\u4e0d\u719f\u6089" = "\u4e0d\u719f\u6089",
    "\u5b8c\u5168\u4e0d\u719f\u6089" = "\u4e0d\u719f\u6089",
    "unfamiliar" = "\u4e0d\u719f\u6089"
  )
  key <- aliases[[tolower(lab)]]
  if (is.null(key)) key <- aliases[[lab]]
  if (is.null(key)) key <- lab
  if (key %in% names(cs_map)) return(unname(cs_map[[key]]))
  # custom maps may use the alias labels directly
  nm <- names(cs_map)
  hit <- which(tolower(nm) == tolower(lab) | nm == key)
  if (length(hit) == 1L) return(unname(cs_map[[hit]]))
  stop("\u65e0\u6cd5\u8bc6\u522b\u7684\u719f\u6089\u7a0b\u5ea6: ", lab, call. = FALSE)
}

.map_influence <- function(x) {
  if (length(x) == 0L || is.null(x) || (length(x) == 1L && is.na(x))) {
    return(NA_character_)
  }
  if (is.numeric(x) && length(x) == 1L) {
    if (x == 3) return("large")
    if (x == 2) return("medium")
    if (x == 1) return("small")
  }
  lab <- .norm_label(x)
  lab <- tolower(lab)
  map <- c(
    "\u5927" = "large", "large" = "large", "high" = "large", "h" = "large",
    "\u4e2d" = "medium", "medium" = "medium", "mid" = "medium", "m" = "medium",
    "\u5c0f" = "small", "small" = "small", "low" = "small", "l" = "small"
  )
  out <- map[[lab]]
  if (is.null(out)) stop("\u65e0\u6cd5\u8bc6\u522b\u7684\u5224\u65ad\u4f9d\u636e\u5f71\u54cd\u7a0b\u5ea6: ", x, call. = FALSE)
  out
}

.lookup_ca <- function(tab, key, influence) {
  inf <- .map_influence(influence)
  if (is.na(inf)) return(NA_real_)
  row <- tab[tab$key == key, , drop = FALSE]
  .stop_if(nrow(row) != 1L, sprintf("Ca \u8868\u7f3a\u5c11 %s \u884c\u3002", key))
  col <- c(large = "large", medium = "medium", small = "small")[[inf]]
  as.numeric(row[[col]][1])
}
