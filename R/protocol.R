#' 创建并锁定 Delphi 统计分析方案
#'
#' 在第一轮调查开始前规定评分尺度、共识阈值、异议标准、稳定性标准和终止规则。
#' 分析函数只执行本对象中的规则，不会根据结果回改阈值。完整参数解读见
#' **Details** 与 [protocol_table()]；教学演示见
#' `vignette("preset-protocol", package = "delphir")`。
#'
#' @param scenario 场景标识。决定**哪一套规则真正被分类函数使用**，其余阈值
#'   仍会写入对象（便于报告完整打印），但可能不参与判定。
#'
#'   - clinical_consensus：医学专家共识（Modified Delphi）。使用复合标准：
#'     高区比例 + Median + IQR。这是最常用的预设。
#'   - comet_cos：COMET 核心结局集。使用 70/15 纳入/排除规则，而不是
#'     high_pct 复合标准。
#'   - rand_ucla：适宜性评价。默认按手册表 4（应答人数查表）加中位数分类，
#'     **不强迫共识**。也可改用 IPRAS、9 人严格/宽松定义或百分比规则。
#'     high_pct / basic_pct 对本场景为 NA。经典适宜性默认最多 2 轮。
#'   - delphi_ram：改良德尔菲结合 RAM。匿名多轮函询，默认最多 3 轮，
#'     每轮复评全部条目。主规则只能是 `table4`、`ipras` 或 `percent`。
#'     临床 `high_pct` 会写入对象供对照，但不参与分类。请用
#'     [delphi_ram_protocol()] 与 [run_delphi_ram()]，不要用 [run_delphi()]
#'     或 [run_rand()]。
#'   - indicator：指标体系筛选。在基本共识之外还要求 CV 不超过 cv_max。
#'   - ranking：优先级排序。不判定必须共识，配合 [rank_priorities()] /
#'     [kendall_w()] 使用。
#'   - definition：定义或诊断标准。默认要求高区比例至少 0.80 且 IQR 不超过 2。
#'   - policy：Policy Delphi。描述分布、呈现分歧，不把「达成共识」当作成功标准。
#'   - standard_element：标准起草中的核心技术要素。5 分量表；重要性看均数；
#'     附录报告调查人数、均值、满分比；轮次宜 2–4，按协调系数决定是否结束。
#' @param scale_min,scale_max 评分尺度端点。除 `standard_element` 外默认 1 与 9。
#'   `standard_element` 默认 1 与 5。须满足 `scale_min < scale_max`。
#'   Likert 通常为整数；分析时非整数会警告。
#'   改成其他量表时，必须同步改 `low_range`/`mid_range`/`high_range`，
#'   否则区间端点会对不上。
#' @param low_range,mid_range,high_range 长度为 2 的数值向量，闭区间。
#'   默认 1-9 量表下低 1-3、中 4-6、高 7-9。p_high / p_low 的分母是有效
#'   Likert 评分（「无法判断」不进入）。区间应覆盖整个量表且不宜重叠。
#' @param high_pct 高度共识所需的**高区比例下限**（0–1），临床预设 0.80。
#'   仅当同时满足 Median ≥ `median_min` 且 IQR ≤ `iqr_max` 才判为高度共识。
#'   Diamond 等系统评价显示文献阈值差异很大；正确原则是**第一轮前写明**，
#'   而不是事后改到「刚好能过」。
#' @param basic_pct 基本共识所需的高区比例下限（0–1），临床预设 0.75。
#'   应不大于 high_pct。落在 basic_pct 与 high_pct 之间且 Median 达标时为基本共识
#'   （可保留，但报告中应与高度共识分列）。
#' @param median_min 共识所需的中位数下限，默认 7（即高区下沿）。
#'   单独把百分比调高但把中位数降到 5，会把「意见集中在中区」误判为共识。
#' @param iqr_max 高度共识允许的最大四分位距，默认 2。IQR 大表示两端仍拉开；
#'   即使 0.80 的评分落在 7–9，若少数人打 1 分把 IQR 拉到 3，按预设不能叫高度共识。
#' @param reject_pct 倾向排除所需的低区比例下限，默认 0.75。
#'   与高度共识对称：多数人明确反对时标记为排除，而不是「没过线就删」。
#' @param comet_in_high COMET 纳入：高区比例下限，默认 0.70。仅 `comet_cos` 使用。
#' @param comet_in_low_max COMET 纳入：低区比例上限，默认 0.15。
#'   「高区至少 0.70 **且** 低区不到 0.15」才能纳入核心结局集。
#' @param comet_out_low COMET 排除：低区比例下限，默认 0.70。
#' @param comet_out_high_max COMET 排除：高区比例上限，默认 0.15。
#'   两套 70/15 都不满足的条目进入下一轮或共识会议，而不是自动删除。
#' @param rand_disagreement RAND 分歧定义，仅 `rand_ucla` 进入 [classify_rand()]。
#'   `"table4"`（默认）按 Fitch 等（2001）表 4 用实际应答人数查表。
#'   `"ipras"` 为 IPRAS（IPR 严格大于 IPRAS 才算分歧）。
#'   `"classic_relaxed"` / `"classic_strict"` 为 9 人小组的手册表 3。
#'   `"percent"` 为百分比共识，属于 RAM 修改版，用于推荐意见。
#'   `"both"` 为表 4 与 IPRAS 任一判分歧即分歧。
#'   `"classic"` 是旧参数名，现与 `"table4"` 相同，不再使用 `ceiling(n/3)`。
#' @param rand_di_threshold IPRAS 的倍率，默认 1，即 IPR > IPRAS 才算分歧。
#'   相等不算分歧。
#' @param rand_round2_mode 第 2 轮形式：`"in_person"` 面对面会议后复评，
#'   `"online"` 线上会议后复评，`"mail"` 再次函询。后两种须在报告中说明。
#' @param rand_necessity 是否计划在适宜且无分歧的条目上另做必要性评分。
#'   必要性不是第三轮适宜性评分。
#' @param rand_boundary 中位数为 3.5 或 6.5 时：`"uncertain"`（默认）归不确定，
#'   `"round_half_down"` 或 `"round_half_up"` 向相邻整数取整后再套三分区。
#' @param rand_iprr,rand_cfa IPRAS 常数，默认 2.35 与 1.5。改动后报告标为非标准参数。
#' @param rand_qtype 分位数算法，默认 7（与手册示例的 R 复算一致）。
#' @param rand_percent_high,rand_percent_low 百分比规则的高区下限与低区上限。
#'   默认 0.70 与 0.15。只在 `rand_disagreement = "percent"` 时驱动分类。
#' @param rand_purpose `"appropriateness"`（默认）或 `"recommendation"`。
#'   推荐意见应使用百分比规则，并在报告中标明 RAM 修改版。
#' @param max_rounds 计划最多轮次，默认 3。这是**终止规则的上限**，不是「必须跑满」。
#'   问卷生成超过该值会报错。DCAT 将迭代视为核心特征，至少应有两轮相同量表。
#' @param stability_class_pct 相邻轮分类不变条目的比例阈值，默认 0.80。
#'   刚跨过共识线就停止可能过早；稳定性看的是分类是否还在变。
#' @param stability_median_delta 中位数变化不超过该值视为稳定，默认 1 分。
#' @param stability_high_pct_delta 高区比例变化不超过该值视为稳定，默认 0.10
#'   （即 10 个百分点）。与分类不变、Median 稳定同时满足才建议终止。
#' @param polarization_threshold 两端比例同时达到该值视为两极分化，默认 0.25。
#'   均数接近 5 往往不是「大家觉得一般」，而是高区与低区对打。
#' @param cv_max 指标体系筛选允许的最大变异系数，默认 0.25。仅 `indicator`
#'   使用；CV 是均数的补充，不能代替 Median/IQR。
#' @param ranking_topk 优先级分析中的 Top-k，默认 5。仅排序场景用于
#'   [rank_priorities()]，不改变 Likert 共识判定。
#' @param mean_min 核心技术要素纳入所需的重要程度均数下限。仅
#'   `standard_element` 使用，默认 3.50（5 分量表）。其他场景为 `NA`。
#' @param w_min 结束调查所需的 Kendall 协调系数下限。仅 `standard_element`
#'   使用，默认 0.50。须与 `w_p_max`、`mean_delta_max` 同时满足。
#' @param w_p_max 协调系数卡方检验的 P 值上限，默认 0.05。P 值显著只说明
#'   排序不像纯随机，不能单独当作「协调程度较好」。
#' @param mean_delta_max 相邻两轮评分中，各条目重要程度均数绝对变化的平均值
#'   上限。仅 `standard_element` 使用，默认 0.25。
#' @param min_experts 调查对象人数参考下限。`standard_element` 默认 30，
#'   少于该人数时 [panel_compliance()] 给出警告，不中断计算。
#' @param response_rate_min 积极系数（回收率）的参考下限，默认 0.70。
#'   只用于报告解读，不加权、不剔除专家。
#' @param cr_min 权威系数组均数的参考下限，默认 0.70。不用于加权评分。
#' @param authority_scale 可选，[authority_scales()] 的返回值。缺省为曾光表。
#'   第 1 轮问卷、分析和报告都使用这份表。自定义后应填写 `rationale`。
#'   已有数值 Ca、Cs 时，分析不再按该表重算。
#' @param allow_new 是否允许第 1 轮结构化补充候选条目。`NULL` 时按场景取值：
#'   医学共识、COMET、定义/诊断标准、指标体系为 `TRUE`；适宜性、排序、
#'   Policy 与 5 分核心技术要素为 `FALSE`。
#' @param min_independent 同一新概念至少由几名专家**独立**提出才走主通道。
#'   默认 2。同一人重复书写只计 1 人。这不是共识阈值。
#' @param allow_single_with_evidence 是否保留例外通道：仅 1 名专家提出，
#'   但有文献、定性访谈、患者证据、指南或重要临床依据时，仍可进入审查。
#'   默认 `TRUE`。
#' @param max_suggestions 每位专家最多可推荐的新条目数，默认 5。
#' @param require_iteration 新条目是否必须至少经过两轮评分才能最终纳入。
#'   默认在允许新增的场景中为 `TRUE`。只评过一次时，即使已达共识线，
#'   也只记为暂时共识。
#' @param review_panel 条目审查组构成。默认示例为 5 人：中医临床 2、
#'   方法学 1、统计或 Delphi 方法 1、患者或患者代表 1。软件不代替人工编码。
#' @param open_first 是否把第 1 轮作为开放式征询（不打分）。
#'   `standard_element` 默认 `TRUE`：缺少前期基础时先凝练要素。
#'   若要素均已有古籍或现代文献基础，设为 `FALSE`，此时第 1 轮就是
#'   技术要求所称的评分调查（文档中的「第 2 轮」）。
#' @param rationale 若修改了预设阈值，必须说明理由；报告会打印「方案偏离预设」。
#'   临床/定义场景改了 `high_pct` 等、或核心技术要素场景改了均数/协调界值，
#'   而未填此项时会警告。
#' @param locked 是否锁定阈值。默认 `TRUE`。锁定后分析阶段不得把阈值当参数回改。
#'
#' @return 带类 `"delphi_protocol"` 的列表。主要字段：
#' * `scenario`, `scale_min`, `scale_max`, `low_range`, `mid_range`, `high_range`
#' * `rules`：共识判定阈值（含 COMET、极化、CV、Top-k，以及核心技术要素的
#'   均数界值、协调系数界值、积极系数与权威系数参考值）
#' * `stability`：`max_rounds` 与三条稳定性规则
#' * `open_first`：第 1 轮是否为开放式征询
#' * `new_items`：第一轮新增条目规则（是否允许、独立人数、单人例外、
#'   每人上限、是否必须再迭代一轮、审查组说明）
#' * `authority_scale`：Ca/Cs 赋分表，缺省为曾光表
#' * `rand`：规则、第 2 轮形式、必要性、边界中位数、IPRAS 常数与百分比阈值
#' * `modified_from_preset`, `rationale`, `locked`
#'
#' @section 输入输出:
#' **输入**均为标量或长度为 2 的数值向量，比例类参数必须在 0 与 1 之间。
#'
#' **输出**供 [delphi_study()] / [delphi_blueprint()] 在第一轮前锁定。
#' [classify_consensus()] **没有**阈值参数，以防止分析阶段静默改规则。
#'
#' @section 何时改预设、何时不要改:
#' * **可以改**：高风险诊疗推荐把 `high_pct` 从 0.80 提到 0.85，并写 `rationale`。
#' * **不要改**：看见第 1 轮刚好比 0.80 低 2 个百分点，就把阈值改成 0.75「凑共识」。
#' * **不要混用**：把 COMET 的 70/15 套到临床共识，或把临床 0.80 套到 RAND 适宜性。
#'
#' @examples
#' p <- preset_protocol("clinical_consensus")
#' p
#' protocol_table(p)[1:8, ]
#'
#' p2 <- delphi_protocol(
#'   scenario = "clinical_consensus",
#'   high_pct = 0.85,
#'   rationale = "高风险诊疗推荐，将高度共识阈值提高到 0.85。"
#' )
#' p2$modified_from_preset
#' p2$rationale
#'
#' # 同一批评分、不同阈值会改变分类（教学数据第 1 轮）
#' data(consensus_demo, package = "delphir")
#' s <- summarize_items(consensus_demo, rounds = 1)
#' tab0 <- table(classify_consensus(s, p)$class_zh)
#' p_loose <- preset_protocol("clinical_consensus", high_pct = 0.70,
#'                            rationale = "仅用于演示：放宽高度共识阈值。")
#' tab1 <- table(classify_consensus(s, p_loose)$class_zh)
#' tab0
#' tab1
#'
#' @seealso [preset_protocol()], [protocol_table()], [classify_consensus()],
#'   [analyze_delphi()], vignette("preset-protocol", package = "delphir")
#'
#' @export
delphi_protocol <- function(
    scenario = c(
      "clinical_consensus", "comet_cos", "rand_ucla", "delphi_ram",
      "indicator", "ranking", "definition", "policy",
      "standard_element"
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
    rand_disagreement = c(
      "table4", "ipras", "classic_relaxed", "classic_strict",
      "percent", "both", "classic"
    ),
    rand_di_threshold = 1,
    rand_round2_mode = c("in_person", "online", "mail"),
    rand_necessity = FALSE,
    rand_boundary = c("uncertain", "round_half_down", "round_half_up"),
    rand_iprr = 2.35,
    rand_cfa = 1.5,
    rand_qtype = 7L,
    rand_percent_high = 0.70,
    rand_percent_low = 0.15,
    rand_purpose = c("appropriateness", "recommendation"),
    max_rounds = 3,
    stability_class_pct = 0.80,
    stability_median_delta = 1,
    stability_high_pct_delta = 0.10,
    polarization_threshold = 0.25,
    cv_max = 0.25,
    ranking_topk = 5,
    mean_min = NA_real_,
    w_min = NA_real_,
    w_p_max = 0.05,
    mean_delta_max = NA_real_,
    min_experts = NA_real_,
    response_rate_min = 0.70,
    cr_min = 0.70,
    authority_scale = NULL,
    allow_new = NULL,
    min_independent = NULL,
    allow_single_with_evidence = NULL,
    max_suggestions = NULL,
    require_iteration = NULL,
    review_panel = NULL,
    open_first = FALSE,
    rationale = NULL,
    locked = TRUE
) {
  scenario <- match.arg(scenario)
  rand_disagreement <- match.arg(rand_disagreement)
  rand_round2_mode <- match.arg(rand_round2_mode)
  rand_boundary <- match.arg(rand_boundary)
  rand_purpose <- match.arg(rand_purpose)
  rand_qtype <- as.integer(rand_qtype)[1]
  .stop_if(is.na(rand_qtype) || !rand_qtype %in% 1:9, "rand_qtype \u5fc5\u987b\u662f 1 \u5230 9\u3002")
  rand_alias <- NULL
  if (identical(rand_disagreement, "classic")) {
    rand_alias <- "classic"
    rand_disagreement <- "table4"
    warning(
      "rand_disagreement=\"classic\" \u73b0\u6309\u624b\u518c\u8868 4 \u5224\u5b9a\u5206\u6b67\uff0c\u4e0d\u518d\u4f7f\u7528 ceiling(n/3)\u3002n \u4e3a 10 \u6216 16 \u7b49\u65f6\u4e0e 0.5.0 \u7684\u7ed3\u679c\u53ef\u80fd\u4e0d\u540c\u3002",
      call. = FALSE
    )
  }
  if (identical(scenario, "delphi_ram")) {
    if (!rand_disagreement %in% c("table4", "ipras", "percent")) {
      stop(
        "\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM \u7684\u4e3b\u89c4\u5219\u53ea\u80fd\u662f table4\u3001ipras \u6216 percent\u3002both \u4e0e 9 \u4eba\u4e25\u683c/\u5bbd\u677e\u5b9a\u4e49\u4e0d\u4f5c\u4e3a\u672c\u573a\u666f\u7684\u9501\u5b9a\u89c4\u5219\u3002",
        call. = FALSE
      )
    }
    if (missing(max_rounds)) max_rounds <- 3L
    if (missing(rand_round2_mode)) rand_round2_mode <- "mail"
    if (missing(high_pct)) high_pct <- NA_real_
    if (missing(basic_pct)) basic_pct <- NA_real_
    if (missing(reject_pct)) reject_pct <- NA_real_
    if (missing(rand_purpose) && identical(rand_disagreement, "percent")) {
      rand_purpose <- "recommendation"
    }
    if (identical(rand_disagreement, "percent")) {
      .stop_if(
        !is.finite(rand_percent_high) || !is.finite(rand_percent_low) ||
          rand_percent_low >= rand_percent_high,
        "\u767e\u5206\u6bd4\u89c4\u5219\u8981\u6c42 percent_low \u5c0f\u4e8e percent_high\u3002"
      )
    }
  }
  if (identical(rand_disagreement, "percent") && !identical(rand_purpose, "recommendation")) {
    warning(
      "\u767e\u5206\u6bd4\u89c4\u5219\u662f RAM \u4fee\u6539\u7248\uff0c\u5efa\u8bae\u628a rand_purpose \u8bbe\u4e3a recommendation\uff0c\u5e76\u5728\u62a5\u544a\u4e2d\u6807\u660e\u3002",
      call. = FALSE
    )
  }
  if (identical(scenario, "standard_element")) {
    ov <- .standard_element_overrides()
    if (missing(scale_min)) scale_min <- ov$scale_min
    if (missing(scale_max)) scale_max <- ov$scale_max
    if (missing(low_range)) low_range <- ov$low_range
    if (missing(mid_range)) mid_range <- ov$mid_range
    if (missing(high_range)) high_range <- ov$high_range
    if (missing(high_pct)) high_pct <- ov$high_pct
    if (missing(basic_pct)) basic_pct <- ov$basic_pct
    if (missing(median_min)) median_min <- ov$median_min
    if (missing(iqr_max)) iqr_max <- ov$iqr_max
    if (missing(reject_pct)) reject_pct <- ov$reject_pct
    if (missing(cv_max)) cv_max <- ov$cv_max
    if (missing(max_rounds)) max_rounds <- ov$max_rounds
    if (missing(mean_min)) mean_min <- ov$mean_min
    if (missing(w_min)) w_min <- ov$w_min
    if (missing(w_p_max)) w_p_max <- ov$w_p_max
    if (missing(mean_delta_max)) mean_delta_max <- ov$mean_delta_max
    if (missing(min_experts)) min_experts <- ov$min_experts
    if (missing(response_rate_min)) response_rate_min <- ov$response_rate_min
    if (missing(cr_min)) cr_min <- ov$cr_min
    if (missing(open_first)) open_first <- ov$open_first
  }
  ni_def <- .new_item_defaults(scenario)
  if (is.null(allow_new)) allow_new <- ni_def$allow
  if (is.null(min_independent)) min_independent <- ni_def$min_independent
  if (is.null(allow_single_with_evidence)) {
    allow_single_with_evidence <- ni_def$allow_single_with_evidence
  }
  if (is.null(max_suggestions)) max_suggestions <- ni_def$max_suggestions
  if (is.null(require_iteration)) require_iteration <- ni_def$require_iteration
  if (is.null(review_panel) || !nzchar(as.character(review_panel)[1])) {
    review_panel <- ni_def$review_panel
  }
  allow_new <- isTRUE(allow_new)
  allow_single_with_evidence <- isTRUE(allow_single_with_evidence)
  require_iteration <- isTRUE(require_iteration) && allow_new
  min_independent <- as.integer(min_independent)[1]
  max_suggestions <- as.integer(max_suggestions)[1]
  review_panel <- as.character(review_panel)[1]
  .stop_if(is.na(min_independent) || min_independent < 1L, "min_independent \u5fc5\u987b\u662f\u6b63\u6574\u6570\u3002")
  .stop_if(is.na(max_suggestions) || max_suggestions < 1L, "max_suggestions \u5fc5\u987b\u662f\u6b63\u6574\u6570\u3002")
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
  if (identical(scenario, "rand_ucla")) {
    if (missing(max_rounds)) max_rounds <- 2L
    if (missing(high_pct)) high_pct <- NA_real_
    if (missing(basic_pct)) basic_pct <- NA_real_
    if (missing(reject_pct)) reject_pct <- NA_real_
  }
  if (identical(scenario, "standard_element") && is.null(rationale)) {
    defaults <- .preset_defaults(scenario)
    if (!isTRUE(all.equal(mean_min, defaults$mean_min)) ||
        !isTRUE(all.equal(w_min, defaults$w_min)) ||
        !isTRUE(all.equal(cv_max, defaults$cv_max)) ||
        !isTRUE(all.equal(mean_delta_max, defaults$mean_delta_max))) {
      warning(
        "\u4fee\u6539\u4e86\u6838\u5fc3\u6280\u672f\u8981\u7d20\u7684\u5747\u6570\u6216\u534f\u8c03\u754c\u503c\u4f46\u672a\u586b\u5199 rationale\u3002\u62a5\u544a\u5c06\u6807\u8bb0\u4e3a\u201c\u65b9\u6848\u504f\u79bb\u9884\u8bbe\u4e14\u7406\u7531\u7f3a\u5931\u201d\u3002",
        call. = FALSE
      )
    }
  }

  defaults <- .preset_defaults(scenario)
  if (scenario %in% c("rand_ucla", "delphi_ram") && is.null(rationale)) {
    if (!identical(rand_disagreement, defaults$rand_disagreement) ||
        !isTRUE(all.equal(rand_iprr, defaults$rand_iprr)) ||
        !isTRUE(all.equal(rand_cfa, defaults$rand_cfa)) ||
        !identical(as.integer(rand_qtype), as.integer(defaults$rand_qtype)) ||
        !isTRUE(all.equal(rand_percent_high, defaults$rand_percent_high)) ||
        !isTRUE(all.equal(rand_percent_low, defaults$rand_percent_low)) ||
        !identical(rand_purpose, defaults$rand_purpose) ||
        !identical(rand_round2_mode, defaults$rand_round2_mode) ||
        !identical(isTRUE(rand_necessity), isTRUE(defaults$rand_necessity)) ||
        !identical(rand_boundary, defaults$rand_boundary)) {
      warning(
        "\u4fee\u6539\u4e86 RAND \u5224\u5b9a\u89c4\u5219\u6216 IPRAS/\u767e\u5206\u6bd4\u53c2\u6570\u4f46\u672a\u586b\u5199 rationale\u3002\u62a5\u544a\u5c06\u6807\u8bb0\u4e3a\u201c\u65b9\u6848\u504f\u79bb\u9884\u8bbe\u4e14\u7406\u7531\u7f3a\u5931\u201d\u3002",
        call. = FALSE
      )
    }
  }
  modified <- !isTRUE(all.equal(
    list(high_pct, basic_pct, median_min, iqr_max, reject_pct,
         comet_in_high, comet_in_low_max, comet_out_low, comet_out_high_max,
         rand_di_threshold, rand_disagreement, rand_iprr, rand_cfa, rand_qtype,
         rand_percent_high, rand_percent_low, rand_purpose, rand_round2_mode,
         isTRUE(rand_necessity), rand_boundary,
         cv_max, mean_min, w_min, mean_delta_max,
         min_experts, open_first, scale_max, max_rounds),
    list(defaults$high_pct, defaults$basic_pct, defaults$median_min, defaults$iqr_max,
         defaults$reject_pct, defaults$comet_in_high, defaults$comet_in_low_max,
         defaults$comet_out_low, defaults$comet_out_high_max,
         defaults$rand_di_threshold, defaults$rand_disagreement, defaults$rand_iprr,
         defaults$rand_cfa, defaults$rand_qtype, defaults$rand_percent_high,
         defaults$rand_percent_low, defaults$rand_purpose, defaults$rand_round2_mode,
         isTRUE(defaults$rand_necessity), defaults$rand_boundary,
         defaults$cv_max, defaults$mean_min, defaults$w_min,
         defaults$mean_delta_max, defaults$min_experts, defaults$open_first,
         defaults$scale_max, defaults$max_rounds)
  ))
  ni_modified <- !identical(allow_new, ni_def$allow) ||
    !identical(min_independent, as.integer(ni_def$min_independent)) ||
    !identical(allow_single_with_evidence, isTRUE(ni_def$allow_single_with_evidence)) ||
    !identical(max_suggestions, as.integer(ni_def$max_suggestions)) ||
    !identical(require_iteration, isTRUE(ni_def$require_iteration) && isTRUE(ni_def$allow)) ||
    !identical(review_panel, ni_def$review_panel)
  authority_scale <- .as_authority_scale(authority_scale)
  scale_modified <- !.authority_scale_same(authority_scale, authority_scales())
  modified <- isTRUE(modified) || ni_modified || scale_modified
  if (scale_modified && is.null(rationale)) {
    warning(
      "\u4fee\u6539\u4e86\u6743\u5a01\u7cfb\u6570\u8d4b\u5206\u8868\u4f46\u672a\u586b\u5199 rationale\u3002\u62a5\u544a\u5c06\u6807\u8bb0\u4e3a\u201c\u65b9\u6848\u504f\u79bb\u9884\u8bbe\u4e14\u7406\u7531\u7f3a\u5931\u201d\u3002",
      call. = FALSE
    )
  }
  if (ni_modified && is.null(rationale)) {
    warning(
      "\u4fee\u6539\u4e86\u7b2c\u4e00\u8f6e\u65b0\u589e\u6761\u76ee\u89c4\u5219\u4f46\u672a\u586b\u5199 rationale\u3002\u62a5\u544a\u5c06\u6807\u8bb0\u4e3a\u201c\u65b9\u6848\u504f\u79bb\u9884\u8bbe\u4e14\u7406\u7531\u7f3a\u5931\u201d\u3002",
      call. = FALSE
    )
  }

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
        ranking_topk = as.integer(ranking_topk),
        mean_min = mean_min,
        w_min = w_min,
        w_p_max = w_p_max,
        mean_delta_max = mean_delta_max,
        min_experts = min_experts,
        response_rate_min = response_rate_min,
        cr_min = cr_min
      ),
      open_first = isTRUE(open_first),
      new_items = list(
        allow = allow_new,
        min_independent = min_independent,
        allow_single_with_evidence = allow_single_with_evidence,
        max_suggestions = max_suggestions,
        require_iteration = require_iteration,
        review_panel = review_panel
      ),
      stability = list(
        max_rounds = as.integer(max_rounds),
        class_pct = stability_class_pct,
        median_delta = stability_median_delta,
        high_pct_delta = stability_high_pct_delta
      ),
      rand = list(
        rule = rand_disagreement,
        disagreement = rand_disagreement,
        legacy_alias = rand_alias,
        di_threshold = rand_di_threshold,
        round2_mode = rand_round2_mode,
        necessity = isTRUE(rand_necessity),
        boundary = rand_boundary,
        iprr = rand_iprr,
        cfa = rand_cfa,
        qtype = rand_qtype,
        percent_high = rand_percent_high,
        percent_low = rand_percent_low,
        purpose = rand_purpose,
        nonstandard_ipras = !isTRUE(all.equal(rand_iprr, 2.35)) ||
          !isTRUE(all.equal(rand_cfa, 1.5)) || !identical(rand_qtype, 7L)
      ),
      authority_scale = authority_scale,
      rationale = rationale,
      modified_from_preset = modified,
      locked = isTRUE(locked),
      combine_ram = identical(scenario, "delphi_ram")
    ),
    class = "delphi_protocol"
  )
}

.preset_common <- function() {
  list(
    scale_min = 1,
    scale_max = 9,
    low_range = c(1, 3),
    mid_range = c(4, 6),
    high_range = c(7, 9),
    max_rounds = 3,
    rand_disagreement = "table4",
    rand_di_threshold = 1,
    rand_round2_mode = "in_person",
    rand_necessity = FALSE,
    rand_boundary = "uncertain",
    rand_iprr = 2.35,
    rand_cfa = 1.5,
    rand_qtype = 7L,
    rand_percent_high = 0.70,
    rand_percent_low = 0.15,
    rand_purpose = "appropriateness",
    mean_min = NA_real_,
    w_min = NA_real_,
    w_p_max = 0.05,
    mean_delta_max = NA_real_,
    min_experts = NA_real_,
    response_rate_min = 0.70,
    cr_min = 0.70,
    open_first = FALSE
  )
}

.standard_element_overrides <- function() {
  list(
    scale_min = 1,
    scale_max = 5,
    low_range = c(1, 2),
    mid_range = c(3, 3),
    high_range = c(4, 5),
    high_pct = NA_real_,
    basic_pct = NA_real_,
    median_min = 4,
    iqr_max = 1,
    reject_pct = NA_real_,
    cv_max = 0.25,
    max_rounds = 4,
    mean_min = 3.5,
    w_min = 0.50,
    w_p_max = 0.05,
    mean_delta_max = 0.25,
    min_experts = 30,
    response_rate_min = 0.70,
    cr_min = 0.70,
    open_first = TRUE
  )
}

.preset_defaults <- function(scenario) {
  thresholds <- switch(
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
      rand_di_threshold = 1, rand_disagreement = "table4", max_rounds = 2,
      cv_max = 0.25
    ),
    delphi_ram = list(
      high_pct = NA_real_, basic_pct = NA_real_, median_min = 7, iqr_max = 2,
      reject_pct = NA_real_, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, rand_disagreement = "table4", max_rounds = 3,
      rand_round2_mode = "mail", rand_purpose = "appropriateness",
      rand_percent_high = 0.70, rand_percent_low = 0.15,
      cv_max = 0.25
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
    ),
    standard_element = list(
      high_pct = NA_real_, basic_pct = NA_real_, median_min = 4, iqr_max = 1,
      reject_pct = NA_real_, comet_in_high = 0.70, comet_in_low_max = 0.15,
      comet_out_low = 0.70, comet_out_high_max = 0.15,
      rand_di_threshold = 1, cv_max = 0.25
    )
  )
  out <- utils::modifyList(.preset_common(), thresholds)
  if (identical(scenario, "standard_element")) {
    out <- utils::modifyList(out, .standard_element_overrides())
  }
  out
}

.new_item_defaults <- function(scenario) {
  on <- scenario %in% c("clinical_consensus", "comet_cos", "definition", "indicator")
  list(
    allow = on,
    min_independent = 2L,
    allow_single_with_evidence = TRUE,
    max_suggestions = 5L,
    require_iteration = on,
    review_panel = if (on) {
      paste0(
        "\u6761\u76ee\u5ba1\u67e5\u7ec45\u4eba\uff1a\u4e2d\u533b\u4e34\u5e8a\u4e13\u5bb62\u540d\u3001",
        "\u4e34\u5e8a\u7814\u7a76\u65b9\u6cd5\u5b66\u4e13\u5bb61\u540d\u3001",
        "\u7edf\u8ba1\u5b66\u6216Delphi\u65b9\u6cd5\u4e13\u5bb61\u540d\u3001",
        "\u60a3\u8005\u6216\u60a3\u8005\u4ee3\u88681\u540d\u3002",
        "\u5148\u75312\u540d\u7814\u7a76\u8005\u72ec\u7acb\u7f16\u7801\uff0c\u4e0d\u4e00\u81f4\u65f6\u7531\u7b2c3\u540d\u7814\u7a76\u8005\u88c1\u51b3\u3002"
      )
    } else {
      ""
    }
  )
}

#' 按场景取出预设分析方案
#'
#' 按 `scenario` 填入该场景的默认阈值，再交给 [delphi_protocol()] 锁定。
#' 这是第一轮问卷发放前的推荐入口：先选场景，再只覆盖确需修改的参数。
#'
#' @param scenario 场景标识，见 [delphi_protocol()]。默认
#'   `"clinical_consensus"`。
#' @param ... 覆盖预设的参数（必须是 [delphi_protocol()] 的形参名）。
#'   若改变共识或分歧阈值，请同时提供 `rationale`。未出现在 `...` 中的参数
#'   保持该场景默认值。
#'
#' @return `delphi_protocol` 对象，字段与 [delphi_protocol()] 相同。
#'
#' @section 各场景实际生效的参数:
#' - 医学专家共识 (clinical_consensus)：`high_pct`、`basic_pct`、
#'   `median_min`、`iqr_max`、`reject_pct`、`polarization_threshold`、
#'   稳定性三条与 `max_rounds`。COMET/RAND/`cv_max` 写入对象但不驱动分类。
#' - COMET (comet_cos)：`comet_in_*`、`comet_out_*`。`high_pct` 默认改为
#'   0.70 以与 70/15 叙述一致，但 [classify_consensus()] 走 COMET 分支。
#' - RAND/UCLA (rand_ucla)：`rand_disagreement`（默认 `table4`）、
#'   `rand_di_threshold`、`rand_round2_mode`、`rand_boundary`、IPRAS 常数。
#'   `high_pct` 为 `NA`。`max_rounds` 默认为 2。百分比规则须标明修改版。
#' - 改良德尔菲结合 RAM (delphi_ram)：`rand_disagreement` 只能是
#'   `table4`、`ipras` 或 `percent`。默认匿名函询、最多 3 轮。
#'   `high_pct` 为 `NA`，不驱动分类。入口是 [delphi_ram_protocol()]。
#' - 指标体系 (indicator)：`basic_pct`、`median_min`、`iqr_max`、`cv_max`。
#' - 排序 (ranking)：共识百分比多为 `NA`；用 [rank_priorities()] 与
#'   `ranking_topk`。
#' - 定义 (definition)：接近临床，但基本共识阈值默认也是 0.80。
#' - Policy (policy)：不判定必须共识；阈值多为 `NA`，报告以分布为主。
#' - 核心技术要素 (standard_element)：5 分量表。纳入看重要程度均数
#'   `mean_min` 与 `cv_max`；结束看 `w_min`、`w_p_max` 与 `mean_delta_max`。
#'   `open_first = TRUE` 时第 1 轮为开放式征询。Median / 高区比例不驱动分类。
#'
#' @examples
#' preset_protocol("comet_cos")
#' preset_protocol("rand_ucla")
#' protocol_table(preset_protocol("indicator"))
#'
#' # 覆盖单参数必须写理由
#' preset_protocol(
#'   "clinical_consensus",
#'   iqr_max = 3,
#'   rationale = "条目表述较长，允许 IQR=3 作为高度共识的离散上限。"
#' )
#'
#' @seealso [delphi_protocol()], [protocol_table()],
#'   vignette("preset-protocol", package = "delphir")
#'
#' @export
preset_protocol <- function(scenario = "clinical_consensus", ...) {
  scenario <- match.arg(
    scenario,
    c("clinical_consensus", "comet_cos", "rand_ucla", "delphi_ram",
      "indicator", "ranking", "definition", "policy",
      "standard_element")
  )
  dots <- list(...)
  args <- c(list(scenario = scenario), .preset_defaults(scenario))
  if (length(dots)) args[names(dots)] <- dots
  do.call(delphi_protocol, args)
}

#' 锁定改良德尔菲结合 RAM 的方案
#'
#' 这是一套独立流程：匿名多轮函询，用预先锁定的 RAM 规则判定一致、分歧、
#' 适宜性和共识。它不走临床共识的高区比例复合标准，也不是经典两轮面对面
#' 适宜性专家组。分析请用 [run_delphi_ram()]，问卷请用 [delphi_ram_questionnaire()]。
#'
#' @param rule `"table4"`（Fitch 等，2001，表 4）、`"ipras"` 或 `"percent"`。
#'   百分比规则属于 RAM 修改版。
#' @param percent_high,percent_low 百分比规则的高区下限与低区上限。默认 0.70
#'   与 0.15。只在 `rule = "percent"` 时驱动共识分类。
#' @param max_rounds 计划最多轮次，默认 3。超过 2 轮是对经典 RAM 的流程偏离，
#'   报告会说明。分歧可以成为最终结果。
#' @param round_mode `"mail"`（默认，再次函询）、`"online"` 或 `"in_person"`。
#' @param rationale 偏离预设时的理由。改规则、改百分比或改函询形式而未填写时会警告。
#' @param ... 传给 [preset_protocol()] 的其他参数，例如量表端点。不要在这里再传
#'   `rand_disagreement`，规则由 `rule` 指定。
#'
#' @return `delphi_protocol`。`scenario` 为 `delphi_ram`，`combine_ram` 为 `TRUE`。
#'
#' @section 输入输出:
#' **输入**为标量。比例必须在 0 与 1 之间，且 `percent_low` 应小于 `percent_high`。
#'
#' **输出**供 [delphi_study()] 在第一轮前锁定。临床 `high_pct` 为 `NA`，
#' [classify_delphi_ram()] 不读取它。
#'
#' @section 文献:
#' Fitch K, Bernstein SJ, Aguilar MD, et al. The RAND/UCLA Appropriateness
#' Method User's Manual. Santa Monica, CA: RAND; 2001.
#'
#' @examples
#' p <- delphi_ram_protocol("table4")
#' p$scenario
#' p$rand$rule
#' p$combine_ram
#'
#' p_ipras <- delphi_ram_protocol("ipras")
#' p_ipras$rand$rule
#'
#' p_pct <- delphi_ram_protocol("percent", percent_high = 0.75, percent_low = 0.15)
#' p_pct$rand$purpose
#'
#' @seealso [run_delphi_ram()], [classify_delphi_ram()], [delphi_ram_questionnaire()]
#'
#' @export
delphi_ram_protocol <- function(rule = c("table4", "ipras", "percent"),
                                percent_high = 0.70,
                                percent_low = 0.15,
                                max_rounds = 3L,
                                round_mode = c("mail", "online", "in_person"),
                                rationale = NULL,
                                ...) {
  rule <- match.arg(rule)
  round_mode <- match.arg(round_mode)
  .stop_if(
    !is.finite(percent_high) || !is.finite(percent_low) ||
      percent_high <= 0 || percent_high > 1 || percent_low < 0 || percent_low >= 1 ||
      percent_low >= percent_high,
    "percent_high \u4e0e percent_low \u5fc5\u987b\u5728 0 \u4e0e 1 \u4e4b\u95f4\uff0c\u4e14 percent_low \u5c0f\u4e8e percent_high\u3002"
  )
  if (is.null(rationale)) {
    rationale <- switch(
      rule,
      table4 = "\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM\uff1a\u533f\u540d\u591a\u8f6e\u51fd\u8be2\uff0c\u5206\u6b67\u4e0e\u9002\u5b9c\u6027\u6309\u624b\u518c\u8868 4 \u5224\u5b9a\u3002\u8fd9\u4e0d\u662f\u7ecf\u5178\u4e24\u8f6e\u9762\u5bf9\u9762\u9002\u5b9c\u6027\u4e13\u5bb6\u7ec4\u3002",
      ipras = "\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM\uff1a\u5206\u6b67\u6309 IPRAS\uff08IPR \u4e25\u683c\u5927\u4e8e IPRAS\uff09\u5224\u5b9a\u3002\u8fd9\u4e0d\u662f\u7ecf\u5178\u4e24\u8f6e\u9762\u5bf9\u9762\u9002\u5b9c\u6027\u4e13\u5bb6\u7ec4\u3002",
      percent = "\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM\uff1a\u4f7f\u7528\u9884\u5148\u9501\u5b9a\u7684\u767e\u5206\u6bd4\u5171\u8bc6\u89c4\u5219\uff0c\u5c5e\u4e8e RAM \u4fee\u6539\u7248\uff0c\u4e0d\u5957\u7528 IPRAS \u5e38\u6570\u3002"
    )
  }
  preset_protocol(
    "delphi_ram",
    rand_disagreement = rule,
    rand_percent_high = percent_high,
    rand_percent_low = percent_low,
    max_rounds = as.integer(max_rounds),
    rand_round2_mode = round_mode,
    rand_purpose = if (identical(rule, "percent")) "recommendation" else "appropriateness",
    rationale = rationale,
    ...
  )
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
  if (identical(x$scenario, "rand_ucla")) {
    cat(sprintf(
      "  RAND \u89c4\u5219: %s\uff1b\u7b2c 2 \u8f6e: %s\uff1b\u5fc5\u8981\u6027: %s\n",
      x$rand$rule %||% x$rand$disagreement,
      x$rand$round2_mode %||% "in_person",
      if (isTRUE(x$rand$necessity)) "\u8ba1\u5212\u53e6\u8bc4" else "\u4e0d\u8bc4"
    ))
  }
  if (identical(x$scenario, "delphi_ram")) {
    cat(sprintf(
      "  \u7ed3\u5408 RAM: \u662f\uff1b\u89c4\u5219: %s\uff1b\u51fd\u8be2: %s\uff1b\u6700\u591a %s \u8f6e\n",
      x$rand$rule %||% x$rand$disagreement,
      x$rand$round2_mode %||% "mail",
      x$stability$max_rounds %||% 3L
    ))
    cat("  \u4e34\u5e8a high_pct \u4e0d\u53c2\u4e0e\u672c\u573a\u666f\u5206\u7c7b\u3002\n")
  }
  if (identical(x$scenario, "standard_element")) {
    cat(sprintf(
      "  \u7eb3\u5165: \u91cd\u8981\u7a0b\u5ea6\u5747\u6570 \u2265 %s \u4e14 CV \u2264 %s\uff1b\u7ec8\u6b62: W \u2265 %s\u3001P < %s\u3001\u5747\u6570\u53d8\u5316 < %s\n",
      x$rules$mean_min, x$rules$cv_max, x$rules$w_min, x$rules$w_p_max, x$rules$mean_delta_max
    ))
    cat(sprintf("  \u7b2c 1 \u8f6e\u5f00\u653e\u5f0f\u5f81\u8be2: %s\n", if (isTRUE(x$open_first)) "\u662f" else "\u5426"))
  }
  if (!is.null(x$new_items) && isTRUE(x$new_items$allow)) {
    cat(sprintf(
      "  \u65b0\u589e\u6761\u76ee: \u5141\u8bb8\uff1b\u2265%s \u4eba\u72ec\u7acb\u63d0\u51fa\uff0c\u6216 1 \u4eba\u4e14\u6709\u5916\u90e8\u8bc1\u636e\uff1b\u81f3\u5c11\u4e24\u8f6e\u8bc4\u5206\u540e\u624d\u80fd\u6700\u7ec8\u7eb3\u5165\n",
      x$new_items$min_independent
    ))
  }
  cat(sprintf("  \u9501\u5b9a: %s\n", if (x$locked) "\u662f" else "\u5426"))
  if (isTRUE(x$modified_from_preset)) {
    cat("  \u5df2\u504f\u79bb\u9884\u8bbe")
    if (!is.null(x$rationale)) cat(sprintf("\uff1b\u7406\u7531: %s", x$rationale))
    cat("\n")
  }
  invisible(x)
}

#' 把锁定方案展开为参数表
#'
#' 一行一个参数，含当前值、该场景预设值、是否偏离，以及简要方法学含义。
#' 供帮助文档、问卷封面和 [delphi_report()] 完整打印方案使用。
#'
#' @param protocol [delphi_protocol()] 对象。
#'
#' @return 数据框，列：`parameter`、`value`、`preset`、`modified`、`meaning`。
#'
#' @examples
#' protocol_table(preset_protocol("clinical_consensus"))
#'
#' @export
protocol_table <- function(protocol) {
  .stop_if(!inherits(protocol, "delphi_protocol"), "\u9700\u8981 delphi_protocol \u5bf9\u8c61\u3002")
  d <- .preset_defaults(protocol$scenario)
  ni_def <- .new_item_defaults(protocol$scenario)
  recorded_new <- !is.null(protocol$new_items)
  ni <- if (recorded_new) protocol$new_items else ni_def
  fmt_rng <- function(x) paste(x, collapse = "\u2013")
  fmt_num <- function(x) {
    if (length(x) == 0L || (length(x) == 1L && is.na(x))) return("NA")
    paste(as.character(x), collapse = ", ")
  }
  rows <- list(
    c("scenario", protocol$scenario, protocol$scenario, .scenario_label(protocol$scenario)),
    c("scale_min", fmt_num(protocol$scale_min), fmt_num(d$scale_min), "\u91cf\u8868\u4e0b\u7aef"),
    c("scale_max", fmt_num(protocol$scale_max), fmt_num(d$scale_max), "\u91cf\u8868\u4e0a\u7aef"),
    c("low_range", fmt_rng(protocol$low_range), fmt_rng(d$low_range), "\u4f4e\u533a\uff08\u503e\u5411\u6392\u9664\uff09"),
    c("mid_range", fmt_rng(protocol$mid_range), fmt_rng(d$mid_range), "\u4e2d\u533a"),
    c("high_range", fmt_rng(protocol$high_range), fmt_rng(d$high_range), "\u9ad8\u533a\uff08\u8d5e\u540c/\u91cd\u8981/\u9002\u5b9c\uff09"),
    c("high_pct", fmt_num(protocol$rules$high_pct), fmt_num(d$high_pct),
      "\u9ad8\u5ea6\u5171\u8bc6\uff1a\u9ad8\u533a\u6bd4\u4f8b\u4e0b\u9650"),
    c("basic_pct", fmt_num(protocol$rules$basic_pct), fmt_num(d$basic_pct),
      "\u57fa\u672c\u5171\u8bc6\uff1a\u9ad8\u533a\u6bd4\u4f8b\u4e0b\u9650"),
    c("median_min", fmt_num(protocol$rules$median_min), fmt_num(d$median_min),
      "\u5171\u8bc6\u6240\u9700 Median \u4e0b\u9650"),
    c("iqr_max", fmt_num(protocol$rules$iqr_max), fmt_num(d$iqr_max),
      "\u9ad8\u5ea6\u5171\u8bc6\u5141\u8bb8\u7684\u6700\u5927 IQR"),
    c("reject_pct", fmt_num(protocol$rules$reject_pct), fmt_num(d$reject_pct),
      "\u503e\u5411\u6392\u9664\uff1a\u4f4e\u533a\u6bd4\u4f8b\u4e0b\u9650"),
    c("comet_in_high", fmt_num(protocol$rules$comet_in_high), fmt_num(d$comet_in_high),
      "COMET \u7eb3\u5165\uff1a\u9ad8\u533a\u6bd4\u4f8b\u4e0b\u9650"),
    c("comet_in_low_max", fmt_num(protocol$rules$comet_in_low_max), fmt_num(d$comet_in_low_max),
      "COMET \u7eb3\u5165\uff1a\u4f4e\u533a\u6bd4\u4f8b\u4e0a\u9650"),
    c("comet_out_low", fmt_num(protocol$rules$comet_out_low), fmt_num(d$comet_out_low),
      "COMET \u6392\u9664\uff1a\u4f4e\u533a\u6bd4\u4f8b\u4e0b\u9650"),
    c("comet_out_high_max", fmt_num(protocol$rules$comet_out_high_max), fmt_num(d$comet_out_high_max),
      "COMET \u6392\u9664\uff1a\u9ad8\u533a\u6bd4\u4f8b\u4e0a\u9650"),
    c("rand_disagreement", protocol$rand$rule %||% protocol$rand$disagreement, d$rand_disagreement %||% "table4",
      "RAND \u89c4\u5219\uff1atable4 / ipras / classic_relaxed / classic_strict / percent / both"),
    c("rand_di_threshold", fmt_num(protocol$rand$di_threshold), fmt_num(d$rand_di_threshold),
      "IPRAS\uff1aIPR > IPRAS \u00d7 \u8be5\u500d\u7387\u624d\u7b97\u5206\u6b67"),
    c("rand_round2_mode", protocol$rand$round2_mode %||% "in_person", d$rand_round2_mode %||% "in_person",
      "\u7b2c 2 \u8f6e\u5f62\u5f0f\uff1ain_person / online / mail"),
    c("rand_necessity", if (isTRUE(protocol$rand$necessity)) "TRUE" else "FALSE",
      if (isTRUE(d$rand_necessity)) "TRUE" else "FALSE",
      "\u662f\u5426\u53e6\u505a\u5fc5\u8981\u6027\u8bc4\u4ef7\uff08\u4e0d\u662f\u7b2c 3 \u8f6e\u9002\u5b9c\u6027\uff09"),
    c("rand_boundary", protocol$rand$boundary %||% "uncertain", d$rand_boundary %||% "uncertain",
      "\u4e2d\u4f4d\u6570 3.5 \u6216 6.5 \u7684\u5904\u7406"),
    c("rand_iprr", fmt_num(protocol$rand$iprr %||% 2.35), fmt_num(d$rand_iprr %||% 2.35),
      "IPRAS \u5e38\u6570 IPRr\uff0c\u6807\u51c6\u4e3a 2.35"),
    c("rand_cfa", fmt_num(protocol$rand$cfa %||% 1.5), fmt_num(d$rand_cfa %||% 1.5),
      "IPRAS \u975e\u5bf9\u79f0\u6821\u6b63 CFA\uff0c\u6807\u51c6\u4e3a 1.5"),
    c("rand_qtype", fmt_num(protocol$rand$qtype %||% 7L), fmt_num(d$rand_qtype %||% 7L),
      "\u5206\u4f4d\u6570\u7b97\u6cd5\uff0c\u6807\u51c6\u4e3a type 7"),
    c("rand_percent_high", fmt_num(protocol$rand$percent_high %||% 0.70), fmt_num(d$rand_percent_high %||% 0.70),
      "\u767e\u5206\u6bd4\u89c4\u5219\uff1a\u9ad8\u533a\u6bd4\u4f8b\u4e0b\u9650\uff08RAM \u4fee\u6539\u7248\uff09"),
    c("rand_percent_low", fmt_num(protocol$rand$percent_low %||% 0.15), fmt_num(d$rand_percent_low %||% 0.15),
      "\u767e\u5206\u6bd4\u89c4\u5219\uff1a\u4f4e\u533a\u6bd4\u4f8b\u4e0a\u9650\uff08RAM \u4fee\u6539\u7248\uff09"),
    c("rand_purpose", protocol$rand$purpose %||% "appropriateness", d$rand_purpose %||% "appropriateness",
      "appropriateness \u6216 recommendation"),
    c("max_rounds", fmt_num(protocol$stability$max_rounds), fmt_num(d$max_rounds),
      "\u8ba1\u5212\u6700\u591a\u8f6e\u6b21"),
    c("stability_class_pct", fmt_num(protocol$stability$class_pct), "0.8",
      "\u7a33\u5b9a\u6027\uff1a\u5206\u7c7b\u4e0d\u53d8\u6bd4\u4f8b"),
    c("stability_median_delta", fmt_num(protocol$stability$median_delta), "1",
      "\u7a33\u5b9a\u6027\uff1aMedian \u53d8\u5316\u4e0a\u9650"),
    c("stability_high_pct_delta", fmt_num(protocol$stability$high_pct_delta), "0.1",
      "\u7a33\u5b9a\u6027\uff1a\u9ad8\u533a\u6bd4\u4f8b\u53d8\u5316\u4e0a\u9650"),
    c("polarization_threshold", fmt_num(protocol$rules$polarization_threshold), "0.25",
      "\u4e24\u6781\u5206\u5316\uff1a\u4e24\u7aef\u540c\u65f6\u8fbe\u5230\u7684\u6bd4\u4f8b"),
    c("cv_max", fmt_num(protocol$rules$cv_max), fmt_num(d$cv_max),
      "\u6307\u6807\u4f53\u7cfb\u5141\u8bb8\u7684\u6700\u5927 CV"),
    c("ranking_topk", fmt_num(protocol$rules$ranking_topk), "5",
      "\u4f18\u5148\u7ea7 Top-k"),
    c("mean_min", fmt_num(protocol$rules$mean_min), fmt_num(d$mean_min),
      "\u6838\u5fc3\u6280\u672f\u8981\u7d20\uff1a\u91cd\u8981\u7a0b\u5ea6\u5747\u6570\u4e0b\u9650"),
    c("w_min", fmt_num(protocol$rules$w_min), fmt_num(d$w_min),
      "\u7ed3\u675f\u8c03\u67e5\uff1aKendall W \u4e0b\u9650"),
    c("w_p_max", fmt_num(protocol$rules$w_p_max), fmt_num(d$w_p_max),
      "\u534f\u8c03\u7cfb\u6570\u5361\u65b9\u68c0\u9a8c P \u503c\u4e0a\u9650"),
    c("mean_delta_max", fmt_num(protocol$rules$mean_delta_max), fmt_num(d$mean_delta_max),
      "\u76f8\u90bb\u8f6e\u91cd\u8981\u7a0b\u5ea6\u5747\u6570\u7edd\u5bf9\u53d8\u5316\u7684\u5e73\u5747\u503c\u4e0a\u9650"),
    c("min_experts", fmt_num(protocol$rules$min_experts), fmt_num(d$min_experts),
      "\u8c03\u67e5\u5bf9\u8c61\u4eba\u6570\u53c2\u8003\u4e0b\u9650"),
    c("response_rate_min", fmt_num(protocol$rules$response_rate_min), fmt_num(d$response_rate_min),
      "\u79ef\u6781\u7cfb\u6570\uff08\u56de\u6536\u7387\uff09\u53c2\u8003\u4e0b\u9650"),
    c("cr_min", fmt_num(protocol$rules$cr_min), fmt_num(d$cr_min),
      "\u6743\u5a01\u7cfb\u6570\u7ec4\u5747\u6570\u53c2\u8003\u4e0b\u9650"),
    c("authority_cs", unname(.encode_authority_scale(.protocol_authority_scale(protocol))["authority_cs"]),
      unname(.encode_authority_scale(authority_scales())["authority_cs"]),
      "\u719f\u6089\u7a0b\u5ea6\u8d4b\u5206\u8868\uff0c\u987b\u5728\u7b2c 1 \u8f6e\u524d\u9501\u5b9a"),
    c("authority_ca", unname(.encode_authority_scale(.protocol_authority_scale(protocol))["authority_ca"]),
      unname(.encode_authority_scale(authority_scales())["authority_ca"]),
      "\u5224\u65ad\u4f9d\u636e\u8d4b\u5206\u8868\uff0c\u987b\u5728\u7b2c 1 \u8f6e\u524d\u9501\u5b9a"),
    c("open_first", if (isTRUE(protocol$open_first)) "TRUE" else "FALSE",
      if (isTRUE(d$open_first)) "TRUE" else "FALSE",
      "\u7b2c 1 \u8f6e\u662f\u5426\u4e3a\u5f00\u653e\u5f0f\u5f81\u8be2"),
    c("allow_new", if (isTRUE(ni$allow)) "TRUE" else "FALSE",
      if (isTRUE(ni_def$allow)) "TRUE" else "FALSE",
      "\u662f\u5426\u5141\u8bb8\u7b2c 1 \u8f6e\u7ed3\u6784\u5316\u8865\u5145\u6761\u76ee"),
    c("min_independent", fmt_num(ni$min_independent), fmt_num(ni_def$min_independent),
      "\u4e3b\u901a\u9053\uff1a\u81f3\u5c11\u51e0\u540d\u4e13\u5bb6\u72ec\u7acb\u63d0\u51fa"),
    c("allow_single_with_evidence", if (isTRUE(ni$allow_single_with_evidence)) "TRUE" else "FALSE",
      if (isTRUE(ni_def$allow_single_with_evidence)) "TRUE" else "FALSE",
      "\u4f8b\u5916\u901a\u9053\uff1a1 \u4eba\u63d0\u51fa\u4e14\u6709\u5916\u90e8\u8bc1\u636e"),
    c("max_suggestions", fmt_num(ni$max_suggestions), fmt_num(ni_def$max_suggestions),
      "\u6bcf\u4f4d\u4e13\u5bb6\u6700\u591a\u63a8\u8350\u7684\u65b0\u6761\u76ee\u6570"),
    c("require_iteration", if (isTRUE(ni$require_iteration)) "TRUE" else "FALSE",
      if (isTRUE(ni_def$require_iteration)) "TRUE" else "FALSE",
      "\u65b0\u6761\u76ee\u81f3\u5c11\u4e24\u8f6e\u8bc4\u5206\u540e\u624d\u80fd\u6700\u7ec8\u7eb3\u5165"),
    c("review_panel", ni$review_panel %||% "", ni_def$review_panel %||% "",
      "\u6761\u76ee\u5ba1\u67e5\u7ec4\u6784\u6210\uff1b\u8f6f\u4ef6\u4e0d\u4ee3\u66ff\u4eba\u5de5\u7f16\u7801"),
    c("locked", if (isTRUE(protocol$locked)) "TRUE" else "FALSE", "TRUE",
      "\u662f\u5426\u7981\u6b62\u5206\u6790\u9636\u6bb5\u56de\u6539\u9608\u503c"),
    c("modified_from_preset", if (isTRUE(protocol$modified_from_preset)) "TRUE" else "FALSE",
      "FALSE", "\u662f\u5426\u504f\u79bb\u8be5\u573a\u666f\u9884\u8bbe"),
    c("rationale", protocol$rationale %||% "", "",
      "\u504f\u79bb\u9884\u8bbe\u7684\u7406\u7531\uff08\u62a5\u544a\u5fc5\u987b\u5199\u660e\uff09")
  )
  if (!recorded_new) {
    drop <- vapply(rows, function(z) {
      z[[1]] %in% c(
        "allow_new", "min_independent", "allow_single_with_evidence",
        "max_suggestions", "require_iteration", "review_panel"
      )
    }, logical(1))
    rows <- rows[!drop]
  }
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("parameter", "value", "preset", "meaning")
  skip <- c("scenario", "modified_from_preset")
  out$modified <- !out$parameter %in% skip & out$value != out$preset
  out[, c("parameter", "value", "preset", "modified", "meaning")]
}
