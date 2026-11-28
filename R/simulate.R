#' 模拟医学专家共识 Delphi 数据
#'
#' 生成带方法学教学价值的非完美数据：含脱落、无法判断、高度共识、基本共识、
#' 未达共识、两极分化和轮次间收敛。
#'
#' @param n_experts 专家人数，默认 40。
#' @param n_items 条目数，默认 20。
#' @param n_rounds 轮次数，默认 2。
#' @param seed 随机种子，保证可复现。
#' @param include_authority 是否生成熟悉程度/判断依据原始勾选及 Ca/Cs，默认 `TRUE`。
#'
#' @return [delphi_study()] 对象，`meta` 中含条目原型说明。
#'
#' @section 输出结构:
#' 与真实研究相同：`protocol`、`panel`、`items`、`ratings`、`comments`。
#' 条目 `pattern` 列标记教学原型：`converge_high`、`basic`、`no_consensus`、
#' `polar`、`reject`、`improve`。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 16, n_items = 8, n_rounds = 2, seed = 2026)
#' table(study$items$pattern)
#'
#' @export
simulate_delphi <- function(n_experts = 40, n_items = 20, n_rounds = 2,
                            seed = 2026, include_authority = TRUE) {
  set.seed(seed)
  n_experts <- as.integer(n_experts)
  n_items <- as.integer(n_items)
  n_rounds <- as.integer(n_rounds)
  .stop_if(n_experts < 6 || n_items < 4 || n_rounds < 1, "\u6a21\u62df\u89c4\u6a21\u8fc7\u5c0f\u3002")

  protocol <- preset_protocol("clinical_consensus")
  stakeholders <- c("\u4e34\u5e8a\u533b\u751f", "\u65b9\u6cd5\u5b66\u4e13\u5bb6", "\u836f\u5b66\u4e13\u5bb6", "\u76d1\u7ba1\u4eba\u5458")
  n_st <- c(
    max(2L, round(n_experts * 0.50)),
    max(1L, round(n_experts * 0.25)),
    max(1L, round(n_experts * 0.15)),
    max(1L, n_experts - 0L)
  )
  n_st[4] <- max(1L, n_experts - sum(n_st[1:3]))
  if (sum(n_st) != n_experts) n_st[1] <- n_st[1] + (n_experts - sum(n_st))

  panel <- data.frame(
    expert_id = sprintf("E%02d", seq_len(n_experts)),
    stakeholder = rep(stakeholders, n_st)[seq_len(n_experts)],
    specialty = NA_character_,
    region = sample(c("\u534e\u5317", "\u534e\u4e1c", "\u534e\u5357", "\u897f\u5357", "\u4e1c\u5317"), n_experts, TRUE),
    title = sample(c("\u4e3b\u4efb\u533b\u5e08", "\u526f\u4e3b\u4efb\u533b\u5e08", "\u6559\u6388", "\u7814\u7a76\u5458"), n_experts, TRUE),
    years = sample(8:32, n_experts, TRUE),
    invited = TRUE,
    stringsAsFactors = FALSE
  )
  panel$specialty <- ifelse(
    panel$stakeholder == "\u4e34\u5e8a\u533b\u751f",
    sample(c("\u80bf\u7624\u5185\u79d1", "\u5fc3\u8840\u7ba1", "\u611f\u67d3", "\u80be\u75c5"), n_experts, TRUE),
    ifelse(panel$stakeholder == "\u65b9\u6cd5\u5b66\u4e13\u5bb6", "\u4e34\u5e8a\u6d41\u884c\u75c5\u5b66",
           ifelse(panel$stakeholder == "\u836f\u5b66\u4e13\u5bb6", "\u4e34\u5e8a\u836f\u5b66", "\u836f\u54c1\u76d1\u7ba1"))
  )
  if (include_authority) {
    panel <- cbind(panel, .simulate_authority_raw(n_experts))
    scored <- score_authority(panel)
    panel$Ca <- scored$Ca
    panel$Cs <- scored$Cs
  }

  patterns <- .assign_patterns(n_items)
  item_text <- .clinical_item_bank(n_items)
  items <- data.frame(
    item_id = sprintf("C%02d", seq_len(n_items)),
    item_text = item_text,
    domain = sample(c("\u65b9\u6848\u8bbe\u8ba1", "\u5b9e\u65bd", "\u7edf\u8ba1", "\u5b89\u5168", "\u62a5\u544a"), n_items, TRUE),
    pattern = patterns,
    evidence_level = sample(c("\u7cfb\u7edf\u8bc4\u4ef7", "RCT", "\u89c2\u5bdf\u6027\u7814\u7a76", "\u6cd5\u89c4/\u6307\u5bfc\u539f\u5219", "\u4e13\u5bb6\u610f\u89c1"),
                            n_items, TRUE),
    stringsAsFactors = FALSE
  )

  ratings <- .simulate_ratings(panel, items, n_rounds, scenario = "clinical")
  comments <- .simulate_comments(panel, items, ratings)

  meta <- list(
    title = "\u836f\u7269\u4e34\u5e8a\u8bd5\u9a8c\u8d28\u91cf\u6e90\u4e8e\u8bbe\u8ba1\u5b9e\u65bd\u4e13\u5bb6\u5171\u8bc6\uff08\u6a21\u62df\u6570\u636e\uff09",
    objective = "\u5c31\u4e34\u5e8a\u8bd5\u9a8c\u65b9\u6848\u8bbe\u8ba1\u4e0e\u5b9e\u65bd\u4e2d\u7684\u5173\u952e\u8d28\u91cf\u8981\u6c42\u5f62\u6210\u4e13\u5bb6\u5171\u8bc6\u3002",
    users = "\u7533\u529e\u8005\u3001\u7814\u7a76\u8005\u3001\u4f26\u7406\u59d4\u5458\u4f1a\u4e0e\u76d1\u7ba1\u76f8\u5173\u4eba\u5458",
    geography = "\u4e2d\u56fd\u5927\u9646",
    evidence_note = "\u6761\u76ee\u6765\u81ea ICH E6(R3)\u3001GCP \u53ca\u65b9\u6cd5\u5b66\u7efc\u8ff0\u540e\u7684\u521d\u59cb\u6761\u76ee\u6c60\uff08\u6a21\u62df\uff09\u3002",
    pilot_tested = TRUE,
    anonymity = "\u8bc4\u5206\u533f\u540d\uff1b\u62a5\u544a\u4ec5\u6c47\u603b\u7edf\u8ba1\u3002",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\uff0c\u65e0\u9700\u4f26\u7406\u5ba1\u67e5\u3002"
  )
  delphi_study(protocol, panel, items, ratings, comments, meta = meta, validate = TRUE)
}

#' 模拟 RAND/UCLA 适宜性评价数据
#'
#' 生成 9 人左右多学科面板对若干临床指征的 1–9 适宜性评分，包含适宜、不适宜、
#' 不确定和明确分歧的指征，以及两轮评分。
#'
#' @param n_experts 面板人数，默认 9。
#' @param n_items 指征数，默认 24。
#' @param n_rounds 轮次数，默认 2。
#' @param seed 随机种子。
#'
#' @return [delphi_study()] 对象，场景为 `rand_ucla`。
#'
#' @examples
#' ram <- simulate_rand_ucla(n_experts = 9, n_items = 12, seed = 7)
#' ram$protocol$scenario
#'
#' @export
simulate_rand_ucla <- function(n_experts = 9, n_items = 24, n_rounds = 2, seed = 2026) {
  set.seed(seed)
  n_experts <- as.integer(n_experts)
  n_items <- as.integer(n_items)
  protocol <- preset_protocol("rand_ucla")
  specs <- c("\u5fc3\u5185\u79d1", "\u5fc3\u810f\u5916\u79d1", "\u5f71\u50cf\u79d1", "\u5168\u79d1", "\u65b9\u6cd5\u5b66")
  panel <- data.frame(
    expert_id = sprintf("P%d", seq_len(n_experts)),
    stakeholder = "\u4e34\u5e8a\u4e13\u5bb6",
    specialty = rep(specs, length.out = n_experts),
    region = sample(c("\u5317\u533a", "\u5357\u533a", "\u4e1c\u533a"), n_experts, TRUE),
    title = "\u4e13\u5bb6\u7ec4\u6210\u5458",
    years = sample(12:35, n_experts, TRUE),
    invited = TRUE,
    stringsAsFactors = FALSE
  )
  patterns <- .assign_ram_patterns(n_items)
  items <- data.frame(
    item_id = sprintf("IND%02d", seq_len(n_items)),
    item_text = .ram_item_bank(n_items),
    domain = "\u9002\u5b9c\u6027",
    indication = sprintf("\u4e34\u5e8a\u573a\u666f %s", seq_len(n_items)),
    procedure = "\u51a0\u72b6\u52a8\u8109\u8840\u8fd0\u91cd\u5efa\uff08\u6a21\u62df\uff09",
    evidence_level = sample(c("A", "B", "C"), n_items, TRUE, prob = c(0.3, 0.45, 0.25)),
    pattern = patterns,
    stringsAsFactors = FALSE
  )
  ratings <- .simulate_ratings(panel, items, n_rounds, scenario = "rand")
  comments <- .simulate_comments(panel, items, ratings, ram = TRUE)
  meta <- list(
    title = "\u7a33\u5b9a\u578b\u51a0\u5fc3\u75c5\u8840\u8fd0\u91cd\u5efa\u9002\u5b9c\u6027\u8bc4\u4ef7\uff08\u6a21\u62df RAND/UCLA \u9762\u677f\uff09",
    objective = "\u5224\u65ad\u7279\u5b9a\u4e34\u5e8a\u6307\u5f81\u4e0b\u8840\u8fd0\u91cd\u5efa\u662f\u5426\u9002\u5b9c\uff0c\u800c\u4e0d\u662f\u5f3a\u8feb\u5f62\u6210\u5355\u4e00\u5171\u8bc6\u3002",
    users = "\u5fc3\u8840\u7ba1\u4e13\u79d1\u4e0e\u5168\u79d1\u533b\u5e08",
    geography = "\u6559\u5b66\u793a\u4f8b\uff0c\u975e\u771f\u5b9e\u6307\u5357",
    evidence_note = "\u6a21\u62df\u8bc1\u636e\u7efc\u8ff0\u5df2\u5728\u7b2c 1 \u8f6e\u524d\u63d0\u4f9b\u7ed9\u9762\u677f\u3002",
    anonymity = "\u7b2c 1 \u8f6e\u72ec\u7acb\u533f\u540d\u8bc4\u5206\uff1b\u7b2c 2 \u8f6e\u4f1a\u95f4\u8ba8\u8bba\u540e\u518d\u6b21\u72ec\u7acb\u8bc4\u5206\u3002",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\u3002"
  )
  delphi_study(protocol, panel, items, ratings, comments, meta = meta)
}

#' 模拟 COMET 核心结局集 Delphi 数据
#'
#' 生成多利益相关者（医生、患者、研究者）对候选结局的 1–9 重要性评分，
#' 并有意制造“总体过线但患者组未过线”的条目，用于教学分层分析。
#'
#' @param n_clinicians,n_patients,n_researchers 各组人数。
#' @param n_items 结局数，默认 25。
#' @param n_rounds 轮次数，默认 2。
#' @param seed 随机种子。
#'
#' @return [delphi_study()] 对象，场景为 `comet_cos`。
#'
#' @examples
#' cos <- simulate_cos(n_clinicians = 16, n_patients = 10, n_researchers = 8,
#'                     n_items = 10, seed = 3)
#' table(cos$panel$stakeholder)
#'
#' @export
simulate_cos <- function(n_clinicians = 40, n_patients = 25, n_researchers = 15,
                         n_items = 25, n_rounds = 2, seed = 2026) {
  set.seed(seed)
  protocol <- preset_protocol("comet_cos")
  n_c <- as.integer(n_clinicians)
  n_p <- as.integer(n_patients)
  n_r <- as.integer(n_researchers)
  n_experts <- n_c + n_p + n_r
  panel <- data.frame(
    expert_id = sprintf("S%03d", seq_len(n_experts)),
    stakeholder = c(rep("\u533b\u751f", n_c), rep("\u60a3\u8005", n_p), rep("\u7814\u7a76\u8005", n_r)),
    specialty = c(
      sample(c("\u80be\u810f\u5185\u79d1", "\u98ce\u6e7f\u514d\u75ab", "\u5168\u79d1"), n_c, TRUE),
      rep("\u60a3\u8005\u4ee3\u8868", n_p),
      sample(c("\u4e34\u5e8a\u8bd5\u9a8c", "\u7ed3\u5c40\u6d4b\u91cf", "\u7edf\u8ba1"), n_r, TRUE)
    ),
    region = sample(c("\u4e2d\u56fd", "\u6b27\u6d32", "\u5317\u7f8e", "\u4e9a\u592a\u5176\u4ed6"), n_experts, TRUE),
    title = NA_character_,
    years = c(sample(5:30, n_c, TRUE), sample(1:15, n_p, TRUE), sample(5:25, n_r, TRUE)),
    invited = TRUE,
    stringsAsFactors = FALSE
  )
  patterns <- .assign_cos_patterns(n_items)
  items <- data.frame(
    item_id = sprintf("O%02d", seq_len(n_items)),
    item_text = .cos_item_bank(n_items),
    domain = "\u7ed3\u5c40",
    outcome_domain = sample(
      c("\u75c5\u6b7b/\u751f\u5b58", "\u75be\u75c5\u6d3b\u52a8", "\u75c7\u72b6/\u751f\u547d\u8d28\u91cf", "\u5b9e\u9a8c\u5ba4", "\u8d44\u6e90\u5229\u7528", "\u4e0d\u826f\u4e8b\u4ef6"),
      n_items, TRUE
    ),
    pattern = patterns,
    stringsAsFactors = FALSE
  )
  ratings <- .simulate_ratings(panel, items, n_rounds, scenario = "cos")
  comments <- .simulate_comments(panel, items, ratings, cos = TRUE)
  meta <- list(
    title = "IgA \u80be\u75c5\u4e34\u5e8a\u8bd5\u9a8c\u6838\u5fc3\u7ed3\u5c40\u96c6\uff08\u6a21\u62df COMET Delphi\uff09",
    objective = "\u786e\u5b9a IgA \u80be\u75c5\u5e72\u9884\u8bd5\u9a8c\u5e94\u81f3\u5c11\u6d4b\u91cf\u7684\u6838\u5fc3\u7ed3\u5c40\u3002",
    users = "\u4e34\u5e8a\u8bd5\u9a8c\u7533\u529e\u8005\u3001\u7814\u7a76\u8005\u4e0e\u60a3\u8005\u7ec4\u7ec7",
    geography = "\u56fd\u9645\u591a\u5229\u76ca\u76f8\u5173\u8005\uff08\u6a21\u62df\uff09",
    evidence_note = "\u5019\u9009\u7ed3\u5c40\u6765\u81ea\u7cfb\u7edf\u8bc4\u4ef7\u4e0e\u60a3\u8005\u8bbf\u8c08\u540e\u7684\u6761\u76ee\u6c60\uff08\u6a21\u62df\uff09\u3002",
    anonymity = "\u5728\u7ebf\u533f\u540d\u8bc4\u5206\uff1b\u53cd\u9988\u542b\u672c\u4eba\u8bc4\u5206\u3001\u603b\u4f53\u53ca\u5229\u76ca\u76f8\u5173\u8005\u7ec4\u6458\u8981\u3002",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\u3002"
  )
  delphi_study(protocol, panel, items, ratings, comments, meta = meta)
}

# ---- pattern helpers ----------------------------------------------------

.assign_patterns <- function(n) {
  base <- c(
    rep("converge_high", max(1L, floor(n * 0.40))),
    rep("basic", max(1L, floor(n * 0.15))),
    rep("no_consensus", max(1L, floor(n * 0.15))),
    rep("polar", max(1L, floor(n * 0.10))),
    rep("reject", max(1L, floor(n * 0.05))),
    rep("improve", max(1L, floor(n * 0.15)))
  )
  if (length(base) < n) base <- c(base, rep("converge_high", n - length(base)))
  base[seq_len(n)]
}

.assign_ram_patterns <- function(n) {
  base <- c(
    rep("appropriate", max(1L, floor(n * 0.35))),
    rep("inappropriate", max(1L, floor(n * 0.20))),
    rep("uncertain_mid", max(1L, floor(n * 0.20))),
    rep("disagreement", max(1L, floor(n * 0.15))),
    rep("shift_agree", max(1L, floor(n * 0.10)))
  )
  if (length(base) < n) base <- c(base, rep("appropriate", n - length(base)))
  base[seq_len(n)]
}

.assign_cos_patterns <- function(n) {
  base <- c(
    rep("core_in", max(1L, floor(n * 0.28))),
    rep("core_out", max(1L, floor(n * 0.16))),
    rep("no_consensus", max(1L, floor(n * 0.20))),
    rep("patient_dissent", max(1L, floor(n * 0.20))),
    rep("improve", max(1L, floor(n * 0.16)))
  )
  if (length(base) < n) base <- c(base, rep("core_in", n - length(base)))
  base[seq_len(n)]
}

.draw_from <- function(n, probs) {
  sample.int(9, n, replace = TRUE, prob = probs)
}

.pattern_probs <- function(pattern, round, stakeholder = NULL, scenario = "clinical") {
  if (scenario == "rand") return(.ram_probs(pattern, round))
  if (scenario == "cos") return(.cos_probs(pattern, round, stakeholder))
  switch(
    pattern,
    converge_high = if (round == 1) c(0, 0, 0.02, 0.03, 0.05, 0.08, 0.22, 0.30, 0.30)
    else c(0, 0, 0, 0.01, 0.02, 0.04, 0.18, 0.35, 0.40),
    basic = if (round == 1) c(0.02, 0.03, 0.05, 0.08, 0.10, 0.12, 0.25, 0.20, 0.15)
    else c(0.01, 0.02, 0.04, 0.06, 0.08, 0.10, 0.28, 0.24, 0.17),
    no_consensus = c(0.08, 0.10, 0.12, 0.14, 0.16, 0.14, 0.12, 0.08, 0.06),
    polar = c(0.22, 0.14, 0.08, 0.04, 0.04, 0.04, 0.08, 0.14, 0.22),
    reject = if (round == 1) c(0.28, 0.22, 0.18, 0.12, 0.08, 0.06, 0.04, 0.01, 0.01)
    else c(0.32, 0.24, 0.18, 0.10, 0.08, 0.04, 0.03, 0.01, 0),
    improve = if (round == 1) c(0.04, 0.06, 0.08, 0.12, 0.14, 0.16, 0.18, 0.14, 0.08)
    else c(0.01, 0.02, 0.03, 0.05, 0.06, 0.08, 0.25, 0.28, 0.22),
    c(0.05, 0.05, 0.10, 0.15, 0.20, 0.15, 0.15, 0.10, 0.05)
  )
}

.ram_probs <- function(pattern, round) {
  switch(
    pattern,
    appropriate = if (round == 1) c(0, 0, 0.02, 0.03, 0.05, 0.08, 0.22, 0.30, 0.30)
    else c(0, 0, 0, 0.01, 0.02, 0.05, 0.20, 0.32, 0.40),
    inappropriate = if (round == 1) c(0.30, 0.28, 0.20, 0.10, 0.06, 0.04, 0.02, 0, 0)
    else c(0.36, 0.30, 0.20, 0.08, 0.04, 0.02, 0, 0, 0),
    uncertain_mid = c(0.04, 0.08, 0.12, 0.18, 0.22, 0.16, 0.12, 0.06, 0.02),
    disagreement = c(0.22, 0.16, 0.10, 0.04, 0.04, 0.04, 0.10, 0.16, 0.14),
    shift_agree = if (round == 1) c(0.10, 0.10, 0.10, 0.12, 0.12, 0.12, 0.12, 0.12, 0.10)
    else c(0.02, 0.03, 0.05, 0.08, 0.10, 0.12, 0.20, 0.18, 0.12),
    c(rep(1 / 9, 9))
  )
}

.cos_probs <- function(pattern, round, stakeholder) {
  if (identical(pattern, "patient_dissent")) {
    if (identical(stakeholder, "\u60a3\u8005")) {
      return(c(0.20, 0.18, 0.16, 0.14, 0.12, 0.08, 0.06, 0.04, 0.02))
    }
    return(if (round == 1) c(0, 0.01, 0.02, 0.04, 0.06, 0.08, 0.24, 0.28, 0.27)
           else c(0, 0, 0.01, 0.02, 0.04, 0.06, 0.22, 0.32, 0.33))
  }
  switch(
    pattern,
    core_in = if (round == 1) c(0, 0, 0.01, 0.02, 0.04, 0.06, 0.20, 0.32, 0.35)
    else c(0, 0, 0, 0.01, 0.02, 0.04, 0.18, 0.34, 0.41),
    core_out = if (round == 1) c(0.30, 0.24, 0.18, 0.10, 0.08, 0.05, 0.03, 0.01, 0.01)
    else c(0.34, 0.26, 0.18, 0.10, 0.06, 0.04, 0.02, 0, 0),
    no_consensus = c(0.08, 0.10, 0.12, 0.14, 0.16, 0.14, 0.12, 0.08, 0.06),
    improve = if (round == 1) c(0.04, 0.06, 0.08, 0.12, 0.12, 0.14, 0.18, 0.16, 0.10)
    else c(0.01, 0.02, 0.03, 0.04, 0.06, 0.08, 0.22, 0.30, 0.24),
    c(rep(1 / 9, 9))
  )
}

.simulate_ratings <- function(panel, items, n_rounds, scenario = "clinical") {
  rows <- list()
  n_experts <- nrow(panel)
  drop_round2 <- if (n_rounds >= 2) sample(panel$expert_id, size = max(1L, floor(n_experts * 0.08))) else character()
  drop_round3 <- if (n_rounds >= 3) {
    remain <- setdiff(panel$expert_id, drop_round2)
    sample(remain, size = max(1L, floor(length(remain) * 0.06)))
  } else character()

  for (rnd in seq_len(n_rounds)) {
    active <- panel$expert_id
    if (rnd >= 2) active <- setdiff(active, drop_round2)
    if (rnd >= 3) active <- setdiff(active, drop_round3)
    for (eid in active) {
      st <- panel$stakeholder[panel$expert_id == eid]
      for (j in seq_len(nrow(items))) {
        pat <- items$pattern[j]
        probs <- .pattern_probs(pat, rnd, st, scenario)
        score <- .draw_from(1L, probs)
        unable <- FALSE
        if (scenario == "clinical" && identical(st, "\u76d1\u7ba1\u4eba\u5458") &&
            identical(items$domain[j], "\u7edf\u8ba1") && runif(1) < 0.18) {
          score <- NA_real_
          unable <- TRUE
        }
        if (scenario == "cos" && identical(st, "\u60a3\u8005") &&
            identical(items$outcome_domain[j], "\u5b9e\u9a8c\u5ba4") && runif(1) < 0.12) {
          score <- NA_real_
          unable <- TRUE
        }
        rows[[length(rows) + 1L]] <- data.frame(
          expert_id = eid,
          item_id = items$item_id[j],
          round = rnd,
          score = score,
          dimension = if (scenario == "clinical") "\u91cd\u8981\u6027" else "overall",
          unable = unable,
          stakeholder = st,
          stringsAsFactors = FALSE
        )
        if (scenario == "clinical" && rnd == 1L && runif(1) < 0.85) {
          feas_probs <- probs
          feas_shift <- pmax(probs * c(1.1, 1.1, 1.05, 1, 1, 0.95, 0.9, 0.85, 0.8), 0)
          feas_shift <- feas_shift / sum(feas_shift)
          rows[[length(rows) + 1L]] <- data.frame(
            expert_id = eid,
            item_id = items$item_id[j],
            round = rnd,
            score = .draw_from(1L, feas_shift),
            dimension = "\u53ef\u884c\u6027",
            unable = FALSE,
            stakeholder = st,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  delphi_ratings(do.call(rbind, rows))
}

.simulate_comments <- function(panel, items, ratings, ram = FALSE, cos = FALSE) {
  set_items <- items$item_id[items$pattern %in% c(
    "no_consensus", "polar", "improve", "disagreement", "patient_dissent",
    "uncertain_mid", "basic"
  )]
  if (length(set_items) == 0L) set_items <- items$item_id[seq_len(min(4L, nrow(items)))]
  r1 <- ratings[ratings$round == 1 & ratings$dimension %in% c("\u91cd\u8981\u6027", "overall"), ]
  pick <- r1[r1$item_id %in% set_items, ]
  if (nrow(pick) == 0L) return(NULL)
  n_c <- min(nrow(pick), max(8L, floor(nrow(pick) * 0.12)))
  pick <- pick[sample.int(nrow(pick), n_c), ]
  themes <- if (ram) {
    c("\u83b7\u76ca\u5927\u4e8e\u4f24\u5bb3", "\u4f24\u5bb3\u5927\u4e8e\u83b7\u76ca", "\u8bc1\u636e\u4e0d\u8db3", "\u9700\u9650\u5b9a\u4eba\u7fa4", "\u8868\u8ff0\u9700\u6f84\u6e05")
  } else if (cos) {
    c("\u5bf9\u60a3\u8005\u81f3\u5173\u91cd\u8981", "\u6d4b\u91cf\u8d1f\u62c5\u9ad8", "\u4e0d\u662f\u6838\u5fc3", "\u5e94\u5206\u5c42\u62a5\u544a", "\u5b9a\u4e49\u4e0d\u6e05")
  } else {
    c("\u652f\u6301", "\u6761\u4ef6\u6027\u652f\u6301", "\u8868\u8ff0\u95ee\u9898", "\u53cd\u5bf9", "\u65b0\u589e\u5185\u5bb9")
  }
  texts <- if (ram) {
    c("\u8be5\u6307\u5f81\u4e0b\u83b7\u76ca\u660e\u786e\u3002", "\u5e76\u53d1\u75c7\u98ce\u9669\u8fc7\u9ad8\u3002", "\u73b0\u6709\u968f\u673a\u8bc1\u636e\u5f88\u5c11\u3002",
      "\u4ec5\u5bf9\u9ad8\u5371\u4e9a\u7ec4\u9002\u5b9c\u3002", "\u6307\u5f81\u5b9a\u4e49\u4e0e\u771f\u5b9e\u4e16\u754c\u4e0d\u5b8c\u5168\u5bf9\u5e94\u3002")
  } else if (cos) {
    c("\u8fd9\u662f\u60a3\u8005\u65e5\u5e38\u6700\u5173\u5fc3\u7684\u7ed3\u5c40\u3002", "\u9891\u7e41\u62bd\u8840\u96be\u4ee5\u575a\u6301\u3002", "\u4e0d\u5e94\u8fdb\u5165\u6838\u5fc3\u96c6\u3002",
      "\u533b\u751f\u548c\u60a3\u8005\u770b\u6cd5\u660e\u663e\u4e0d\u540c\u3002", "\u64cd\u4f5c\u5b9a\u4e49\u9700\u8981\u7edf\u4e00\u3002")
  } else {
    c("\u539f\u6761\u76ee\u5408\u7406\u3002", "\u4ec5\u9002\u7528\u4e8e\u9ad8\u98ce\u9669\u5e72\u9884\u6027\u8bd5\u9a8c\u3002", "\u5efa\u8bae\u5c06\u201c\u5e94\u201d\u6539\u4e3a\u201c\u5b9c\u201d\u3002",
      "\u76ee\u524d\u8bc1\u636e\u4e0d\u8db3\uff0c\u4e0d\u5b9c\u4f5c\u4e3a\u786c\u6027\u8981\u6c42\u3002", "\u5efa\u8bae\u589e\u52a0\u98ce\u9669\u5206\u5c42\u3002")
  }
  idx <- sample.int(length(themes), nrow(pick), TRUE)
  delphi_comments(data.frame(
    expert_id = pick$expert_id,
    item_id = pick$item_id,
    round = 1L,
    comment = texts[idx],
    theme = themes[idx],
    stringsAsFactors = FALSE
  ))
}

.simulate_authority_raw <- function(n) {
  fam_levels <- c(
    "\u5f88\u719f\u6089", "\u8f83\u719f\u6089", "\u4e00\u822c",
    "\u4e0d\u592a\u719f\u6089", "\u4e0d\u719f\u6089"
  )
  infl <- c("\u5927", "\u4e2d", "\u5c0f")
  data.frame(
    familiarity = sample(fam_levels, n, TRUE, prob = c(0.28, 0.38, 0.22, 0.09, 0.03)),
    practice = sample(infl, n, TRUE, prob = c(0.55, 0.35, 0.10)),
    theory = sample(infl, n, TRUE, prob = c(0.45, 0.40, 0.15)),
    literature = sample(infl, n, TRUE, prob = c(0.40, 0.40, 0.20)),
    intuition = sample(infl, n, TRUE, prob = c(0.20, 0.40, 0.40)),
    stringsAsFactors = FALSE
  )
}

.clinical_item_bank <- function(n) {
  bank <- c(
    "\u968f\u673a\u5bf9\u7167\u8bd5\u9a8c\u5e94\u5728\u65b9\u6848\u4e2d\u9884\u5148\u89c4\u5b9a\u8865\u6551\u6cbb\u7597\u542f\u52a8\u6807\u51c6\u3002",
    "\u4e3b\u8981\u7597\u6548\u6307\u6807\u5e94\u9884\u5148\u660e\u786e\u7f3a\u5931\u6570\u636e\u5904\u7406\u7b56\u7565\u3002",
    "\u65b9\u6848\u5e94\u5b9a\u4e49\u4e34\u5e8a\u8bd5\u9a8c\u91cd\u5927\u65b9\u6848\u504f\u79bb\u3002",
    "\u77e5\u60c5\u540c\u610f\u8fc7\u7a0b\u5e94\u8bb0\u5f55\u53d7\u8bd5\u8005\u7406\u89e3\u786e\u8ba4\u3002",
    "\u5b89\u5168\u6027\u6570\u636e\u5e94\u91c7\u7528\u9884\u5148\u5b9a\u4e49\u7684\u7f16\u7801\u8bcd\u5178\u3002",
    "\u4e2d\u671f\u5206\u6790\u7684\u7edf\u8ba1\u5b66\u8fb9\u754c\u5e94\u5728\u65b9\u6848\u4e2d\u9884\u5148\u89c4\u5b9a\u3002",
    "\u6570\u636e\u76d1\u67e5\u59d4\u5458\u4f1a\u7684\u72ec\u7acb\u6027\u5e94\u4e88\u4ee5\u4fdd\u8bc1\u3002",
    "\u968f\u673a\u5316\u9690\u85cf\u7684\u5b9e\u65bd\u5e94\u4e0e\u4ea7\u751f\u968f\u673a\u5e8f\u5217\u7684\u4eba\u5458\u5206\u79bb\u3002",
    "\u7535\u5b50\u6570\u636e\u91c7\u96c6\u7cfb\u7edf\u5e94\u5177\u5907\u5ba1\u8ba1\u8ffd\u8e2a\u3002",
    "\u5b9e\u9a8c\u5ba4\u68c0\u6d4b\u5e94\u4f7f\u7528\u4e2d\u5fc3\u5b9e\u9a8c\u5ba4\u6216\u7ecf\u8fc7\u6bd4\u5bf9\u7684\u672c\u5730\u5b9e\u9a8c\u5ba4\u3002",
    "\u65b9\u6848\u504f\u79bb\u5e94\u5728\u9884\u5b9a\u65f6\u9650\u5185\u62a5\u544a\u3002",
    "\u53d7\u8bd5\u8005\u8131\u843d\u539f\u56e0\u5e94\u9010\u4f8b\u8bb0\u5f55\u5e76\u53ef\u6c47\u603b\u3002",
    "\u6b21\u8981\u7ec8\u70b9\u7684\u591a\u91cd\u6027\u6821\u6b63\u7b56\u7565\u5e94\u9884\u5148\u8bf4\u660e\u3002",
    "\u4e9a\u7ec4\u5206\u6790\u5e94\u533a\u5206\u9a8c\u8bc1\u6027\u4e0e\u63a2\u7d22\u6027\u3002",
    "\u5916\u90e8\u5bf9\u7167\u7684\u4f7f\u7528\u5e94\u8bba\u8bc1\u53ef\u6bd4\u6027\u3002",
    "\u771f\u5b9e\u4e16\u754c\u6570\u636e\u7528\u4e8e\u786e\u8bc1\u6027\u63a8\u65ad\u65f6\u5e94\u9884\u5148\u5b9a\u4e49\u504f\u501a\u63a7\u5236\u3002",
    "\u6240\u6709\u4e34\u5e8a\u8bd5\u9a8c\u5747\u5fc5\u987b\u8bbe\u7f6e\u72ec\u7acb\u7ec8\u70b9\u88c1\u51b3\u59d4\u5458\u4f1a\u3002",
    "\u751f\u7269\u6807\u5fd7\u7269\u9634\u6027\u60a3\u8005\u4e00\u5f8b\u4e0d\u5f97\u5165\u7ec4\u9776\u5411\u6cbb\u7597\u8bd5\u9a8c\u3002",
    "\u8d28\u91cf\u6e90\u4e8e\u8bbe\u8ba1\u5e94\u8986\u76d6\u7533\u529e\u8005\u8d28\u91cf\u7ba1\u7406\u4f53\u7cfb\u3002",
    "\u98ce\u9669\u9002\u5e94\u6027\u76d1\u67e5\u8ba1\u5212\u5e94\u57fa\u4e8e\u5173\u952e\u6570\u636e\u4e0e\u5173\u952e\u6d41\u7a0b\u3002",
    "\u4e0d\u826f\u4e8b\u4ef6\u4e0e\u65b9\u6848\u6cbb\u7597\u7684\u56e0\u679c\u5173\u7cfb\u5224\u65ad\u6807\u51c6\u5e94\u7edf\u4e00\u3002",
    "\u65b9\u6848\u5e94\u8bf4\u660e\u5982\u4f55\u5904\u7406\u598a\u5a20\u66b4\u9732\u3002",
    "\u7edf\u8ba1\u5b9e\u8df5\u8ba1\u5212\u5e94\u4e0e\u65b9\u6848\u540c\u65f6\u5b9a\u7a3f\u3002",
    "\u53d7\u8bd5\u8005\u8865\u507f\u4e0d\u5f97\u6784\u6210\u4e0d\u5f53\u5f15\u8bf1\u3002"
  )
  if (n <= length(bank)) bank[seq_len(n)] else c(bank, paste0("\u5019\u9009\u6761\u76ee ", seq_len(n - length(bank))))
}

.ram_item_bank <- function(n) {
  bank <- c(
    "\u5de6\u4e3b\u5e72\u75c5\u53d8\u3001\u65e0\u624b\u672f\u7981\u5fcc\u3001 SYNTAX \u9ad8\u5206\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u5355\u652f\u5c40\u9650\u6027\u75c5\u53d8\u3001\u75c7\u72b6\u8f7b\u5fae\u3001\u8d1f\u8377\u8bd5\u9a8c\u9634\u6027\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u4e09\u652f\u75c5\u53d8\u5408\u5e76\u7cd6\u5c3f\u75c5\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u6025\u6027 ST \u62ac\u9ad8\u5fc3\u808c\u6897\u6b7b 12 \u5c0f\u65f6\u5185\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u6162\u6027\u5b8c\u5168\u95ed\u585e\u3001\u5b58\u6d3b\u5fc3\u808c\u4e0d\u660e\u786e\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u7a33\u5b9a\u578b\u5fc3\u7ede\u75db\u3001\u6700\u4f73\u836f\u7269\u6cbb\u7597\u540e\u4ecd CCS III\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u65e0\u75c7\u72b6\u3001\u4f4e\u5371\u8d1f\u8377\u8bd5\u9a8c\u7ed3\u679c\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u9ad8\u51fa\u8840\u98ce\u9669\u4e14\u75c5\u53d8\u590d\u6742\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u5c04\u8840\u5206\u6570\u964d\u4f4e\u5408\u5e76\u5de6\u524d\u964d\u652f\u8fd1\u6bb5\u75c5\u53d8\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u9ad8\u9f84\u8870\u5f31\u3001\u9884\u671f\u5bff\u547d\u6709\u9650\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u51a0\u72b6\u52a8\u8109\u75c9\u631b\u4e3a\u4e3b\u3001\u65e0\u56fa\u5b9a\u72ed\u7a84\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u836f\u7269\u96be\u6cbb\u6027\u5fc3\u7ede\u75db\u3001\u89e3\u5256\u9002\u5408 PCI\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u5fc3\u810f\u5916\u79d1\u672f\u524d\u7684\u4f34\u968f\u51a0\u8109\u75c5\u53d8\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u80be\u529f\u80fd\u4e25\u91cd\u4e0d\u5168\u3001\u5bf9\u6bd4\u5242\u98ce\u9669\u9ad8\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u65e0\u4fdd\u62a4\u5de6\u4e3b\u5e72\u3001\u5916\u79d1\u9ad8\u98ce\u9669\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u4e34\u754c\u72ed\u7a84\u3001F<0.80\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u4e34\u754c\u72ed\u7a84\u3001F>0.90\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u591a\u652f\u75c5\u53d8\u3001\u5b8c\u5168\u8840\u8fd0\u91cd\u5efa\u4e0d\u53ef\u884c\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u5fc3\u810f\u9aa4\u505c\u590d\u82cf\u540e\u3001\u660f\u8ff7\u72b6\u6001\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u7a33\u5b9a\u6027\u75c5\u53d8\u3001\u8fd1\u671f\u5927\u624b\u672f\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u79fb\u690d\u8840\u7ba1\u9000\u884c\u6027\u53d8\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u518d\u72ed\u7a84\u3001\u836f\u7269\u7403\u56ca\u5931\u8d25\u540e\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u6162\u6027\u51a0\u72b6\u52a8\u8109\u7efc\u5408\u5f81\u3001\u65e0\u7f3a\u8840\u8bc1\u636e\uff1a\u8840\u8fd0\u91cd\u5efa",
    "\u5de6\u524d\u964d\u652f\u8fd1\u6bb5 70%\u201390% \u72ed\u7a84\u3001\u4e2d\u7b49\u7f3a\u8840\uff1a\u8840\u8fd0\u91cd\u5efa"
  )
  if (n <= length(bank)) bank[seq_len(n)] else paste0("\u6307\u5f81 ", seq_len(n), "\uff1a\u8840\u8fd0\u91cd\u5efa")
}

.cos_item_bank <- function(n) {
  bank <- c(
    "\u5168\u56e0\u6b7b\u4ea1",
    "\u80be\u810f\u590d\u5408\u7ec8\u70b9\uff08\u8840\u808c\u9150\u500d\u589e/\u8fdb\u5165\u80be\u810f\u66ff\u4ee3/\u80be\u6e90\u6027\u6b7b\u4ea1\uff09",
    "\u4f30\u8ba1\u80be\u5c0f\u7403\u6ee4\u8fc7\u7387\u659c\u7387",
    "\u86cb\u767d\u5c3f\u53d8\u5316",
    "\u7ec8\u672b\u671f\u80be\u810f\u75c5",
    "\u75b2\u52b3",
    "\u5065\u5eb7\u76f8\u5173\u751f\u547d\u8d28\u91cf",
    "\u6c34\u80bf",
    "\u8840\u538b\u63a7\u5236",
    "\u611f\u67d3\u76f8\u5173\u4f4f\u9662",
    "\u7cd6\u76ae\u8d28\u6fc0\u7d20\u7d2f\u79ef\u66b4\u9732",
    "\u60a3\u8005\u603b\u4f53\u5370\u8c61\u53d8\u5316",
    "\u5de5\u4f5c\u4e0e\u5b66\u4e1a\u53d7\u5f71\u54cd\u5929\u6570",
    "\u8840\u5c3f",
    "\u8840\u6e05\u767d\u86cb\u767d",
    "\u8865\u4f53 C3",
    "Oxford \u75c5\u7406\u8bc4\u5206\u53d8\u5316",
    "\u4e0d\u826f\u4e8b\u4ef6\u5bfc\u81f4\u505c\u836f",
    "\u4e25\u91cd\u611f\u67d3",
    "\u6076\u6027\u80bf\u7624",
    "\u533b\u7597\u8d39\u7528",
    "\u968f\u8bbf\u4f9d\u4ece\u6027",
    "\u75be\u75c5\u590d\u53d1",
    "\u598a\u5a20\u7ed3\u5c40",
    "\u75ab\u82d7\u53cd\u5e94"
  )
  if (n <= length(bank)) bank[seq_len(n)] else paste0("\u5019\u9009\u7ed3\u5c40 ", seq_len(n))
}
