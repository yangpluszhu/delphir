#' 慢性心衰核心指标集：第一轮新增条目教学数据
#'
#' 按「中医药治疗成人慢性心力衰竭临床研究的核心疗效指标集」这一案例生成
#' 可重复的三轮数据，不是随机凑百分比。30 名专家，初始 26 项。
#' 开放建议归并后按独立提出人数审查：3 项进入下一轮，其余有排除或转阶段理由。
#' 第 2 轮三个新条目是第一次评分；第 3 轮才是它们的第二次评分。
#'
#' 文中「18 条自由文本」与归并后的「独立提出人数」不是同一个数。
#' 本数据按独立提出人数构造，并让「睡眠质量」由同一专家写两次，以演示去重。
#'
#' @param up_to_round 保留到第几轮。1 只含初始 26 项及第 1 轮评分；
#'   2 起条目池为 29 项。建议与概念编码在各轮都附在对象上。
#'
#' @return `delphi_study`。场景为 `comet_cos`，共识规则为 70/15。
#'
#' @examples
#' s1 <- simulate_chf_new_items(up_to_round = 1)
#' screen <- screen_new_items(s1$suggestions, s1$concept_review, s1$protocol, s1$items)
#' screen$n_admitted
#'
#' @export
simulate_chf_new_items <- function(up_to_round = 3L) {
  up_to_round <- as.integer(up_to_round)[1]
  .stop_if(is.na(up_to_round) || up_to_round < 1L || up_to_round > 3L,
           "up_to_round \u53ea\u80fd\u662f 1\u30012 \u6216 3\u3002")
  protocol <- preset_protocol("comet_cos")
  panel <- .chf_panel()
  items <- .chf_items(include_new = up_to_round >= 2L)
  suggestions <- .chf_suggestions()
  review <- .chf_concept_review()
  ratings <- .chf_ratings(up_to_round)
  comments <- .chf_comments(up_to_round)
  meta <- list(
    title = "\u4e2d\u533b\u836f\u6cbb\u7597\u6162\u6027\u5fc3\u529b\u8870\u7aed\u4e34\u5e8a\u7814\u7a76\u6838\u5fc3\u7597\u6548\u6307\u6807\u96c6",
    objective = "\u5efa\u7acb\u9002\u7528\u4e8e\u4e2d\u533b\u836f\u6cbb\u7597\u6210\u4eba\u6162\u6027\u5fc3\u529b\u8870\u7aed\u4e34\u5e8a\u7814\u7a76\u7684\u6838\u5fc3\u7597\u6548\u8bc4\u4ef7\u6307\u6807\u96c6\u3002\u8bc4\u4ef7\u7684\u662f\uff1a\u54ea\u4e9b\u7ed3\u5c40\u91cd\u8981\u5230\u5e94\u5f53\u8fdb\u5165\u6838\u5fc3\u6307\u6807\u96c6\uff0c\u800c\u4e0d\u662f\u7b3c\u7edf\u5730\u95ee\u54ea\u4e9b\u6307\u6807\u91cd\u8981\u3002",
    users = "\u4e2d\u533b\u836f\u4e34\u5e8a\u7814\u7a76\u8005\u3001\u65b9\u6cd5\u5b66\u5bb6\u4e0e\u6807\u51c6\u5236\u5b9a\u8005",
    geography = "\u4e2d\u56fd\u5927\u9646",
    evidence_note = "\u521d\u59cb 26 \u9879\u6765\u81ea\u53e4\u7c4d\u3001\u73b0\u4ee3\u6587\u732e\u3001\u4e34\u5e8a\u8bd5\u9a8c\u3001\u6307\u5357\u300120 \u540d\u60a3\u8005\u8bbf\u8c08\u4e0e 15 \u540d\u533b\u52a1\u4eba\u5458\u8bbf\u8c08\u3002\u8303\u56f4\u4ec5\u4e3a\u4e34\u5e8a\u7597\u6548\uff0c\u4e0d\u542b\u536b\u751f\u7ecf\u6d4e\u5b66\u3002",
    anonymity = "\u8bc4\u5206\u533f\u540d\u6c47\u603b\uff0c\u53cd\u9988\u4e0d\u51fa\u73b0\u59d3\u540d",
    ethics = "\u6559\u5b66\u6a21\u62df\u6570\u636e\uff0c\u4e0d\u6d89\u53ca\u771f\u5b9e\u53d7\u8bd5\u8005",
    scope = "\u4e34\u5e8a\u7597\u6548\u6838\u5fc3\u6307\u6807\uff0c\u4e0d\u542b\u536b\u751f\u7ecf\u6d4e\u5b66\u8bc4\u4ef7"
  )
  delphi_study(
    protocol = protocol,
    panel = panel,
    items = items,
    ratings = ratings,
    comments = comments,
    suggestions = suggestions,
    concept_review = review,
    meta = meta,
    validate = FALSE
  )
}

.chf_panel <- function() {
  id <- sprintf("E%02d", 1:30)
  stakeholder <- c(
    rep("\u4e2d\u533b\u4e34\u5e8a", 18),
    rep("\u897f\u533b\u4e34\u5e8a", 6),
    rep("\u60a3\u8005\u4ee3\u8868", 3),
    rep("\u65b9\u6cd5\u5b66", 2),
    "\u62a4\u7406"
  )
  delphi_panel(data.frame(
    expert_id = id,
    stakeholder = stakeholder,
    specialty = stakeholder,
    years = 8L + (seq_len(30) %% 18L),
    Ca = round(0.75 + (seq_len(30) %% 5) * 0.03, 2),
    Cs = round(0.70 + (seq_len(30) %% 4) * 0.05, 2),
    invited = TRUE,
    stringsAsFactors = FALSE
  ))
}

.chf_items <- function(include_new = TRUE) {
  base <- data.frame(
    item_id = sprintf("CHF-%02d", 1:26),
    item_text = c(
      "\u5168\u56e0\u6b7b\u4ea1", "\u5fc3\u8840\u7ba1\u6b7b\u4ea1", "\u5fc3\u8870\u4f4f\u9662", "\u5fc3\u8870\u6025\u6027\u52a0\u91cd",
      "\u547c\u5438\u56f0\u96be", "\u75b2\u4e4f", "\u5fc3\u60b8", "\u6c34\u80bf", "\u80f8\u95f7",
      "\u4e2d\u533b\u8bc1\u5019\u603b\u79ef\u5206", "\u6c14\u865a\u75c7\u72b6", "\u8840\u7600\u75c7\u72b6", "\u9633\u865a\u75c7\u72b6",
      "NYHA\u5fc3\u529f\u80fd\u5206\u7ea7", "6\u5206\u949f\u6b65\u884c\u8ddd\u79bb",
      "\u5065\u5eb7\u76f8\u5173\u751f\u6d3b\u8d28\u91cf",
      "NT-proBNP", "\u5de6\u5ba4\u5c04\u8840\u5206\u6570\uff08LVEF\uff09",
      "\u5fc3\u8840\u7ba1\u4f4f\u9662", "\u5fc3\u8870\u76f8\u5173\u6025\u8bca\u5c31\u8bca", "\u4f53\u91cd\u589e\u52a0",
      "\u7aef\u5750\u547c\u5438", "\u591c\u95f4\u9635\u53d1\u6027\u547c\u5438\u56f0\u96be", "\u65e5\u5e38\u6d3b\u52a8\u53d7\u9650",
      "\u4e25\u91cd\u4e0d\u826f\u4e8b\u4ef6", "\u56e0\u5fc3\u8870\u7684\u8ba1\u5212\u5916\u95e8\u8bca\u5c31\u8bca"
    ),
    domain = c(
      rep("\u4e34\u5e8a\u4e8b\u4ef6", 4), rep("\u75c7\u72b6", 5), rep("\u4e2d\u533b\u7279\u8272", 4),
      rep("\u529f\u80fd", 2), "\u60a3\u8005\u62a5\u544a\u7ed3\u5c40", rep("\u5ba2\u89c2\u6307\u6807", 2),
      rep("\u4e34\u5e8a\u4e8b\u4ef6", 2), rep("\u75c7\u72b6", 3), "\u529f\u80fd", "\u5b89\u5168\u6027", "\u4e34\u5e8a\u4e8b\u4ef6"
    ),
    source_type = "\u524d\u671f\u8bc1\u636e\u7efc\u5408",
    introduced_round = 1L,
    origin = "initial",
    modification = "none",
    stringsAsFactors = FALSE
  )
  if (!include_new) return(delphi_items(base))
  extra <- data.frame(
    item_id = c("NEW-01", "NEW-02", "NEW-03"),
    item_text = c(
      "\u60a3\u8005\u603b\u4f53\u6539\u5584\u611f\u53d7",
      "\u7761\u7720\u8d28\u91cf",
      "\u6d3b\u52a8\u540e\u547c\u5438\u56f0\u96be\u6062\u590d\u60c5\u51b5"
    ),
    domain = c("\u60a3\u8005\u62a5\u544a\u7ed3\u5c40", "\u75c7\u72b6", "\u75c7\u72b6"),
    source_type = "\u7b2c 1 \u8f6e\u4e13\u5bb6\u65b0\u589e",
    source_note = c(
      "8 \u540d\u4e13\u5bb6\u72ec\u7acb\u63d0\u51fa\uff0c\u901a\u9053 A\u3002",
      "1 \u540d\u4e13\u5bb6\u63d0\u51fa\uff0c\u5e76\u6709\u60a3\u8005\u8bbf\u8c08\u652f\u6301\uff0c\u901a\u9053 B\u3002",
      "3 \u540d\u4e13\u5bb6\u72ec\u7acb\u63d0\u51fa\uff1b\u7f16\u7801\u4e0d\u4e00\u81f4\u540e\u7531\u7b2c\u4e09\u4eba\u88c1\u51b3\u4e3a\u72ec\u7acb\u6982\u5ff5\u3002"
    ),
    introduced_round = 2L,
    origin = "new",
    modification = "none",
    stringsAsFactors = FALSE
  )
  delphi_items(.rbind_fill(base, extra))
}

.chf_suggestions <- function() {
  spec <- rbind(
    data.frame(expert_id = sprintf("E%02d", 1:8), concept_id = "C-PGIC", raw_name = "\u60a3\u8005\u603b\u4f53\u6539\u5584\u611f\u53d7", stringsAsFactors = FALSE),
    data.frame(expert_id = c("E25", "E25"), concept_id = "C-SLEEP", raw_name = c("\u7761\u7720\u8d28\u91cf", "\u591c\u95f4\u7761\u7720\u53d7\u5f71\u54cd"), stringsAsFactors = FALSE),
    data.frame(expert_id = sprintf("E%02d", 9:11), concept_id = "C-RECOV", raw_name = "\u8fd0\u52a8\u540e\u547c\u5438\u56f0\u96be\u6062\u590d\u65f6\u95f4", stringsAsFactors = FALSE),
    data.frame(expert_id = sprintf("E%02d", 12:13), concept_id = "C-TONGUE", raw_name = "\u820c\u8272\u6539\u5584", stringsAsFactors = FALSE),
    data.frame(expert_id = sprintf("E%02d", 14:15), concept_id = "C-PULSE", raw_name = "\u8109\u8c61\u6539\u5584", stringsAsFactors = FALSE),
    data.frame(expert_id = sprintf("E%02d", 16:19), concept_id = "C-D30", raw_name = "30\u65e5\u518d\u4f4f\u9662", stringsAsFactors = FALSE),
    data.frame(expert_id = sprintf("E%02d", 20:21), concept_id = "C-COST", raw_name = "\u533b\u7597\u8d39\u7528", stringsAsFactors = FALSE),
    data.frame(expert_id = sprintf("E%02d", 22:24), concept_id = "C-MLHFQ", raw_name = "MLHFQ", stringsAsFactors = FALSE),
    data.frame(expert_id = "E26", concept_id = "C-WORD", raw_name = "\u628a\u8fd0\u52a8\u8010\u91cf\u6539\u79f0\u8fd0\u52a8\u80fd\u529b", stringsAsFactors = FALSE)
  )
  spec$meaning <- spec$raw_name
  spec$rationale <- "\u7b2c 1 \u8f6e\u5f00\u653e\u9898\u4e2d\u63d0\u51fa"
  spec$distinction <- "\u89c1\u6982\u5ff5\u5ba1\u67e5\u8868"
  spec$evidence_note <- ifelse(
    spec$concept_id == "C-SLEEP",
    "\u60a3\u8005\u8bbf\u8c08\u4e2d\u591a\u4eba\u63d0\u5230\u591c\u95f4\u618b\u9192\u4e0e\u4e0d\u80fd\u5e73\u5367",
    "\u4e34\u5e8a\u7ecf\u9a8c"
  )
  spec$round <- 1L
  spec$slot <- ave(seq_len(nrow(spec)), spec$expert_id, FUN = seq_along)
  delphi_suggestions(spec)
}

.chf_concept_review <- function() {
  data.frame(
    concept_id = c("C-PGIC", "C-SLEEP", "C-RECOV", "C-TONGUE", "C-PULSE", "C-D30", "C-COST", "C-MLHFQ", "C-WORD"),
    standardized_name = c(
      "\u60a3\u8005\u603b\u4f53\u6539\u5584\u611f\u53d7", "\u7761\u7720\u8d28\u91cf", "\u6d3b\u52a8\u540e\u547c\u5438\u56f0\u96be\u6062\u590d\u60c5\u51b5",
      "\u820c\u8272\u6539\u5584", "\u8109\u8c61\u6539\u5584", "30\u65e5\u518d\u4f4f\u9662", "\u533b\u7597\u8d39\u7528", "MLHFQ", "\u8fd0\u52a8\u80fd\u529b"
    ),
    construct = c(
      "new_item", "new_item", "new_item", "hierarchy", "hierarchy",
      "timepoint", "out_of_scope", "instrument", "modification"
    ),
    in_scope = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
    covered_by = c(NA, NA, NA, "CHF-10", "CHF-10", "CHF-03", NA, "CHF-16", "CHF-15"),
    mapped_item_id = c(NA, NA, NA, "CHF-10", "CHF-10", "CHF-03", NA, "CHF-16", "CHF-15"),
    parent_item_id = c(NA, NA, NA, "CHF-10", "CHF-10", "CHF-03", NA, "CHF-16", "CHF-15"),
    modification_type = c(NA, NA, NA, NA, NA, NA, NA, NA, "wording"),
    external_support = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    external_note = c(
      "",
      "\u524d\u671f 20 \u540d\u60a3\u8005\u8bbf\u8c08\u4e2d\u6709 7 \u4eba\u4e3b\u52a8\u63d0\u5230\u591c\u95f4\u618b\u9192\u3001\u4e0d\u80fd\u5e73\u5367\u3001\u7761\u7720\u4e25\u91cd\u53d7\u5f71\u54cd\u3002",
      "\u60a3\u8005\u8bbf\u8c08\u4e2d\u5e38\u63cf\u8ff0\u6d3b\u52a8\u540e\u591a\u4e45\u80fd\u591f\u7f13\u8fc7\u6765\uff0c\u4e0e\u9759\u6001\u547c\u5438\u56f0\u96be\u4e0d\u5b8c\u5168\u76f8\u540c\u3002",
      "\u8bc1\u5019\u8bc4\u4ef7\u5df2\u7ecf\u5305\u62ec\u820c\u8d28\u3001\u820c\u82d4\u3002",
      "\u8bc1\u5019\u8bc4\u4ef7\u5df2\u7ecf\u5305\u62ec\u8109\u8c61\u3002",
      "\u5c5e\u4e8e\u5fc3\u8870\u4f4f\u9662\u7684\u6d4b\u91cf\u65f6\u95f4\u7a97\u3002",
      "\u672c\u7814\u7a76\u53ea\u8bc4\u4ef7\u4e34\u5e8a\u7597\u6548\uff0c\u4e0d\u5305\u62ec\u536b\u751f\u7ecf\u6d4e\u5b66\u3002",
      "\u660e\u5c3c\u82cf\u8fbe\u5fc3\u8870\u751f\u6d3b\u8d28\u91cf\u91cf\u8868\u662f\u6d4b\u91cf\u5de5\u5177\uff0c\u5bf9\u5e94\u7684\u7ed3\u5c40\u6982\u5ff5\u4ecd\u662f\u5065\u5eb7\u76f8\u5173\u751f\u6d3b\u8d28\u91cf\u3002",
      "\u4e0e 6 \u5206\u949f\u6b65\u884c\u8ddd\u79bb\u6240\u6307\u7684\u8fd0\u52a8\u80fd\u529b\u662f\u540c\u4e00\u6784\u5ff5\uff0c\u53ea\u662f\u7528\u8bed\u66f4\u901a\u987a\u3002"
    ),
    coder_a = c("new_item", "new_item", "hierarchy", "hierarchy", "hierarchy", "timepoint", "out_of_scope", "instrument", "wording"),
    coder_b = c("new_item", "new_item", "new_item", "hierarchy", "hierarchy", "timepoint", "out_of_scope", "instrument", "wording"),
    adjudicator = c("", "", "\u7b2c\u4e09\u540d\u7814\u7a76\u8005", "", "", "", "", "", ""),
    adjudicated_construct = c("", "", "new_item", "", "", "", "", "", ""),
    proposed_item_id = c("NEW-01", "NEW-02", "NEW-03", NA, NA, NA, NA, NA, NA),
    proposed_text = c(
      "\u60a3\u8005\u603b\u4f53\u6539\u5584\u611f\u53d7", "\u7761\u7720\u8d28\u91cf", "\u6d3b\u52a8\u540e\u547c\u5438\u56f0\u96be\u6062\u590d\u60c5\u51b5",
      NA, NA, NA, NA, NA, NA
    ),
    domain = c("\u60a3\u8005\u62a5\u544a\u7ed3\u5c40", "\u75c7\u72b6", "\u75c7\u72b6", "\u4e2d\u533b\u7279\u8272", "\u4e2d\u533b\u7279\u8272", "\u4e34\u5e8a\u4e8b\u4ef6", "\u7ecf\u6d4e\u5b66", "\u6d4b\u91cf\u5de5\u5177", "\u529f\u80fd"),
    stringsAsFactors = FALSE
  )
}

.chf_scores <- function(n_high, n_low, n = 30L) {
  n_mid <- n - n_high - n_low
  .stop_if(n_mid < 0L, "\u9ad8\u533a\u4e0e\u4f4e\u533a\u4eba\u6570\u4e4b\u548c\u8d85\u8fc7 30\u3002")
  c(rep(8L, n_high), rep(2L, n_low), rep(5L, n_mid))
}

.chf_ratings <- function(up_to_round) {
  experts <- sprintf("E%02d", 1:30)
  spec <- data.frame(
    item_id = sprintf("CHF-%02d", 1:26),
    r1h = 22L, r1l = 2L,
    r2h = 24L, r2l = 1L,
    stringsAsFactors = FALSE
  )
  set_hl <- function(i, r1h, r1l, r2h, r2l) {
    spec$r1h[i] <<- r1h
    spec$r1l[i] <<- r1l
    spec$r2h[i] <<- r2h
    spec$r2l[i] <<- r2l
  }
  set_hl(9L, 18L, 4L, 22L, 2L)
  set_hl(11L, 16L, 6L, 17L, 5L)
  set_hl(21L, 3L, 22L, 2L, 23L)
  set_hl(23L, 15L, 7L, 16L, 6L)
  rows <- list()
  add <- function(item_id, round, n_high, n_low) {
    rows[[length(rows) + 1L]] <<- data.frame(
      expert_id = experts,
      item_id = item_id,
      round = round,
      score = .chf_scores(n_high, n_low),
      dimension = "\u91cd\u8981\u6027",
      unable = FALSE,
      stringsAsFactors = FALSE
    )
  }
  for (i in seq_len(nrow(spec))) {
    add(spec$item_id[i], 1L, spec$r1h[i], spec$r1l[i])
    if (up_to_round >= 2L) add(spec$item_id[i], 2L, spec$r2h[i], spec$r2l[i])
  }
  if (up_to_round >= 2L) {
    add("NEW-01", 2L, 27L, 1L)
    add("NEW-02", 2L, 23L, 2L)
    add("NEW-03", 2L, 19L, 3L)
  }
  if (up_to_round >= 3L) {
    add("CHF-11", 3L, 18L, 4L)
    add("CHF-23", 3L, 17L, 5L)
    add("NEW-01", 3L, 28L, 0L)
    add("NEW-02", 3L, 25L, 2L)
    add("NEW-03", 3L, 20L, 4L)
  }
  delphi_ratings(do.call(rbind, rows))
}

.chf_comments <- function(up_to_round) {
  if (up_to_round < 2L) return(NULL)
  rows <- data.frame(
    expert_id = c("E01", "E02"),
    item_id = c("NEW-02", "NEW-02"),
    round = 2L,
    comment = c(
      "\u7761\u7720\u53d7\u591c\u95f4\u547c\u5438\u56f0\u96be\u5f71\u54cd\u660e\u663e\uff0c\u662f\u60a3\u8005\u53ef\u611f\u77e5\u7684\u91cd\u8981\u83b7\u76ca\u3002",
      "\u7761\u7720\u53d7\u7126\u8651\u3001\u73af\u5883\u548c\u5408\u5e76\u75be\u75c5\u5f71\u54cd\u8f83\u5927\uff0c\u7279\u5f02\u6027\u4e0d\u8db3\u3002"
    ),
    theme = c("\u652f\u6301", "\u53cd\u5bf9"),
    stringsAsFactors = FALSE
  )
  if (up_to_round >= 3L) {
    rows <- rbind(rows, data.frame(
      expert_id = "E03",
      item_id = "NEW-03",
      round = 3L,
      comment = "\u6d3b\u52a8\u540e\u6062\u590d\u4ecd\u4e0e\u547c\u5438\u56f0\u96be\u91cd\u53e0\uff0c\u5c1a\u96be\u5355\u72ec\u4f5c\u4e3a\u6838\u5fc3\u6307\u6807\u3002",
      theme = "\u4fdd\u7559\u610f\u89c1",
      stringsAsFactors = FALSE
    ))
  }
  delphi_comments(rows)
}
