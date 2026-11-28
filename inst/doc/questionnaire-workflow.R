## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(delphir)
delphi_guides()

## -----------------------------------------------------------------------------
protocol <- preset_protocol("clinical_consensus")
protocol

## -----------------------------------------------------------------------------
qn1 <- system.file("questionnaires", "round1-complete.md", package = "delphir")
file.exists(qn1)
substr(paste(readLines(qn1, encoding = "UTF-8", warn = FALSE), collapse = "\n"), 1, 180)

## -----------------------------------------------------------------------------
mini_items <- utils::read.csv(
  system.file("extdata", "survey_items.csv", package = "delphir"),
  fileEncoding = "UTF-8", stringsAsFactors = FALSE
)
draft <- delphi_study(
  protocol,
  panel = data.frame(expert_id = "E01", stringsAsFactors = FALSE),
  items = mini_items,
  ratings = data.frame(
    expert_id = "E01", item_id = mini_items$item_id[1],
    round = 1, score = 8, stringsAsFactors = FALSE
  ),
  meta = list(
    title = "药物临床试验质量源于设计实施专家共识（教学）",
    objective = "就方案设计与实施中的关键质量要求形成专家共识。",
    users = "申办者、研究者、伦理与监管相关人员",
    geography = "中国大陆",
    anonymity = "评分匿名汇总；反馈含本人上轮评分与组统计。"
  ),
  validate = FALSE
)
q_auto <- delphi_questionnaire(draft, round = 1)
grepl("权威程度自评", q_auto)

## -----------------------------------------------------------------------------
authority_scales()$cs
authority_scales()$ca

## -----------------------------------------------------------------------------
raw_ok <- data.frame(
  expert_id = c("E1", "E2"),
  familiarity = c("较熟悉", "很熟悉"),
  practice = c("大", "中"),
  theory = c("中", "大"),
  literature = c("大", "中"),
  intuition = c("小", "小"),
  stringsAsFactors = FALSE
)
score_authority(raw_ok)[, c("expert_id", "Ca", "Cs", "Cr")]

## ----error=TRUE---------------------------------------------------------------
try({
score_authority(cbind(raw_ok[c(1, 1), ], item_id = c("C01", "C02")))
})

## -----------------------------------------------------------------------------
d <- tempfile("survey-kit-")
invisible(write_survey_templates(d, overwrite = TRUE))
list.files(d, recursive = TRUE)

## -----------------------------------------------------------------------------
ex <- system.file("extdata", package = "delphir")
readu <- function(f) {
  utils::read.csv(file.path(ex, f), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}
panel <- readu("survey_panel.csv")
auth_raw <- readu("survey_authority_raw.csv")
items <- readu("survey_items.csv")
ratings <- readu("survey_ratings.csv")
comments <- readu("survey_comments.csv")
scored <- score_authority(auth_raw)
panel$Ca <- scored$Ca[match(panel$expert_id, scored$expert_id)]
panel$Cs <- scored$Cs[match(panel$expert_id, scored$expert_id)]
knitr::kable(scored[, c("expert_id", "familiarity", "Ca", "Cs", "Cr", "cr_ok")],
             caption = "由问卷勾选赋分，而不是直接填写无法追溯的小数")

## -----------------------------------------------------------------------------
meta <- list(
  title = "药物临床试验质量源于设计实施专家共识（迷你教学数据）",
  objective = "就方案设计与实施中的关键质量要求形成专家共识。",
  users = "申办者、研究者、伦理委员会与监管相关人员",
  geography = "中国大陆",
  evidence_note = "条目来自 ICH E6(R3)、GCP 及方法学综述后的初始条目池（教学）。",
  pilot_tested = TRUE,
  anonymity = "评分匿名汇总；第 2 轮反馈含本人上轮评分。"
)
study_r1 <- delphi_study(
  protocol,
  panel = panel,
  items = items,
  ratings = ratings[ratings$round == 1, ],
  comments = comments,
  meta = meta
)
study_r1

## -----------------------------------------------------------------------------
part <- response_summary(study_r1)
part$by_round
auth <- expert_authority(study_r1$panel)
attr(auth, "overall")[c("mean_ca", "mean_cs", "mean_cr", "pct_ok", "threshold")]

## -----------------------------------------------------------------------------
s1 <- summarize_items(study_r1, rounds = 1, dimensions = "重要性")
c1 <- classify_consensus(s1, protocol)
c1[, c("item_id", "n_valid", "n_unable", "median_iqr", "p_high", "p_low", "class_zh", "action")]

## -----------------------------------------------------------------------------
res1 <- analyze_delphi(study_r1)
r1_html <- delphi_report(res1, file = tempfile(fileext = ".html"),
                         template = "clinical", stage = "round1")
file.exists(r1_html)

## -----------------------------------------------------------------------------
fb <- prepare_feedback(res1, round = 1, dimension = "重要性")
fb[fb$expert_id == "E01", c("item_id", "own_score", "median_iqr", "p_high", "class_zh")]

## -----------------------------------------------------------------------------
q2 <- delphi_questionnaire(res1, round = 2, expert_id = "E01", items = "unresolved")
grepl("不再", q2)
grepl("C03", q2)

## -----------------------------------------------------------------------------
study <- delphi_study(
  protocol,
  panel = panel,
  items = items,
  ratings = ratings,
  comments = comments,
  meta = meta
)
res <- analyze_delphi(study)
res

## -----------------------------------------------------------------------------
last <- res$last_classification
last <- last[last$dimension == "重要性", ]
last[, c("item_id", "n_valid", "median_iqr", "p_high", "p_low", "class_zh", "action")]

## -----------------------------------------------------------------------------
head(res$stability[, c("item_id", "median_r1", "median_r2", "p_high_r1",
                       "p_high_r2", "class_unchanged")])
res$stopping

## -----------------------------------------------------------------------------
plot_round_shift(res, metric = "p_high")

## -----------------------------------------------------------------------------
res$participation$retention
head(res$dropout)

## -----------------------------------------------------------------------------
html <- delphi_report(res, file = tempfile(fileext = ".html"),
                      template = "clinical", stage = "final")
file.exists(html)

## -----------------------------------------------------------------------------
head(accord_checklist(res)[, c("id", "section", "status")], 8)
dcat_checklist(res)[1:4, c("id", "core", "item", "status")]

## -----------------------------------------------------------------------------
sim <- simulate_delphi(n_experts = 16, n_items = 8, n_rounds = 2, seed = 2026)
head(sim$panel[, c("expert_id", "familiarity", "practice", "Ca", "Cs")])

