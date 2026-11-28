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
demo <- simulate_standard_delphi(seed = 2026)
demo

## -----------------------------------------------------------------------------
protocol <- preset_protocol("standard_element")
protocol

## -----------------------------------------------------------------------------
panel <- demo$panel
compliance <- panel_compliance(demo)
compliance[, c("check", "value", "threshold", "ok", "message")]

## -----------------------------------------------------------------------------
profile <- expert_profile(panel)
profile$tables$sex
profile$age
profile$tables$education
profile$tables$title_level
profile$tables$department
profile$tables$region
profile$tables$familiarity
profile$years

## -----------------------------------------------------------------------------
initial <- demo$meta$initial_items
initial[, c("item_id", "item_text", "source_type")]

## -----------------------------------------------------------------------------
meta <- demo$meta
bp <- delphi_blueprint(protocol, panel, initial, meta = meta)
dir1 <- tempfile("open-")
dir.create(dir1)
files1 <- delphi_questionnaire(bp, round = 1, format = "both", file = dir1)
basename(files1)
md1 <- delphi_questionnaire(bp, round = 1)
grepl("1  2  3  4  5", md1, fixed = TRUE)

## -----------------------------------------------------------------------------
open_only <- bp
open_only$comments <- demo$comments[demo$comments$round == 1, ]
open_res <- analyze_open_round(open_only, round = 1)
open_res$by_theme
open_res$n_experts
utils::head(open_res$new_suggestions[, c("expert_id", "item_id", "theme", "comment")], 4)

## -----------------------------------------------------------------------------
added <- demo$items[demo$items$source_type == "开放式凝练", ]
bp2 <- update_delphi_study(bp, items = added, validate = FALSE)
bp2$items$item_id

## -----------------------------------------------------------------------------
md2 <- delphi_questionnaire(bp2, round = 2)
grepl("权威程度自评", md2)
grepl("重要程度", md2)
grepl("熟悉程度", md2)

## -----------------------------------------------------------------------------
xlsx2 <- tempfile(fileext = ".xlsx")
holder <- demo
holder$ratings <- demo$ratings[demo$ratings$round == 2, ]
holder$comments <- demo$comments[demo$comments$round == 2, ]
delphi_questionnaire(holder, round = 2, format = "xlsx", file = xlsx2, prefill = TRUE)
got2 <- read_survey_xlsx(xlsx2, study = bp2)
nrow(got2$ratings)
utils::head(got2$ratings[, c("expert_id", "item_id", "dimension", "score")])

## -----------------------------------------------------------------------------
study2 <- got2
study2$ratings <- study2$ratings[study2$ratings$round <= 2, ]
res2 <- analyze_delphi(study2)
imp2 <- res2$item_summary
imp2 <- imp2[imp2$round == 2 & imp2$dimension == "重要程度", ]
imp2[, c("item_id", "n_valid", "mean", "cv", "full_rate")]
res2$participation$by_round
round(attr(res2$authority, "overall")$mean_cr, 3)
coordination_decision(study2)$note

## -----------------------------------------------------------------------------
dir3 <- tempfile("r3-")
dir.create(dir3)
files3 <- next_round_questionnaire(res2, format = "both", file = dir3, expert_id = "E01")
basename(files3)
md3 <- delphi_questionnaire(res2, round = 3, expert_id = "E01")
grepl("均数", md3)
grepl("您的上轮评分", md3)

## -----------------------------------------------------------------------------
study3 <- demo
study3$ratings <- demo$ratings[demo$ratings$round <= 3, ]
study3$comments <- demo$comments[demo$comments$round <= 3, ]
res3 <- analyze_delphi(study3)
cd3 <- coordination_decision(study3)
cd3$by_round
cd3$recommend_stop
cd3$note

## -----------------------------------------------------------------------------
res <- analyze_delphi(demo)
cd <- coordination_decision(demo)
cd$by_round[, c("round", "w", "p_value", "mean_abs_delta", "criteria_met")]
cd$recommend_stop
cd$note

## -----------------------------------------------------------------------------
last <- res$last_classification
last[, c("item_id", "item_text", "n_valid", "mean", "cv", "full_rate", "class_zh")]

## -----------------------------------------------------------------------------
fam <- res$item_summary
fam <- fam[fam$round == max(fam$round) & fam$dimension == "熟悉程度", ]
fam[, c("item_id", "n_valid", "mean", "full_rate")]

## -----------------------------------------------------------------------------
app <- appendix_b_table(res)
data.frame(
  标准内容 = app$standard_text,
  确定方法 = app$method,
  调查人数 = app$n_respondents,
  均值 = app$mean,
  满分比 = sprintf("%.0f%%", app$full_rate),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
report_file <- tempfile("standard-report-", fileext = ".docx")
res_doc <- run_delphi(demo, report = TRUE, format = "docx", file = report_file)
out <- attr(res_doc, "report_file")
basename(out)
file.exists(out)
file.info(out)$size

