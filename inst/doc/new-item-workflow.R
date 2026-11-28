## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(delphir)
protocol <- preset_protocol("comet_cos")
protocol$new_items

## -----------------------------------------------------------------------------
round1 <- simulate_chf_new_items(up_to_round = 1)
nrow(round1$items)
table(round1$items$domain)
bp <- delphi_blueprint(protocol, round1$panel, round1$items, round1$meta)
dir1 <- tempfile("chf-r1-")
dir.create(dir1)
files1 <- delphi_questionnaire(bp, round = 1, format = "both", file = dir1)
basename(files1)

## -----------------------------------------------------------------------------
filled <- tempfile(fileext = ".xlsx")
delphi_questionnaire(round1, round = 1, format = "xlsx", file = filled, prefill = TRUE)
got <- read_survey_xlsx(filled)
c(nrow(got$ratings), nrow(got$suggestions))

## -----------------------------------------------------------------------------
screen <- screen_new_items(
  round1$suggestions, round1$concept_review, round1$protocol, round1$items
)
screen$concepts[, c("concept_id", "standardized_name", "n_independent", "channel", "decision_zh")]
screen$n_admitted
stopifnot(screen$n_admitted == 3L, screen$disposition_complete)

## -----------------------------------------------------------------------------
pooled <- update_delphi_study(round1, items = screen$admitted)
nrow(pooled$items)
stopifnot(nrow(pooled$items) == 29L)
audit1 <- audit_new_items(
  pooled$protocol, screen$concepts, suggestions = pooled$suggestions
)
audit1[, c("check", "status")]

## -----------------------------------------------------------------------------
wording_demo <- classify_modification(
  delphi_items(data.frame(
    item_id = c("W", "S"),
    item_text = c("运动耐量", "中医症状改善"),
    introduced_round = c(1L, 1L),
    stringsAsFactors = FALSE
  )),
  data.frame(
    item_id = c("W", "S"),
    modification = c("wording", "substantive"),
    new_text = c("运动能力", "气虚血瘀证证候积分改善"),
    new_item_id = c(NA, "S2"),
    stringsAsFactors = FALSE
  ),
  enter_round = 3
)
wording_demo$log

## -----------------------------------------------------------------------------
res1 <- analyze_delphi(pooled)
round1_cls <- res1$classification
round1_cls <- round1_cls[round1_cls$round == 1 & round1_cls$dimension == "重要性", ]
table(round1_cls$class_zh)

## -----------------------------------------------------------------------------
md2 <- delphi_questionnaire(res1, round = 2, expert_id = "E01", items = "all")
stopifnot(grepl("首次评分", md2), grepl("NEW-02", md2))
dir2 <- tempfile("chf-r2-")
dir.create(dir2)
delphi_questionnaire(res1, round = 2, format = "xlsx", file = dir2, items = "all")
delphi_questionnaire(res1, round = 2, format = "docx", file = dir2,
                     expert_id = "E01", items = "all")
basename(list.files(dir2))

## -----------------------------------------------------------------------------
src2 <- simulate_chf_new_items(up_to_round = 2)
study2 <- update_delphi_study(
  pooled,
  ratings = src2$ratings[src2$ratings$round == 2, ],
  comments = src2$comments
)
res2 <- analyze_delphi(study2)
show2 <- res2$classification
show2 <- show2[show2$round == 2 & show2$item_id %in% c("NEW-01", "NEW-02", "NEW-03") &
                 show2$dimension == "重要性", ]
show2[, c("item_id", "p_high", "p_low", "class_zh", "action", "n_scoring_rounds", "provisional")]

## -----------------------------------------------------------------------------
md3 <- delphi_questionnaire(res2, round = 3, expert_id = "E01", items = "iteration")
stopifnot(grepl("NEW-01", md3), grepl("CHF-11", md3))
dir3 <- tempfile("chf-r3-")
dir.create(dir3)
delphi_questionnaire(res2, round = 3, format = "xlsx", file = dir3, items = "iteration")
delphi_questionnaire(res2, round = 3, format = "docx", file = dir3,
                     expert_id = "E01", items = "iteration")
basename(list.files(dir3))

## -----------------------------------------------------------------------------
src3 <- simulate_chf_new_items(up_to_round = 3)
study3 <- update_delphi_study(
  study2,
  ratings = src3$ratings[src3$ratings$round == 3, ],
  comments = src3$comments[src3$comments$round == 3, ]
)
study3$floor_items <- data.frame(
  item_id = "FLOOR-01",
  item_text = "心衰患者照护者负担",
  stringsAsFactors = FALSE
)
res3 <- analyze_delphi(study3)
final_new <- res3$last_classification
final_new <- final_new[final_new$item_id %in% c("NEW-01", "NEW-02", "NEW-03", "CHF-11", "CHF-21"), ]
final_new[, c("item_id", "class_zh", "action", "p_high", "p_low", "provisional")]

## -----------------------------------------------------------------------------
utils::head(res3$item_log, 8)
agenda <- res3$meeting_agenda
table(agenda$band)
agenda[agenda$band %in% c("C", "floor"), c("item_id", "item_text", "band")]

## -----------------------------------------------------------------------------
floor <- agenda[agenda$band == "floor", ]
stopifnot(nrow(floor) == 1L)
floor$note

## -----------------------------------------------------------------------------
docx <- tempfile(fileext = ".docx")
out <- delphi_report(res3, file = docx, format = "docx", template = "comet_cos")
file.exists(out)
file.info(out)$size

