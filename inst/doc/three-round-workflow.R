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
data("consensus_3round_demo", package = "delphir")
demo <- consensus_3round_demo
demo
unique(demo$ratings$round)

## -----------------------------------------------------------------------------
protocol <- preset_protocol("clinical_consensus")
protocol
head(protocol_table(protocol), 8)

## -----------------------------------------------------------------------------
bp <- delphi_blueprint(protocol, demo$panel, demo$items, demo$meta)
nrow(bp$ratings)

## -----------------------------------------------------------------------------
dir1 <- tempfile("r1-")
dir.create(dir1)
files1 <- delphi_questionnaire(bp, round = 1, format = "both", file = dir1)
basename(files1)

## -----------------------------------------------------------------------------
xlsx1 <- tempfile(fileext = ".xlsx")
study_pre <- delphi_study(protocol, demo$panel, demo$items,
                          demo$ratings[demo$ratings$round == 1, ],
                          demo$comments[demo$comments$round == 1, ],
                          meta = demo$meta, validate = FALSE)
delphi_questionnaire(study_pre, round = 1, format = "xlsx", file = xlsx1, prefill = TRUE)
got1 <- read_survey_xlsx(xlsx1)
nrow(got1$ratings)
head(got1$ratings[, c("expert_id", "item_id", "score", "unable")])

## -----------------------------------------------------------------------------
study1 <- delphi_study(
  protocol, demo$panel, demo$items,
  demo$ratings[demo$ratings$round == 1, ],
  demo$comments[demo$comments$round == 1, ],
  meta = demo$meta
)
res1 <- run_delphi(study1, round = 1)
res1

## -----------------------------------------------------------------------------
head(res1$participation$by_round)
table(res1$last_classification$class_zh)

## -----------------------------------------------------------------------------
head(accord_checklist(res1)[, c("id", "status")], 6)

## -----------------------------------------------------------------------------
dir2 <- tempfile("r2-")
files2 <- next_round_questionnaire(res1, format = "xlsx", file = dir2)
basename(files2)

## -----------------------------------------------------------------------------
md2 <- delphi_questionnaire(res1, round = 2, expert_id = demo$panel$expert_id[1])
grepl("权威程度自评", md2)
grepl("不再", md2)

## -----------------------------------------------------------------------------
study2 <- update_delphi_study(
  study1,
  ratings = demo$ratings[demo$ratings$round == 2, ],
  comments = demo$comments[demo$comments$round == 2, ]
)
unique(study2$ratings$round)
res2 <- run_delphi(study2, round = 2)
res2$participation$by_round
if (!is.null(res2$stability)) {
  attr(res2$stability, "stopping")[c("recommend_stop", "class_unchanged_pct")]
}

## -----------------------------------------------------------------------------
head(res2$dropout[, intersect(c("item_id", "from_round", "n_dropout",
                                "median_completer", "median_dropout"), names(res2$dropout))])

## -----------------------------------------------------------------------------
dir3 <- tempfile("r3-")
files3 <- next_round_questionnaire(res2, format = "both", file = dir3)
basename(files3)

study3 <- update_delphi_study(
  study2,
  ratings = demo$ratings[demo$ratings$round == 3, ],
  comments = demo$comments[demo$comments$round == 3, ]
)
res3 <- run_delphi(study3, round = "last")
res3
table(res3$last_classification$class_zh)

## ----error=TRUE---------------------------------------------------------------
try({
try(next_round_questionnaire(res3, format = "markdown"), silent = FALSE)
})

## -----------------------------------------------------------------------------
res_upto2 <- analyze_delphi(study3, up_to_round = 2)
res_upto2$last_round
max(res_upto2$study$ratings$round)

## -----------------------------------------------------------------------------
docx <- tempfile(fileext = ".docx")
out <- run_delphi(study3, report = TRUE, format = "docx", file = docx)
file.exists(attr(out, "report_file"))

## -----------------------------------------------------------------------------
if (!is.null(res3$authority)) attr(res3$authority, "overall")[c("mean_cr", "pct_ok")]

## -----------------------------------------------------------------------------
utils::head(res3$classification[res3$classification$round == 3,
  c("item_id", "median_iqr", "p_high", "class_zh", "action")], 8)

## -----------------------------------------------------------------------------
if (!is.null(res3$stopping)) {
  res3$stopping[c("recommend_stop", "current_round", "max_rounds")]
}

## -----------------------------------------------------------------------------
dcat_checklist(res3)[1:4, c("id", "core", "status")]

