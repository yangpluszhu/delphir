## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(delphir)
data("rand_ucla_demo", package = "delphir")
rand_ucla_demo
unlist(rand_ucla_demo$meta)

## -----------------------------------------------------------------------------
protocol <- preset_protocol("rand_ucla")
protocol
protocol$rand

## -----------------------------------------------------------------------------
head(rand_ucla_demo$items[, c("item_id", "item_text", "evidence_level", "pattern")])
table(rand_ucla_demo$panel$specialty)

## -----------------------------------------------------------------------------
rand_disagreement(c(8, 8, 9, 9, 8, 7, 9, 8, 8))

## -----------------------------------------------------------------------------
rand_disagreement(c(1, 1, 2, 2, 5, 8, 9, 9, 9))

## -----------------------------------------------------------------------------
s1 <- classify_rand(rand_ucla_demo, round = 1)
s1[, c("item_id", "n_valid", "median_iqr", "di", "classic_disagreement",
       "ipras_disagreement", "class_zh")]
table(s1$class_zh)

## -----------------------------------------------------------------------------
plot_rand_panel(s1)

## -----------------------------------------------------------------------------
fb <- prepare_feedback(rand_ucla_demo, round = 1)
fb[fb$expert_id == "P1",
   c("item_id", "own_score", "median", "di", "class_zh", "anonymous_comments")]

## -----------------------------------------------------------------------------
res <- analyze_delphi(rand_ucla_demo)
res
last <- res$last_classification
last[, c("item_id", "median_iqr", "di", "disagreement", "class_zh", "evidence_level")]

## -----------------------------------------------------------------------------
last[, c("item_id", "classic_disagreement", "ipras_disagreement", "di", "class_zh")]

## -----------------------------------------------------------------------------
res$stability[, c("item_id", "median_r1", "median_r2", "class_r1", "class_r2",
                  "class_unchanged")]
res$stopping

## -----------------------------------------------------------------------------
plot_round_shift(res, metric = "median")

## -----------------------------------------------------------------------------
last[, c("item_id", "evidence_level", "median", "di", "class_zh")]

## -----------------------------------------------------------------------------
summarize_comments(study = rand_ucla_demo)$by_theme

## -----------------------------------------------------------------------------
html <- delphi_report(res, file = tempfile(fileext = ".html"), template = "rand_ucla")
file.exists(html)
accord_checklist(res)[accord_checklist(res)$id %in% c("T1", "M9", "M11", "M14", "R3"),
                      c("id", "item", "status")]

