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
data("cos_demo", package = "delphir")
cos_demo
table(cos_demo$panel$stakeholder)

## -----------------------------------------------------------------------------
protocol <- preset_protocol("comet_cos")
protocol$rules[c("comet_in_high", "comet_in_low_max",
                 "comet_out_low", "comet_out_high_max")]

## -----------------------------------------------------------------------------
unlist(cos_demo$meta)

## -----------------------------------------------------------------------------
table(cos_demo$items$outcome_domain)
head(cos_demo$items[, c("item_id", "item_text", "outcome_domain", "pattern")])

## -----------------------------------------------------------------------------
response_summary(cos_demo)$by_round
response_summary(cos_demo)$retention

## -----------------------------------------------------------------------------
res <- analyze_delphi(cos_demo)
res
last <- res$last_classification
last[, c("item_id", "item_text", "n_valid", "n_unable", "p_high", "p_low", "class_zh")]

## -----------------------------------------------------------------------------
plot_item_distribution(cos_demo, round = 2)

## -----------------------------------------------------------------------------
st <- res$stakeholder
# 找总体纳入但某组未纳入的结局
overall_in <- last$item_id[last$class == "consensus_in"]
st_in <- st[st$item_id %in% overall_in, c("item_id", "group_level", "n_valid",
                                         "p_high", "p_low", "class_zh")]
st_in

## -----------------------------------------------------------------------------
plot_stakeholder(res)

## -----------------------------------------------------------------------------
clinical_rule <- classify_consensus(res$item_summary, preset_protocol("clinical_consensus"))
cmp <- merge(
  last[, c("item_id", "p_high", "p_low", "class_zh")],
  clinical_rule[clinical_rule$round == res$last_round,
                c("item_id", "class_zh")],
  by = "item_id", suffixes = c("_COMET", "_临床复合")
)
head(cmp, 10)

## -----------------------------------------------------------------------------
res$stability[, c("item_id", "p_high_r1", "p_high_r2", "class_r1", "class_r2",
                  "class_unchanged")]
res$stopping

## -----------------------------------------------------------------------------
plot_round_shift(res, metric = "p_high")

## -----------------------------------------------------------------------------
db <- res$dropout
head(db[order(db$wilcoxon_p), c("item_id", "median_completer", "median_dropout",
                                "p_high_completer", "p_high_dropout", "wilcoxon_p")])

## -----------------------------------------------------------------------------
fb <- prepare_feedback(res, round = 1)
fb[fb$expert_id == fb$expert_id[1],
   c("item_id", "own_score", "p_high", "class_zh")]

## -----------------------------------------------------------------------------
summarize_comments(study = cos_demo)$by_theme

## -----------------------------------------------------------------------------
table(last$class_zh)

## -----------------------------------------------------------------------------
html <- delphi_report(res, file = tempfile(fileext = ".html"), template = "comet_cos")
file.exists(html)

## -----------------------------------------------------------------------------
dcat_checklist(res)[, c("id", "core", "status", "item")]

