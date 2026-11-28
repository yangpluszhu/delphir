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
data("consensus_demo", package = "delphir")
consensus_demo

## -----------------------------------------------------------------------------
unlist(consensus_demo$meta)

## -----------------------------------------------------------------------------
protocol <- preset_protocol("clinical_consensus")
protocol

## -----------------------------------------------------------------------------
stricter <- delphi_protocol(
  scenario = "clinical_consensus",
  high_pct = 0.85,
  rationale = "涉及受试者安全的硬性要求，将高度共识提高到 85%。"
)
stricter$modified_from_preset

## -----------------------------------------------------------------------------
head(consensus_demo$panel[, c("expert_id", "stakeholder", "years", "Ca", "Cs")])
head(consensus_demo$items[, c("item_id", "item_text", "domain", "pattern")])
head(consensus_demo$ratings)

## -----------------------------------------------------------------------------
ex <- system.file("extdata", "consensus_ratings.csv", package = "delphir")
if (nzchar(ex)) {
  raw <- utils::read.csv(ex, fileEncoding = "UTF-8")
  cat("外部评分表示例行数:", nrow(raw), "\n")
}

## -----------------------------------------------------------------------------
part <- response_summary(consensus_demo)
part$by_round
part$retention

## -----------------------------------------------------------------------------
plot_attrition(consensus_demo)

## -----------------------------------------------------------------------------
auth <- expert_authority(consensus_demo$panel)
attr(auth, "overall")
authority_scales()$cs
authority_scales()$ca

## -----------------------------------------------------------------------------
s1 <- summarize_items(consensus_demo, rounds = 1, dimensions = "重要性")
s1[, c("item_id", "n_valid", "n_unable", "median_iqr", "p_high", "p_low", "cv", "polarized")]

## -----------------------------------------------------------------------------
s1[order(-s1$median, -s1$iqr), c("item_id", "median", "iqr", "p_high")][1:8, ]

## -----------------------------------------------------------------------------
plot_item_distribution(consensus_demo, round = 1, dimension = "重要性")

## -----------------------------------------------------------------------------
c1 <- classify_consensus(s1, consensus_demo$protocol)
table(c1$class_zh)
c1[, c("item_id", "p_high", "median", "iqr", "class_zh", "action")]

## -----------------------------------------------------------------------------
detect_polarization(p_high = c(0, 50), p_low = c(0, 50))
coefficient_variation(c(8, 8, 8, 8))
coefficient_variation(c(1, 1, 1, 9, 9, 9))

## -----------------------------------------------------------------------------
kw <- kendall_w(consensus_demo, round = 1, dimension = "重要性")
kw

## -----------------------------------------------------------------------------
q1 <- summarize_comments(study = consensus_demo, round = 1)
q1$by_theme
head(q1$by_item)

## -----------------------------------------------------------------------------
fb <- prepare_feedback(consensus_demo, round = 1, dimension = "重要性")
fb[fb$expert_id == fb$expert_id[1],
   c("item_id", "own_score", "median_iqr", "p_high", "class_zh", "anonymous_comments")]

## -----------------------------------------------------------------------------
res <- analyze_delphi(consensus_demo)
res

## -----------------------------------------------------------------------------
last <- res$last_classification
last[, c("item_id", "median_iqr", "p_high", "p_low", "class_zh", "action")]

## -----------------------------------------------------------------------------
wide <- res$classification
wide <- wide[wide$dimension == "重要性",
             c("item_id", "round", "median_iqr", "p_high", "class_zh")]
wide

## -----------------------------------------------------------------------------
head(res$stability[, c("item_id", "median_r1", "median_r2", "p_high_r1",
                       "p_high_r2", "class_unchanged", "wilcoxon_p")])
res$stopping

## -----------------------------------------------------------------------------
plot_round_shift(res, metric = "p_high")

## -----------------------------------------------------------------------------
head(res$stakeholder[, c("item_id", "group_level", "n_valid", "median", "p_high", "class_zh")])

## -----------------------------------------------------------------------------
head(res$dropout)

## -----------------------------------------------------------------------------
feas <- classify_consensus(
  summarize_items(consensus_demo, rounds = 1, dimensions = "可行性"),
  consensus_demo$protocol
)
cmp <- merge(
  last[, c("item_id", "p_high", "class_zh")],
  feas[, c("item_id", "p_high", "class_zh")],
  by = "item_id", suffixes = c("_重要性", "_可行性")
)
head(cmp)

## -----------------------------------------------------------------------------
table(last$action, last$class_zh)

## -----------------------------------------------------------------------------
last[, c("item_id", "evidence_level", "p_high", "class_zh")]

## -----------------------------------------------------------------------------
html <- delphi_report(res, file = tempfile(fileext = ".html"), template = "clinical")
file.exists(html)

## -----------------------------------------------------------------------------
head(accord_checklist(res)[, c("id", "section", "status")], 8)
dcat_checklist(res)[1:4, c("id", "core", "item", "status")]

