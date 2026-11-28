## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4,
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(delphir)
data("consensus_demo", package = "delphir")
p <- preset_protocol("clinical_consensus")
p

## -----------------------------------------------------------------------------
tab <- protocol_table(p)
nrow(tab)
head(tab, 10)

## -----------------------------------------------------------------------------
vapply(
  c("clinical_consensus", "comet_cos", "rand_ucla", "indicator",
    "ranking", "definition", "policy"),
  function(s) {
    pr <- preset_protocol(s)
    sprintf("%s | high_pct=%s | comet_in=%.2f | rand_di=%s",
            s,
            pr$rules$high_pct,
            pr$rules$comet_in_high,
            pr$rand$di_threshold)
  },
  character(1)
)

## -----------------------------------------------------------------------------
wrong <- delphi_protocol(
  scenario = "clinical_consensus",
  scale_max = 5,
  high_pct = 0.80,
  rationale = "错误示范：改了量表端点却没改高区。"
)
c(scale = paste(wrong$scale_min, wrong$scale_max, sep = "-"),
  high_range = paste(wrong$high_range, collapse = "-"))

## -----------------------------------------------------------------------------
s1 <- summarize_items(consensus_demo, rounds = 1)
s1 <- s1[s1$dimension == "重要性", ]
cls80 <- classify_consensus(s1, p)
p70 <- preset_protocol("clinical_consensus", high_pct = 0.70,
                       rationale = "仅用于演示：把高度共识从 80% 放到 70%。")
cls70 <- classify_consensus(s1, p70)
table(cls80$class_zh)
table(cls70$class_zh)

## -----------------------------------------------------------------------------
p_iqr <- preset_protocol("clinical_consensus", iqr_max = 1,
                         rationale = "演示：把 IQR 上限从 2 收到 1。")
table(classify_consensus(s1, p_iqr)$class_zh)

## -----------------------------------------------------------------------------
polar <- s1[order(-s1$p_high * 0 - s1$polarized, -pmin(s1$p_high, s1$p_low)),
            c("item_id", "median", "p_high", "p_low", "polarized")]
head(polar, 5)

## -----------------------------------------------------------------------------
pc <- preset_protocol("comet_cos")
unlist(pc$rules[c("comet_in_high", "comet_in_low_max", "comet_out_low", "comet_out_high_max")])

## -----------------------------------------------------------------------------
pr <- preset_protocol("rand_ucla")
pr$rand

## -----------------------------------------------------------------------------
p2 <- delphi_protocol(
  scenario = "clinical_consensus",
  high_pct = 0.85,
  rationale = "高风险诊疗推荐，将高度共识阈值提高到 85%。"
)
c(modified = p2$modified_from_preset, rationale = p2$rationale)

