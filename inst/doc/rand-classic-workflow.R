## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4.5,
                      message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(delphir)
ram <- simulate_ram_classic()
ram
ram$meta$objective

## -----------------------------------------------------------------------------
protocol <- ram$protocol
protocol
protocol$rand
protocol$stability$max_rounds

## -----------------------------------------------------------------------------
tab <- protocol_table(protocol)
tab[tab$parameter %in% c("rand_disagreement", "rand_round2_mode", "max_rounds", "rand_boundary"), ]

## -----------------------------------------------------------------------------
qdir <- tempfile("ram-q1-")
dir.create(qdir)
q1 <- rand_questionnaire(ram, round = 1, format = "both", file = qdir)
q1

## -----------------------------------------------------------------------------
ram$items[, c("item_id", "indication", "procedure", "evidence_level", "pattern")]

## -----------------------------------------------------------------------------
cls1 <- classify_rand(ram, round = 1)
cls1[, c("item_id", "n_valid", "median", "n_low", "n_mid", "n_high", "n_out",
         "agreement_status", "class", "class_zh")]

## -----------------------------------------------------------------------------
plot_rand_heatmap(ram, round = 1)

## -----------------------------------------------------------------------------
q2 <- rand_questionnaire(ram, round = 2, format = "both",
                         file = tempfile("ram-q2-"))
basename(q2)
raw2 <- openxlsx::read.xlsx(q2[grepl("xlsx", q2)][1], sheet = "\u8bc4\u5206")
length(unique(raw2$item_id))
head(raw2[, intersect(c("expert_id", "item_id", "own_prev", "class_zh"), names(raw2))], 3)

## -----------------------------------------------------------------------------
res <- run_rand(ram, report = FALSE, loo = TRUE)
res$last_round
res$rand_meta
table(res$last_classification$class_detail)

## -----------------------------------------------------------------------------
res$last_classification[, c("item_id", "median", "n_low", "n_high", "ipr", "ipras",
                            "di", "table4_disagreement", "ipras_disagreement", "class_zh")]

## -----------------------------------------------------------------------------
cmp <- res$round_comparison
cmp$group
cmp$items[, c("item_id", "median_from", "median_to", "class_zh_from", "class_zh_to", "changed", "wilcoxon_p")]
cmp$note

## -----------------------------------------------------------------------------
plot_rand_progress(ram)
plot_round_shift(ram)

## -----------------------------------------------------------------------------
sens <- res$sensitivity
sens$kappa
sens$loo
sens$note
plot_rand_sensitivity(sens)

## -----------------------------------------------------------------------------
plot_rand_deviation(ram, round = 2)

## -----------------------------------------------------------------------------
report_file <- tempfile("ram-report-", fileext = ".docx")
reported <- run_rand(ram, report = TRUE, format = "docx", file = report_file, loo = FALSE)
attr(reported, "report_file")
file.exists(attr(reported, "report_file"))

