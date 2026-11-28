## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4.5,
                      message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(delphir)

## -----------------------------------------------------------------------------
polar <- rand_disagreement(c(1, 2, 2, 3, 5, 7, 8, 9, 9), method = "both")
polar[, c("median", "n_low", "n_high", "p30", "p70", "ipr", "ai", "ipras", "di",
          "table4_disagreement", "ipras_disagreement", "class_detail")]

## -----------------------------------------------------------------------------
agree <- rand_disagreement(c(6, 7, 7, 8, 8, 8, 9, 9, 9), method = "table4")
agree[, c("median", "n_out", "agreement_max", "agreement_status", "p30", "p70",
          "ipr", "ipras", "class")]

## -----------------------------------------------------------------------------
split_rule <- rand_disagreement(c(2, 3, 3, 7, 8, 8, 8, 9, 9), method = "table4")
split_rule[, c("median", "n_low", "n_high", "table4_disagreement", "p30", "p70",
               "ipr", "iprcp", "ai", "ipras", "ipras_disagreement", "class_zh")]
ipras_only <- rand_disagreement(c(2, 3, 3, 7, 8, 8, 8, 9, 9), method = "ipras")
ipras_only$class

## -----------------------------------------------------------------------------
rand_panel_thresholds(c(7, 9, 12, 15, 17))

## -----------------------------------------------------------------------------
mild <- c(1, 7, 7, 8, 8, 8, 8, 8, 8)
rand_disagreement(mild, method = "classic_strict", warn = FALSE)$class
rand_disagreement(mild, method = "classic_relaxed", warn = FALSE)$class
rand_disagreement(mild, method = "table4", warn = FALSE)$class

## -----------------------------------------------------------------------------
ram <- simulate_ram_classic()
sens <- rand_sensitivity(ram, round = 2, loo = TRUE)
sens$kappa
sens$by_item[, c("item_id", "class_table4", "class_ipras")]

## -----------------------------------------------------------------------------
sens$loo
plot_rand_sensitivity(sens)

## -----------------------------------------------------------------------------
rec <- simulate_rand_recommendation()
rec$protocol$rand[c("rule", "purpose", "percent_high", "percent_low", "round2_mode")]
rec$protocol$rationale

## -----------------------------------------------------------------------------
q <- rand_questionnaire(rec, round = 1, format = "docx",
                        file = tempfile("rec-q-", fileext = ".docx"))
basename(q)
cls <- classify_rand(rec, round = 2)
cls[, c("item_id", "median", "n_low", "n_high", "agreement_status", "class", "class_zh", "modified_ram")]

## -----------------------------------------------------------------------------
res <- run_rand(rec, report = FALSE, loo = FALSE)
res$rand_meta$modified_ram
res$rand_meta$round2_mode
docx <- tempfile("rec-report-", fileext = ".docx")
reported <- run_rand(rec, report = TRUE, format = "docx", file = docx, loo = FALSE)
file.exists(attr(reported, "report_file"))

