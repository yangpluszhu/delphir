## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4.5,
                      message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(delphir)
nec <- simulate_rand_necessity()
nec$protocol$rand$necessity
sort(unique(nec$ratings$round))
unique(nec$ratings$dimension)

## -----------------------------------------------------------------------------
app <- classify_rand(nec, round = 2)
app[, c("item_id", "median", "disagreement", "class", "class_zh")]

## -----------------------------------------------------------------------------
path <- rand_questionnaire(nec, round = "necessity", format = "both",
                           file = tempfile("ram-nec-"))
basename(path)
raw <- openxlsx::read.xlsx(path[grepl("xlsx", path)][1], sheet = "\u8bc4\u5206")
unique(raw$item_id)
unique(raw$dimension)
unique(raw$round)

## -----------------------------------------------------------------------------
out <- classify_necessity(nec)
out[, c("item_id", "appropriateness_zh", "eligible", "n_valid", "median",
        "disagreement", "necessity_class", "necessity_zh", "note")]

## -----------------------------------------------------------------------------
res <- run_rand(nec, report = FALSE, loo = FALSE)
res$last_round
res$rand_meta$deviation
table(res$necessity$necessity_zh, useNA = "ifany")

## -----------------------------------------------------------------------------
res$round_comparison$group

## -----------------------------------------------------------------------------
docx <- tempfile("ram-nec-report-", fileext = ".docx")
reported <- run_rand(nec, report = TRUE, format = "docx", file = docx, loo = FALSE)
attr(reported, "report_file")

