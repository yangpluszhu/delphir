test_that("standard_element preset is a locked 5-point protocol", {
  p <- preset_protocol("standard_element")
  expect_false(isTRUE(p$modified_from_preset))
  expect_equal(p$scale_max, 5)
  expect_equal(p$low_range, c(1, 2))
  expect_equal(p$high_range, c(4, 5))
  expect_equal(p$rules$mean_min, 3.5)
  expect_equal(p$rules$w_min, 0.5)
  expect_equal(p$stability$max_rounds, 4L)
  expect_true(p$open_first)
  clin <- preset_protocol("clinical_consensus")
  expect_false(isTRUE(clin$modified_from_preset))
  expect_equal(clin$scale_max, 9)
  expect_false(clin$open_first)
  tab <- protocol_table(p)
  expect_true("mean_min" %in% tab$parameter)
  expect_true("open_first" %in% tab$parameter)
})

test_that("full score ratio is the share of maximum scores", {
  expect_equal(full_score_ratio(c(5, 5, 4, 5, 3), scale_max = 5), 60)
  expect_equal(full_score_ratio(c(5, NA, 5), scale_max = 5), 100)
  study <- simulate_standard_delphi(2026)
  s <- summarize_items(study, rounds = 4, dimensions = "重要程度")
  raw <- study$ratings
  raw <- raw[raw$round == 4 & raw$item_id == "T02" & raw$dimension == "重要程度" & !raw$unable, ]
  expect_equal(s$full_rate[s$item_id == "T02"], full_score_ratio(raw$score, study$protocol))
})

test_that("importance mean and CV decide retention, familiarity does not", {
  proto <- preset_protocol("standard_element")
  sm <- data.frame(
    item_id = c("A", "B", "C"),
    round = 2L,
    dimension = c("重要程度", "重要程度", "熟悉程度"),
    mean = c(4.2, 3.2, 4.8),
    cv = c(0.10, 0.10, 0.05),
    p_high = c(80, 20, 90),
    p_low = c(0, 10, 0),
    median = c(4, 3, 5),
    iqr = c(1, 1, 0),
    polarized = FALSE,
    stringsAsFactors = FALSE
  )
  cls <- classify_consensus(sm, proto)
  expect_equal(cls$class, c("retain", "not_retain", "described"))
  expect_equal(cls$action, c("keep", "drop", "report"))
})

test_that("simulated study stops on coordination only at the last scoring round", {
  study <- simulate_standard_delphi(2026)
  expect_equal(sort(unique(study$ratings$round)), 2:4)
  expect_gte(nrow(study$panel), 30)
  expect_true(all(panel_compliance(study)$ok))
  full <- coordination_decision(study)
  expect_true(full$recommend_stop)
  expect_true(full$by_round$criteria_met[full$by_round$round == 4])
  expect_false(full$by_round$criteria_met[full$by_round$round == 3])
  early <- study
  early$ratings <- early$ratings[early$ratings$round <= 3, ]
  expect_false(coordination_decision(early)$recommend_stop)
  res <- analyze_delphi(study)
  expect_true(res$stopping$recommend_stop)
  app <- appendix_b_table(res)
  kept <- res$last_classification$item_id[res$last_classification$action == "keep"]
  expect_equal(nrow(app), length(kept))
  expect_true("T10" %in% res$last_classification$item_id[res$last_classification$action == "drop"])
  expect_true(all(app$mean >= 3.5))
  expect_gt(res$open_round$n_comments, 0)
})

test_that("open round has no Likert and feedback shows mean and own score", {
  study <- simulate_standard_delphi(2026)
  bp <- delphi_blueprint(
    study$protocol, study$panel, study$meta$initial_items, meta = study$meta
  )
  md1 <- delphi_questionnaire(bp, round = 1)
  expect_false(grepl("1  2  3  4  5", md1, fixed = TRUE))
  expect_false(grepl("权威程度自评", md1, fixed = TRUE))
  expect_true(grepl("开放式", md1))
  md2 <- delphi_questionnaire(study, round = 2)
  expect_true(grepl("权威程度自评", md2, fixed = TRUE))
  expect_true(grepl("重要程度", md2, fixed = TRUE))
  expect_true(grepl("熟悉程度", md2, fixed = TRUE))
  res <- analyze_delphi(study)
  md3 <- delphi_questionnaire(res, round = 3, expert_id = "E01")
  expect_true(grepl("均数", md3, fixed = TRUE))
  expect_true(grepl("满分比", md3, fixed = TRUE))
  expect_true(grepl("您的上轮评分", md3, fixed = TRUE))
  expect_false(grepl("权威程度自评", md3, fixed = TRUE))
})

test_that("round-2 xlsx round-trips 5-point scores and restores the protocol", {
  skip_if_not_installed("openxlsx")
  study <- simulate_standard_delphi(2026)
  f <- tempfile(fileext = ".xlsx")
  delphi_questionnaire(study, round = 2, format = "xlsx", file = f, prefill = TRUE)
  got <- read_survey_xlsx(f)
  expect_equal(got$protocol$scale_max, 5)
  expect_true(got$protocol$open_first)
  orig <- study$ratings[study$ratings$round == 2 & !study$ratings$unable, ]
  m <- merge(
    orig[, c("expert_id", "item_id", "dimension", "score")],
    got$ratings[, c("expert_id", "item_id", "dimension", "score")],
    by = c("expert_id", "item_id", "dimension"),
    suffixes = c(".o", ".g")
  )
  expect_gt(nrow(m), 0)
  expect_equal(m$score.o, m$score.g)
})

test_that("standard report is a docx that states the appendix statistics", {
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available(), "pandoc not available")
  study <- simulate_standard_delphi(2026)
  f <- tempfile(fileext = ".docx")
  res <- run_delphi(study, report = TRUE, format = "docx", file = f)
  out <- attr(res, "report_file")
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 5000)
  xml <- tempfile("docx-xml-")
  dir.create(xml)
  utils::unzip(out, files = "word/document.xml", exdir = xml)
  txt <- paste(readLines(file.path(xml, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  expect_true(grepl("满分比", txt, fixed = TRUE))
  expect_true(grepl("附录", txt, fixed = TRUE))
  expect_true(grepl("德尔菲法", txt, fixed = TRUE))
})
