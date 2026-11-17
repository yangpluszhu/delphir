test_that("delphi_questionnaire returns sample markdown and rejects item-level authority", {
  md <- delphi_questionnaire(sample = TRUE, round = 1)
  expect_true(grepl("权威程度自评", md))
  expect_true(grepl("无法判断", md))
  md2 <- delphi_questionnaire(sample = TRUE, round = 2)
  expect_true(grepl("不再", md2))
  expect_false(grepl("熟悉程度赋分 Cs", md2))
})

test_that("write_survey_templates copies files", {
  d <- tempfile("q-")
  fs <- write_survey_templates(d, include_mini_example = TRUE, overwrite = TRUE)
  expect_true(any(grepl("round1-complete.md", fs)))
  expect_true(any(grepl("round3-complete.md", fs)))
  expect_true(file.exists(file.path(d, "templates", "authority_raw.csv")))
  expect_true(file.exists(file.path(d, "mini_example", "survey_panel.csv")))
})

test_that("sample round-3 markdown has no Ca/Cs self-rating block", {
  md3 <- delphi_questionnaire(sample = TRUE, round = 3)
  expect_true(grepl("不再", md3))
  expect_false(grepl("权威程度自评", md3))
  expect_false(grepl("熟悉程度赋分 Cs", md3))
})

test_that("mini survey CSV pipeline produces authority and round-1 study", {
  ex <- system.file("extdata", package = "delphir")
  panel <- utils::read.csv(file.path(ex, "survey_panel.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  raw <- utils::read.csv(file.path(ex, "survey_authority_raw.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  items <- utils::read.csv(file.path(ex, "survey_items.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  ratings <- utils::read.csv(file.path(ex, "survey_ratings.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  scored <- score_authority(raw)
  expect_equal(scored$Cs[scored$expert_id == "E01"], 1.0)
  expect_equal(scored$Ca[scored$expert_id == "E01"], 0.9)
  panel$Ca <- scored$Ca[match(panel$expert_id, scored$expert_id)]
  panel$Cs <- scored$Cs[match(panel$expert_id, scored$expert_id)]
  study <- suppressWarnings(delphi_study(
    protocol = preset_protocol("clinical_consensus"),
    panel = panel,
    items = items,
    ratings = ratings[ratings$round == 1, ],
    meta = list(title = "mini", objective = "教学", users = "研究者", geography = "中国大陆")
  ))
  res <- suppressWarnings(analyze_delphi(study))
  expect_equal(res$last_round, 1)
  expect_true(!is.null(res$authority))
  q1 <- delphi_questionnaire(study, round = 1)
  expect_true(grepl("C01", q1))
  q2 <- delphi_questionnaire(res, round = 2, expert_id = "E01")
  expect_true(grepl("E01", q2))
  expect_true(grepl("不再", q2))
})
