test_that("delphi_blueprint has no ratings and can write round-1 questionnaire", {
  bp <- delphi_blueprint(
    protocol = preset_protocol("clinical_consensus"),
    panel = data.frame(expert_id = c("E01", "E02"), stakeholder = c("临床", "方法学")),
    items = data.frame(item_id = c("C01", "C02"), item_text = c("条目一", "条目二")),
    meta = list(title = "测试", objective = "教学", users = "研究者", geography = "中国大陆")
  )
  expect_equal(nrow(bp$ratings), 0)
  md <- delphi_questionnaire(bp, round = 1)
  expect_true(grepl("C01", md))
  expect_true(grepl("权威程度自评", md))
})

test_that("xlsx round-trip preserves scores and update_delphi_study rejects overlap", {
  skip_if_not_installed("openxlsx")
  study <- suppressWarnings(simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 21))
  f <- tempfile(fileext = ".xlsx")
  delphi_questionnaire(study, round = 1, format = "xlsx", file = f, prefill = TRUE)
  expect_true(file.exists(f))
  got <- read_survey_xlsx(f)
  expect_true(nrow(got$ratings) >= 8)
  expect_true(all(c("expert_id", "item_id", "round", "score") %in% names(got$ratings)))
  expect_equal(unique(got$ratings$round), 1L)
  orig <- study$ratings[study$ratings$round == 1, ]
  if (!"dimension" %in% names(orig)) orig$dimension <- "overall"
  expect_equal(nrow(got$ratings), nrow(orig))

  extra <- study$ratings
  extra$round <- 2L
  study2 <- update_delphi_study(study, ratings = extra)
  expect_equal(sort(unique(study2$ratings$round)), c(1L, 2L))
  expect_error(update_delphi_study(study, ratings = study$ratings), "重复")
})

test_that("next_round_questionnaire writes round 2/3 and errors after max_rounds", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("officer")
  study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 3, seed = 22)
  s1 <- study
  s1$ratings <- s1$ratings[s1$ratings$round == 1, ]
  if (!is.null(s1$comments)) s1$comments <- s1$comments[s1$comments$round == 1, ]
  res1 <- suppressWarnings(analyze_delphi(s1))
  d2 <- tempfile("r2-")
  fs <- next_round_questionnaire(res1, format = "xlsx", file = d2)
  expect_true(any(grepl("xlsx$", fs)))
  md2 <- delphi_questionnaire(res1, round = 2, expert_id = study$panel$expert_id[1])
  expect_true(grepl("不再", md2))
  expect_false(grepl("权威程度自评", md2))

  s2 <- update_delphi_study(s1, ratings = study$ratings[study$ratings$round == 2, ])
  res2 <- suppressWarnings(analyze_delphi(s2))
  d3 <- tempfile("r3-")
  fs3 <- next_round_questionnaire(res2, format = "both", file = d3)
  expect_true(any(grepl("docx$", fs3)))
  expect_true(any(grepl("xlsx$", fs3)))

  res3 <- suppressWarnings(analyze_delphi(study))
  expect_error(next_round_questionnaire(res3, format = "markdown"), "最多轮次")
})

test_that("analyze_delphi up_to_round keeps history but drops later rounds", {
  study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 3, seed = 23)
  res2 <- suppressWarnings(analyze_delphi(study, up_to_round = 2))
  expect_equal(res2$last_round, 2)
  expect_equal(max(res2$study$ratings$round), 2)
  expect_true(1 %in% res2$study$ratings$round)
  res3 <- suppressWarnings(analyze_delphi(study, up_to_round = 3))
  expect_equal(res3$last_round, 3)
})

test_that("run_delphi can write a docx report", {
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available(), "pandoc not available")
  study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 2, seed = 24)
  f <- tempfile(fileext = ".docx")
  res <- suppressWarnings(run_delphi(study, report = TRUE, format = "docx", file = f))
  out <- attr(res, "report_file")
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 1000)
})

test_that("protocol_table covers every delphi_protocol argument", {
  p <- preset_protocol("comet_cos")
  expect_false(isTRUE(p$modified_from_preset))
  tab <- protocol_table(p)
  expect_true(all(c("high_pct", "comet_in_high", "max_rounds", "locked") %in% tab$parameter))
  p2 <- preset_protocol("clinical_consensus", high_pct = 0.85,
                        rationale = "演示")
  expect_true(p2$modified_from_preset)
  tab2 <- protocol_table(p2)
  expect_true(isTRUE(tab2$modified[tab2$parameter == "high_pct"]))
})

test_that("blank xlsx cells are not ingested as unable", {
  skip_if_not_installed("openxlsx")
  bp <- delphi_blueprint(
    protocol = preset_protocol("clinical_consensus"),
    panel = data.frame(expert_id = c("E01", "E02"), stakeholder = c("临床", "方法学")),
    items = data.frame(item_id = c("C01", "C02"), item_text = c("条目一", "条目二")),
    meta = list(title = "测试", objective = "教学", users = "研究者", geography = "中国大陆")
  )
  f <- tempfile(fileext = ".xlsx")
  delphi_questionnaire(bp, round = 1, format = "xlsx", file = f)
  got <- read_survey_xlsx(f)
  expect_equal(nrow(got$ratings), 0L)
})

test_that("round-2 xlsx uses follow-up dimensions and the same unresolved items as markdown", {
  skip_if_not_installed("openxlsx")
  study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 2, seed = 25)
  s1 <- study
  s1$ratings <- s1$ratings[s1$ratings$round == 1, ]
  if (!is.null(s1$comments)) s1$comments <- s1$comments[s1$comments$round == 1, ]
  res1 <- suppressWarnings(analyze_delphi(s1))
  d <- tempfile("r2io-")
  fs <- next_round_questionnaire(res1, format = "xlsx", file = d, items = "unresolved")
  x2 <- fs[grepl("xlsx$", fs)][1]
  rat <- tryCatch(openxlsx::read.xlsx(x2, sheet = "评分"), error = function(e) NULL)
  dims <- if (is.null(rat) || !nrow(rat)) character() else unique(as.character(rat$dimension))
  expect_false("可行性" %in% dims)
  md <- delphi_questionnaire(res1, round = 2, expert_id = study$panel$expert_id[1],
                             items = "unresolved")
  md_ids <- unique(regmatches(md, gregexpr("### 条目 [A-Za-z0-9]+", md))[[1]])
  md_ids <- sub("^### 条目 ", "", md_ids)
  x_ids <- if (is.null(rat) || !nrow(rat)) character() else unique(as.character(rat$item_id))
  expect_equal(sort(x_ids), sort(md_ids))
  cls1 <- res1$last_classification
  dim0 <- if ("dimension" %in% names(cls1)) {
    prefer <- intersect(c("重要性", "overall"), unique(cls1$dimension))
    if (length(prefer)) cls1[cls1$dimension == prefer[1], ] else cls1
  } else cls1
  keep <- dim0$item_id[dim0$action %in% c("keep", "drop")]
  expect_false(any(x_ids %in% keep))
})

