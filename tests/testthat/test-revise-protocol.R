test_that("the first lock is the study preset and does not require rationale", {
  warned <- character()
  p <- withCallingHandlers(
    preset_protocol("clinical_consensus", high_pct = 0.85, iqr_max = 3),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("rationale", warned)))
  expect_false(isTRUE(p$modified_from_preset))
  expect_equal(p$rules$high_pct, 0.85)
  expect_equal(p$study_preset$rules$high_pct, 0.85)
  tab <- protocol_table(p)
  expect_false(any(tab$modified))
})

test_that("revising a locked protocol requires rationale and keeps the original baseline", {
  p <- preset_protocol("clinical_consensus", high_pct = 0.85)
  expect_warning(revise_protocol(p, high_pct = 0.90), "rationale")
  p2 <- revise_protocol(p, high_pct = 0.90, rationale = "锁定后再提高。")
  expect_true(isTRUE(p2$modified_from_preset))
  expect_equal(p2$rules$high_pct, 0.90)
  expect_equal(p2$study_preset$rules$high_pct, 0.85)
  tab <- protocol_table(p2)
  expect_true(isTRUE(tab$modified[tab$parameter == "high_pct"]))
  expect_false(isTRUE(tab$modified[tab$parameter == "basic_pct"]))
  expect_equal(tab$preset[tab$parameter == "high_pct"], "0.85")

  p3 <- revise_protocol(p2, high_pct = 0.92, rationale = "相对初次锁定再调整。")
  expect_equal(p3$study_preset$rules$high_pct, 0.85)
  expect_equal(p3$rules$high_pct, 0.92)
  expect_true(isTRUE(p3$modified_from_preset))

  p4 <- revise_protocol(p3, high_pct = 0.85, rationale = "改回初次锁定值。")
  expect_false(isTRUE(p4$modified_from_preset))
  expect_false(any(protocol_table(p4)$modified))
})

test_that("a revision with no parameter change does not require rationale", {
  p <- preset_protocol("clinical_consensus", high_pct = 0.85)
  p2 <- revise_protocol(p)
  expect_false(isTRUE(p2$modified_from_preset))
  expect_equal(p2$rules$high_pct, 0.85)
})

test_that("an older protocol without study_preset uses its current values as the baseline", {
  p <- preset_protocol("clinical_consensus")
  p$study_preset <- NULL
  p2 <- revise_protocol(p, high_pct = 0.90, rationale = "旧对象锁定后再改。")
  expect_true(isTRUE(p2$modified_from_preset))
  expect_equal(p2$study_preset$rules$high_pct, 0.80)
  expect_equal(p2$rules$high_pct, 0.90)
})

test_that("initial RAND and RAM choices are not deviations", {
  ipras <- delphi_ram_protocol("ipras")
  expect_false(isTRUE(ipras$modified_from_preset))
  expect_null(ipras$rationale)
  pct <- preset_protocol(
    "rand_ucla",
    rand_disagreement = "percent",
    rand_purpose = "recommendation"
  )
  expect_false(isTRUE(pct$modified_from_preset))
  changed <- revise_protocol(
    ipras,
    rand_disagreement = "percent",
    rand_purpose = "recommendation",
    rationale = "锁定后改用百分比。"
  )
  expect_true(isTRUE(changed$modified_from_preset))
  expect_equal(changed$study_preset$rand$rule, "ipras")
})

test_that("a revised protocol survives the questionnaire sheet", {
  skip_if_not_installed("openxlsx")
  p <- revise_protocol(
    preset_protocol("clinical_consensus"),
    high_pct = 0.90,
    rationale = "问卷往返前已经锁定后再改。"
  )
  bp <- delphi_blueprint(
    protocol = p,
    panel = data.frame(expert_id = "E01", stakeholder = "临床"),
    items = data.frame(item_id = "C01", item_text = "条目"),
    meta = list(title = "测试", objective = "教学", users = "研究者", geography = "中国大陆")
  )
  f <- tempfile(fileext = ".xlsx")
  delphi_questionnaire(bp, round = 1, format = "xlsx", file = f)
  sheet <- openxlsx::read.xlsx(f, sheet = "方案")
  expect_true("初始锁定值" %in% names(sheet))
  got <- read_survey_xlsx(f)
  expect_equal(got$protocol$rules$high_pct, 0.90)
  expect_true(isTRUE(got$protocol$modified_from_preset))
  expect_equal(got$protocol$study_preset$rules$high_pct, 0.80)
  expect_equal(got$protocol$rationale, "问卷往返前已经锁定后再改。")
})
