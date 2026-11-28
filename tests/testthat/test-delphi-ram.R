test_that("delphi_ram is a separate entry", {
  p <- delphi_ram_protocol("table4")
  expect_identical(p$scenario, "delphi_ram")
  expect_true(p$combine_ram)
  expect_identical(p$rand$rule, "table4")
  expect_identical(p$rand$round2_mode, "mail")
  expect_identical(p$stability$max_rounds, 3L)
  expect_true(is.na(p$rules$high_pct))

  ram <- simulate_delphi_ram("table4")
  expect_error(run_delphi(ram), "run_delphi_ram")
  expect_error(run_rand(ram), "run_delphi_ram")
  expect_error(delphi_questionnaire(ram, round = 1, format = "xlsx", file = tempfile()),
               "delphi_ram_questionnaire")
  expect_error(
    preset_protocol("delphi_ram", rand_disagreement = "both", rationale = "x"),
    "table4"
  )
})

test_that("table 4 and IPRAS keep the known split item", {
  tab <- classify_delphi_ram(simulate_delphi_ram("table4"), round = 1)
  ip <- classify_delphi_ram(simulate_delphi_ram("ipras"), round = 1)
  st05_tab <- tab[tab$item_id == "ST05", ]
  st05_ip <- ip[ip$item_id == "ST05", ]
  expect_true(st05_tab$table4_disagreement)
  expect_false(st05_tab$ipras_disagreement)
  expect_identical(st05_tab$class_detail, "uncertain_disagreement")
  expect_identical(st05_tab$consensus, "no_consensus")
  expect_identical(st05_ip$class_detail, "appropriate")
  expect_identical(st05_ip$consensus, "consensus_endorse")
  expect_false(st05_ip$disagreement)

  st06 <- tab[tab$item_id == "ST06", ]
  expect_identical(st06$class, "appropriate")
  expect_identical(st06$agreement_status, "indeterminate")
  expect_identical(st06$consensus, "consensus_endorse")
})

test_that("percent consensus does not treat a high median as enough", {
  pct <- simulate_delphi_ram("percent")
  r1 <- classify_delphi_ram(pct, round = 1)
  pc05 <- r1[r1$item_id == "PC05", ]
  expect_identical(pc05$decision_basis, "percent_consensus")
  expect_true(pc05$median >= 7)
  expect_lt(pc05$n_high / pc05$n_valid, 0.70)
  expect_identical(pc05$consensus, "no_consensus")
  expect_identical(pc05$class_detail, "uncertain_shortfall")
  expect_true(pc05$modified_ram)

  r2 <- classify_delphi_ram(pct, round = 2)
  expect_identical(r2$consensus[r2$item_id == "PC06"], "consensus_endorse")
  expect_identical(r2$consensus[r2$item_id == "PC05"], "no_consensus")
  expect_identical(r1$consensus[r1$item_id == "PC04"], "disagreement")
  expect_identical(r1$class[r1$item_id == "PC01"], "appropriate")
  expect_identical(r1$class[r1$item_id == "PC02"], "inappropriate")
})

test_that("questionnaires write docx and xlsx and can be read back", {
  ram <- simulate_delphi_ram("table4")
  d <- tempfile("dram-")
  dir.create(d)
  paths <- delphi_ram_questionnaire(ram, round = 1, format = "both", file = file.path(d, "round1"))
  expect_true(any(grepl("\\.docx$", paths)))
  expect_true(any(grepl("\\.xlsx$", paths)))
  expect_true(all(file.exists(paths)))

  base <- ram
  base$ratings <- base$ratings[base$ratings$round == 1, ]
  xlsx <- delphi_ram_questionnaire(
    ram, round = 2, format = "xlsx",
    file = file.path(d, "round2.xlsx"), prefill = TRUE
  )
  back <- read_survey_xlsx(xlsx, study = base)
  expect_true(all(c(1L, 2L) %in% back$ratings$round))
  expect_equal(sum(back$ratings$round == 2L), 9L * 6L)
})

test_that("run_delphi_ram writes a docx report and keeps sensitivity exploratory", {
  ram <- simulate_delphi_ram("ipras")
  res <- run_delphi_ram(ram, report = FALSE)
  expect_s3_class(res, "delphi_result")
  expect_identical(res$delphi_ram_meta$rule, "ipras")
  expect_true(any(res$sensitivity$kappa$exploratory))
  expect_false(all(res$sensitivity$by_item$matches_locked))

  f <- tempfile(fileext = ".docx")
  reported <- run_delphi_ram(ram, report = TRUE, format = "docx", file = f)
  expect_true(file.exists(attr(reported, "report_file")))
  expect_gt(file.info(attr(reported, "report_file"))$size, 1000)
})

test_that("ordinary Delphi classification is unchanged", {
  study <- simulate_delphi(n_experts = 12, n_items = 4, n_rounds = 2, seed = 3)
  expect_false(isTRUE(study$protocol$combine_ram))
  cls <- classify_consensus(summarize_items(study), study$protocol)
  expect_true(all(cls$rule_set == "clinical_consensus"))
  expect_error(classify_delphi_ram(study), "delphi_ram")
})
