test_that("consensus_3round_demo contains three rounds", {
  data("consensus_3round_demo", package = "delphir", envir = environment())
  expect_s3_class(consensus_3round_demo, "delphi_study")
  expect_equal(sort(unique(consensus_3round_demo$ratings$round)), 1:3)
  expect_gt(nrow(consensus_3round_demo$ratings), 0)
})

test_that("rounds 1-3 write docx+xlsx and xlsx round-trip matches scores", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("officer")
  study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 3, seed = 31)
  orig1 <- study$ratings[study$ratings$round == 1, , drop = FALSE]
  if (!"dimension" %in% names(orig1)) orig1$dimension <- "overall"
  if (!"unable" %in% names(orig1)) orig1$unable <- is.na(orig1$score)

  f1 <- tempfile(fileext = ".xlsx")
  d1 <- tempfile("r1docx-")
  dir.create(d1)
  delphi_questionnaire(study, round = 1, format = "xlsx", file = f1, prefill = TRUE)
  delphi_questionnaire(study, round = 1, format = "docx", file = d1)
  expect_true(file.exists(f1))
  expect_true(any(grepl("docx$", list.files(d1, full.names = TRUE))))

  got <- read_survey_xlsx(f1)
  m <- merge(
    orig1[, c("expert_id", "item_id", "dimension", "score", "unable")],
    got$ratings[, c("expert_id", "item_id", "dimension", "score", "unable")],
    by = c("expert_id", "item_id", "dimension"),
    suffixes = c(".o", ".g")
  )
  expect_gt(nrow(m), 0)
  expect_equal(nrow(got$ratings), nrow(orig1))
  expect_equal(m$score.o, m$score.g)
  expect_equal(as.logical(m$unable.o), as.logical(m$unable.g))

  s1 <- study
  s1$ratings <- s1$ratings[s1$ratings$round == 1, ]
  if (!is.null(s1$comments)) s1$comments <- s1$comments[s1$comments$round == 1, ]
  res1 <- suppressWarnings(analyze_delphi(s1))
  d2 <- tempfile("r2-")
  fs2 <- next_round_questionnaire(res1, format = "both", file = d2)
  expect_true(any(grepl("docx$", fs2)))
  expect_true(any(grepl("xlsx$", fs2)))

  s2 <- update_delphi_study(s1, ratings = study$ratings[study$ratings$round == 2, ])
  res2 <- suppressWarnings(analyze_delphi(s2))
  d3 <- tempfile("r3-")
  fs3 <- next_round_questionnaire(res2, format = "both", file = d3)
  expect_true(any(grepl("docx$", fs3)))
  expect_true(any(grepl("xlsx$", fs3)))

  x3 <- fs3[grepl("xlsx$", fs3)][1]
  sheets <- openxlsx::getSheetNames(x3)
  expect_true(any(sheets %in% c("方案", "protocol")))
  expect_false("权威系数" %in% sheets)
})

test_that("three-round analyze, overlap rejection, and final docx report", {
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available(), "pandoc not available")
  data("consensus_3round_demo", package = "delphir", envir = environment())
  demo <- consensus_3round_demo
  res2 <- suppressWarnings(analyze_delphi(demo, up_to_round = 2))
  expect_equal(res2$last_round, 2)
  expect_equal(max(res2$study$ratings$round), 2)
  expect_true(1 %in% res2$study$ratings$round)
  expect_false(3 %in% res2$study$ratings$round)

  expect_error(update_delphi_study(demo, ratings = demo$ratings[demo$ratings$round == 1, ]), "重复")

  res3 <- suppressWarnings(analyze_delphi(demo, up_to_round = 3))
  expect_equal(res3$last_round, 3)
  expect_error(next_round_questionnaire(res3, format = "markdown"), "最多轮次")

  f <- tempfile(fileext = ".docx")
  out <- suppressWarnings(run_delphi(demo, report = TRUE, format = "docx", file = f))
  path <- attr(out, "report_file")
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 1000)
})
