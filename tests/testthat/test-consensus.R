test_that("clinical composite rules match the methodology table", {
  proto <- preset_protocol("clinical_consensus")
  # A: 87%, median 8, IQR 1 -> high
  # B: 77%, median 8, IQR 3 -> basic (IQR fails high) or no? basic does not require IQR
  # C: 62%, median 7, IQR 2 -> no / borderline
  # D: 15% high and 75%+ low, median 3 -> reject
  summary <- data.frame(
    item_id = c("A", "B", "C", "D"),
    round = 1,
    dimension = "overall",
    median = c(8, 8, 7, 3),
    iqr = c(1, 3, 2, 2),
    p_high = c(87, 77, 62, 15),
    p_low = c(5, 8, 10, 80),
    polarized = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  cls <- classify_consensus(summary, proto)
  expect_equal(cls$class[1], "high_consensus")
  expect_equal(cls$class[2], "basic_consensus")
  expect_true(cls$class[3] %in% c("no_consensus", "borderline"))
  expect_equal(cls$class[4], "reject")
})

test_that("COMET 70/15 rule", {
  proto <- preset_protocol("comet_cos")
  summary <- data.frame(
    item_id = c("death", "fatigue", "labX", "y"),
    round = 1,
    dimension = "overall",
    median = c(9, 8, 6, 2),
    iqr = c(1, 2, 3, 2),
    p_high = c(96, 76, 52, 8),
    p_low = c(0, 9, 17, 76),
    polarized = FALSE,
    stringsAsFactors = FALSE
  )
  cls <- classify_consensus(summary, proto)
  expect_equal(cls$class, c("consensus_in", "consensus_in", "no_consensus", "consensus_out"))
})

test_that("unable ratings are excluded from the denominator", {
  proto <- preset_protocol("clinical_consensus")
  study <- delphi_study(
    protocol = proto,
    panel = data.frame(expert_id = c("E1", "E2", "E3", "E4")),
    items = data.frame(item_id = "I1", item_text = "test"),
    ratings = data.frame(
      expert_id = c("E1", "E2", "E3", "E4", "E1", "E2", "E3", "E4"),
      item_id = "I1",
      round = c(1, 1, 1, 1, 2, 2, 2, 2),
      score = c(8, 9, NA, 7, 8, 9, NA, 8),
      unable = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
    ),
    validate = TRUE
  )
  s <- summarize_items(study, rounds = 1)
  expect_equal(s$n_valid, 3)
  expect_equal(s$n_unable, 1)
  expect_equal(s$p_high, 100)
})

test_that("classify_consensus uses locked protocol and has no silent threshold args", {
  expect_false("high_pct" %in% names(formals(classify_consensus)))
  proto_a <- preset_protocol("clinical_consensus")
  proto_b <- delphi_protocol(
    scenario = "clinical_consensus",
    high_pct = 0.95,
    rationale = "测试不同阈值"
  )
  summary <- data.frame(
    item_id = "A", round = 1, dimension = "overall",
    median = 8, iqr = 1, p_high = 82, p_low = 5, polarized = FALSE
  )
  a <- classify_consensus(summary, proto_a)
  b <- classify_consensus(summary, proto_b)
  expect_equal(a$class, "high_consensus")
  expect_equal(b$class, "basic_consensus")
})
