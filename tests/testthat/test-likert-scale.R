test_that("9-point presets are unchanged", {
  p <- preset_protocol("clinical_consensus")
  expect_false(isTRUE(p$modified_from_preset))
  expect_equal(p$scale_max, 9)
  expect_equal(p$low_range, c(1, 3))
  expect_equal(p$mid_range, c(4, 6))
  expect_equal(p$high_range, c(7, 9))
  expect_equal(p$rules$median_min, 7)
  expect_equal(p$rules$iqr_max, 2)
  expect_equal(p$stability$median_delta, 1)
  expect_equal(p$rand$iprr, 2.35)
  expect_equal(p$rand$cfa, 1.5)
  expect_false(isTRUE(p$rand$ipras_rescaled))
  expect_false(isTRUE(p$rand$nonstandard_ipras))

  se <- preset_protocol("standard_element")
  expect_equal(se$scale_max, 5)
  expect_equal(se$rules$mean_min, 3.5)
  expect_equal(se$rules$mean_delta_max, 0.25)
  expect_false(isTRUE(se$modified_from_preset))

  ru <- preset_protocol("rand_ucla")
  expect_equal(ru$rand$iprr, 2.35)
  expect_false(isTRUE(ru$modified_from_preset))
})

test_that("5-point and 7-point presets fill scale-dependent parameters", {
  p5 <- preset_protocol("clinical_consensus", points = 5)
  expect_false(isTRUE(p5$modified_from_preset))
  expect_equal(p5$low_range, c(1, 2))
  expect_equal(p5$mid_range, c(3, 3))
  expect_equal(p5$high_range, c(4, 5))
  expect_equal(p5$rules$median_min, 4)
  expect_equal(p5$rules$iqr_max, 1)
  expect_equal(p5$stability$median_delta, 0.5)
  expect_equal(p5$rules$high_pct, 0.80)
  expect_equal(p5$rand$iprr, 2.35 * 4 / 8)
  expect_equal(p5$rand$cfa, 1.5 * 4 / 8)
  expect_true(isTRUE(p5$rand$ipras_rescaled))

  p7 <- delphi_protocol(scenario = "clinical_consensus", scale_max = 7)
  expect_false(isTRUE(p7$modified_from_preset))
  expect_equal(p7$low_range, c(1, 2))
  expect_equal(p7$mid_range, c(3, 5))
  expect_equal(p7$high_range, c(6, 7))
  expect_equal(p7$rules$median_min, 6)
  expect_equal(p7$rules$iqr_max, 1.5)
  expect_equal(p7$stability$median_delta, 0.75)

  se7 <- preset_protocol("standard_element", points = 7)
  expect_equal(se7$rules$mean_min, 4.75)
  expect_equal(se7$rules$mean_delta_max, 0.375)
  expect_false(isTRUE(se7$modified_from_preset))

  rk <- preset_protocol("ranking", points = 7)
  expect_true(is.na(rk$rules$median_min))
  expect_true(is.na(rk$rules$iqr_max))
  expect_equal(rk$high_range, c(6, 7))
})

test_that("explicit thresholds still override the scale preset", {
  p <- preset_protocol(
    "clinical_consensus", points = 5, median_min = 5,
    rationale = "演示：中位数下限不用高区下沿。"
  )
  expect_equal(p$rules$median_min, 5)
  expect_equal(p$high_range, c(4, 5))
  expect_false(isTRUE(p$modified_from_preset))
  tab <- protocol_table(p)
  expect_false(isTRUE(tab$modified[tab$parameter == "median_min"]))
  expect_false(isTRUE(tab$modified[tab$parameter == "high_range"]))
  p2 <- revise_protocol(p, median_min = 4, rationale = "改回该分制的高区下沿。")
  expect_true(isTRUE(p2$modified_from_preset))
  tab2 <- protocol_table(p2)
  expect_true(isTRUE(tab2$modified[tab2$parameter == "median_min"]))
  expect_false(isTRUE(tab2$modified[tab2$parameter == "high_range"]))
})

test_that("9-point IPRAS example is unchanged and 5-point constants rescale", {
  d9 <- rand_disagreement(c(6, 7, 7, 8, 8, 8, 9, 9, 9), method = "ipras")
  expect_equal(d9$ipras, 2.35 + 1.5 * d9$ai, tolerance = 1e-8)
  expect_false(isTRUE(d9$nonstandard_ipras))

  d5 <- suppressWarnings(rand_disagreement(
    c(4, 4, 5, 5, 5, 4, 5), scale = c(1, 5), method = "ipras"
  ))
  expect_equal(d5$ipras, (2.35 * 0.5) + (1.5 * 0.5) * d5$ai, tolerance = 1e-8)
  expect_false(isTRUE(d5$nonstandard_ipras))
  expect_equal(d5$n_high, sum(c(4, 4, 5, 5, 5, 4, 5) >= 4))
})

test_that("simulated 5-point and 7-point scores stay on the scale", {
  s5 <- simulate_delphi(n_experts = 12, n_items = 6, n_rounds = 2, seed = 11, points = 5)
  expect_s3_class(validate_delphi(s5), "delphi_study")
  scores <- s5$ratings$score
  expect_true(all(is.na(scores) | (scores >= 1 & scores <= 5)))
  expect_equal(s5$protocol$high_range, c(4, 5))

  s7 <- simulate_rand_ucla(n_experts = 9, n_items = 6, seed = 12, points = 7)
  scores7 <- s7$ratings$score
  expect_true(all(is.na(scores7) | (scores7 >= 1 & scores7 <= 7)))
  cls <- classify_rand(s7)
  expect_true(all(cls$n_low + cls$n_mid + cls$n_high == cls$n_valid))
})

test_that("necessity cut follows the high band", {
  study <- simulate_rand_ucla(n_experts = 9, n_items = 4, seed = 3, points = 5)
  study$protocol$rand$necessity <- TRUE
  nec <- study$ratings
  nec$dimension <- "必要性"
  study$ratings <- rbind(study$ratings, nec)
  out <- suppressWarnings(classify_necessity(study))
  eligible <- out[out$eligible %in% TRUE & !is.na(out$necessity_class), , drop = FALSE]
  expect_true(nrow(out) > 0)
  if (nrow(eligible)) {
    expect_true(all(grepl("4", eligible$note, fixed = TRUE)))
  }
})
