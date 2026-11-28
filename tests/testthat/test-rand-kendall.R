test_that("RAND IPRAS and classic disagreement on known patterns", {
  tight <- c(8, 8, 9, 9, 8, 7, 9, 8, 8)
  d1 <- rand_disagreement(tight)
  expect_false(d1$disagreement)
  expect_lt(d1$di, 1)

  split <- c(1, 1, 2, 2, 5, 8, 9, 9, 9)
  d2 <- rand_disagreement(split)
  expect_true(d2$classic_disagreement)
  expect_true(d2$di >= 1 || d2$classic_disagreement)

  # reconstruct Fitch-style numbers
  expect_equal(d1$ipras, 2.35 + 1.5 * d1$ai, tolerance = 1e-8)
  expect_equal(d1$di, d1$ipr / d1$ipras, tolerance = 1e-8)
})

test_that("RAND classification: disagreement forces uncertain", {
  study <- delphi_study(
    protocol = preset_protocol("rand_ucla"),
    panel = data.frame(expert_id = paste0("P", 1:9)),
    items = data.frame(item_id = "IND1", item_text = "test"),
    ratings = data.frame(
      expert_id = rep(paste0("P", 1:9), 2),
      item_id = "IND1",
      round = rep(1:2, each = 9),
      score = c(c(8, 8, 8, 8, 8, 8, 1, 1, 1), c(8, 8, 8, 8, 8, 8, 2, 1, 1))
    )
  )
  cls <- classify_rand(study)
  expect_true(cls$disagreement)
  expect_equal(cls$class, "uncertain")
})

test_that("Kendall W endpoints", {
  agree <- matrix(rep(1:4, 5), nrow = 5, byrow = TRUE)
  w1 <- kendall_w(agree)
  expect_equal(w1$w, 1, tolerance = 1e-8)

  set.seed(1)
  noise <- matrix(replicate(8, sample.int(6)), nrow = 8, byrow = TRUE)
  w0 <- kendall_w(noise)
  expect_lt(w0$w, 0.4)
})

test_that("detect_polarization flags both tails", {
  expect_true(detect_polarization(p_high = 50, p_low = 40))
  expect_false(detect_polarization(p_high = 87, p_low = 5))
})
