test_that("Cs five-level Zeng scale", {
  expect_equal(
    score_cs(c("很熟悉", "较熟悉", "一般", "不太熟悉", "不熟悉")),
    c(1.0, 0.8, 0.6, 0.4, 0.2)
  )
  expect_equal(score_cs("一般熟悉"), 0.6)
  expect_equal(score_cs(0.8), 0.8)
})

test_that("Ca sums to 1.0 / 0.8 / 0.6 for all-large / mid / small", {
  expect_equal(score_ca("大", "大", "大", "大"), 1.0)
  expect_equal(score_ca("中", "中", "中", "中"), 0.8)
  expect_equal(score_ca("小", "小", "小", "小"), 0.6)
  expect_equal(score_ca("大", "中", "大", "小"), 0.5 + 0.2 + 0.1 + 0.1)
})

test_that("score_authority is expert-level and rejects item tables", {
  raw <- data.frame(
    expert_id = c("E1", "E2"),
    familiarity = c("较熟悉", "很熟悉"),
    practice = c("大", "中"),
    theory = c("中", "大"),
    literature = c("大", "中"),
    intuition = c("小", "小"),
    stringsAsFactors = FALSE
  )
  out <- score_authority(raw)
  expect_equal(out$Cs, c(0.8, 1.0))
  expect_equal(out$Ca[1], 0.5 + 0.2 + 0.1 + 0.1)
  expect_equal(out$Cr[1], (out$Ca[1] + out$Cs[1]) / 2)
  expect_true(all(c("mean_ca", "mean_cs", "mean_cr") %in% names(attr(out, "overall"))))

  bad_item <- cbind(raw[c(1, 1), ], item_id = c("C01", "C02"))
  expect_error(score_authority(bad_item), "按专家")

  bad_dup <- rbind(raw, raw[1, ])
  bad_dup$expert_id[3] <- "E1"
  expect_error(score_authority(bad_dup), "唯一")
})

test_that("expert_authority still computes Cr and rejects item_id", {
  panel <- data.frame(
    expert_id = c("E1", "E2"),
    Ca = c(0.9, 0.8),
    Cs = c(0.8, 0.6)
  )
  auth <- expert_authority(panel)
  expect_equal(auth$Cr, c(0.85, 0.70))
  expect_true(all(auth$cr_ok))
  expect_error(
    expert_authority(data.frame(expert_id = "E1", item_id = "C01", Ca = 0.9, Cs = 0.8)),
    "item_id"
  )
})

test_that("simulate_delphi authority columns come from scoring tables", {
  study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 2, seed = 11)
  expect_true(all(c("familiarity", "practice", "theory", "literature", "intuition", "Ca", "Cs") %in% names(study$panel)))
  rescore <- score_authority(study$panel)
  expect_equal(rescore$Ca, study$panel$Ca)
  expect_equal(rescore$Cs, study$panel$Cs)
})
