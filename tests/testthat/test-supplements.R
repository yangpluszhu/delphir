test_that("Kendall W applies the tie correction and chi-square", {
  mat <- matrix(c(
    1, 1, 2,
    1, 1, 2,
    1, 1, 2
  ), nrow = 3, byrow = TRUE)
  kw <- kendall_w(mat)
  expect_true(kw$tie_adjusted)
  expect_equal(kw$tie_correction, 18)
  expect_equal(kw$w, 1)
  expect_equal(kw$chi_sq, 3 * (3 - 1) * kw$w)
  expect_equal(kw$df, 2)
  expect_equal(kw$p_value, stats::pchisq(kw$chi_sq, df = 2, lower.tail = FALSE))

  agree <- matrix(rep(1:4, 5), nrow = 5, byrow = TRUE)
  plain <- kendall_w(agree)
  expect_equal(plain$tie_correction, 0)
  expect_false(plain$tie_adjusted)
})

test_that("rater reliability returns alpha, ICC, and optional Krippendorff alpha", {
  rel <- rater_reliability(cbind(1:5, 1:5))
  expect_equal(rel$cronbach_alpha, 1)

  same <- rbind(1:4, 1:4, 1:4)
  icc <- rater_reliability(same)
  expect_equal(icc$icc_2_1, 1, tolerance = 1e-8)
  expect_equal(icc$icc_2_k, 1, tolerance = 1e-8)
  if (requireNamespace("irr", quietly = TRUE)) {
    expect_true(is.finite(icc$krippendorff_alpha))
  } else {
    expect_null(icc$krippendorff_alpha)
    expect_true(grepl("irr", icc$krippendorff_note))
  }
})

test_that("custom authority scales are locked, rescored, and round-tripped", {
  sc <- authority_scales(cs = c(
    "很熟悉" = 0.9, "较熟悉" = 0.7, "一般" = 0.5,
    "不太熟悉" = 0.3, "不熟悉" = 0.1
  ))
  expect_false(isTRUE(
    preset_protocol("clinical_consensus", authority_scale = sc)$modified_from_preset
  ))
  p <- preset_protocol(
    "clinical_consensus",
    authority_scale = sc,
    rationale = "教学：改用另一套熟悉程度赋分。"
  )
  expect_false(isTRUE(p$modified_from_preset))
  tab <- protocol_table(p)
  expect_false(isTRUE(tab$modified[tab$parameter == "authority_cs"]))
  sc2 <- authority_scales(cs = c(
    "很熟悉" = 1, "较熟悉" = 0.8, "一般" = 0.6,
    "不太熟悉" = 0.4, "不熟悉" = 0.2
  ))
  expect_warning(revise_protocol(p, authority_scale = sc2), "rationale")
  p_rev <- revise_protocol(p, authority_scale = sc2, rationale = "锁定后更换赋分表。")
  expect_true(isTRUE(p_rev$modified_from_preset))
  tab_rev <- protocol_table(p_rev)
  expect_true(isTRUE(tab_rev$modified[tab_rev$parameter == "authority_cs"]))
  p2 <- suppressWarnings(delphir:::.protocol_from_table(tab$parameter, tab$value))
  expect_equal(unname(p2$authority_scale$cs), unname(sc$cs))

  study <- suppressWarnings(simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 11))
  study$protocol <- p
  study$panel$Ca <- NULL
  study$panel$Cs <- NULL
  res <- suppressWarnings(analyze_delphi(study, by_stakeholder = FALSE, check_dropout = FALSE, reliability = FALSE))
  expect_true(isTRUE(attr(res$authority, "rescored")))
  expect_equal(res$authority$Cs, score_cs(study$panel$familiarity, scales = sc))
  part <- res$participation$by_round
  expect_equal(part$positive_coefficient, part$response_rate)
  expect_true("meets_reference" %in% names(part))
})

test_that("round comparison records median, IPR, changed, and group movement", {
  ids <- paste0("E", 1:10)
  r1 <- data.frame(
    expert_id = rep(ids, 2),
    item_id = rep(c("A", "B"), each = 10),
    round = 1L,
    score = c(rep(8, 8), 4, 4, rep(8, 10)),
    stringsAsFactors = FALSE
  )
  r2 <- r1
  r2$round <- 2L
  r2$score <- c(c(1, 1, 2, 2, 3, 8, 9, 9, 9, 1), rep(8, 10))
  study <- delphi_study(
    protocol = preset_protocol("clinical_consensus"),
    panel = data.frame(expert_id = ids),
    items = data.frame(item_id = c("A", "B"), item_text = c("甲", "乙")),
    ratings = rbind(r1, r2)
  )
  res <- analyze_delphi(study, by_stakeholder = FALSE, check_dropout = FALSE)
  cmp <- res$round_comparison
  a <- cmp$items[cmp$items$item_id == "A", ]
  b <- cmp$items[cmp$items$item_id == "B", ]
  expect_true(a$changed)
  expect_false(b$changed)
  expect_true(is.finite(a$ipr_from) && is.finite(a$ipr_to))
  expect_true(a$ipr_to > a$ipr_from)
  expect_true(is.finite(a$wilcoxon_p))
  expect_true(grepl("探索", cmp$note))
  expect_true(grepl("Wilcoxon", cmp$note))
  expect_equal(nrow(cmp$group), 2L)
  expect_true(all(c("consensus_pct", "mean_ipr", "kendall_w", "tie_correction") %in% names(cmp$group)))
  expect_lt(cmp$group_change$consensus_pct_delta, 0)
  expect_true(all(res$sensitivity$summary$exploratory))
  expect_equal(res$study$protocol$rules$high_pct, 0.80)
  one <- study
  one$ratings <- one$ratings[one$ratings$round == 1, ]
  sens1 <- threshold_sensitivity(one, round = 1)
  expect_true(any(sens1$summary$parameter == "high_pct" & sens1$summary$n_changed > 0))
  expect_equal(one$protocol$rules$high_pct, 0.80)
  s <- summarize_items(study, rounds = 1)
  expect_equal(s$ipr, s$p70 - s$p30)

  txt <- paste(capture.output({
    report_rounds_section(res)
    report_sensitivity_section(res)
    report_extended_section(res)
  }), collapse = "\n")
  expect_true(grepl("探索", txt))
  expect_true(grepl("积极系数", txt))
  expect_true(grepl("Krippendorff", txt))
})

test_that("COMET, standard, ranking and policy sensitivity do not replace the locked rule", {
  cos <- suppressWarnings(simulate_cos(
    n_clinicians = 6, n_patients = 4, n_researchers = 4,
    n_items = 6, n_rounds = 1, seed = 12
  ))
  locked_in <- cos$protocol$rules$comet_in_high
  res <- suppressWarnings(analyze_delphi(cos, by_stakeholder = FALSE, check_dropout = FALSE, reliability = FALSE))
  expect_true(any(res$sensitivity$summary$parameter == "comet_in_high"))
  expect_equal(res$study$protocol$rules$comet_in_high, locked_in)
  expect_true(grepl("探索", res$sensitivity$note))

  std <- simulate_standard_delphi(seed = 13)
  mean0 <- std$protocol$rules$mean_min
  res_s <- suppressWarnings(analyze_delphi(std, reliability = FALSE))
  expect_true(any(res_s$sensitivity$summary$parameter == "mean_min"))
  expect_equal(res_s$study$protocol$rules$mean_min, mean0)

  ranked <- suppressWarnings(simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 14))
  ranked$protocol <- preset_protocol("ranking")
  sens_r <- threshold_sensitivity(ranked, round = 1)
  expect_true(all(sens_r$summary$parameter == "ranking_topk"))
  expect_true(grepl("Top-k", sens_r$note))

  pol <- suppressWarnings(simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 15))
  pol$protocol <- preset_protocol("policy")
  sens_p <- threshold_sensitivity(pol, round = 1)
  expect_true(all(sens_p$summary$parameter == "polarization_threshold"))
  expect_true(grepl("不以达成共识", sens_p$note))
})

test_that("RAND and modified Delphi keep their own sensitivity and gain round metrics", {
  ram <- simulate_ram_classic()
  res <- suppressWarnings(analyze_delphi(ram, reliability = FALSE))
  expect_true(is.data.frame(res$sensitivity$kappa))
  expect_true("kendall_w" %in% names(res$round_comparison$group))
  expect_true("group_change" %in% names(res$round_comparison))
  expect_true(any(res$round_comparison$items$changed))
  expect_true(grepl("不是百分比共识", res$round_comparison$consensus_definition))

  pct <- simulate_delphi_ram("percent")
  res_p <- suppressWarnings(analyze_delphi(pct, reliability = FALSE))
  expect_true(is.data.frame(res_p$sensitivity$kappa))
  expect_true(grepl("百分比", res_p$round_comparison$consensus_definition))
  expect_equal(res_p$last_classification$class, classify_delphi_ram(pct, round = res_p$last_round, warn = FALSE)$class)
})
