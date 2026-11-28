test_that("duplicate writings by one expert count as one person", {
  protocol <- preset_protocol("comet_cos")
  suggestions <- delphi_suggestions(data.frame(
    expert_id = c("E1", "E1", "E1"),
    raw_name = c("睡眠", "睡眠", "还是睡眠"),
    concept_id = "C1",
    stringsAsFactors = FALSE
  ))
  review <- data.frame(
    concept_id = "C1",
    standardized_name = "睡眠质量",
    construct = "new_item",
    in_scope = TRUE,
    external_support = FALSE,
    stringsAsFactors = FALSE
  )
  screen <- screen_new_items(suggestions, review, protocol)
  expect_equal(screen$concepts$n_independent, 1L)
  expect_equal(screen$concepts$decision, "exclude")
  expect_equal(screen$n_admitted, 0L)
})

test_that("scope outranks frequency and channel B admits a single supported concept", {
  protocol <- preset_protocol("comet_cos")
  suggestions <- delphi_suggestions(data.frame(
    expert_id = c("E1", "E2", "E3", "E4"),
    raw_name = c("费用", "费用", "睡眠", "总体改善"),
    concept_id = c("COST", "COST", "SLEEP", "PGIC"),
    stringsAsFactors = FALSE
  ))
  review <- data.frame(
    concept_id = c("COST", "SLEEP", "TOOL", "TIME", "PGIC"),
    standardized_name = c("医疗费用", "睡眠质量", "MLHFQ", "30日再住院", "患者总体改善感受"),
    construct = c("out_of_scope", "new_item", "instrument", "timepoint", "new_item"),
    in_scope = c(FALSE, TRUE, TRUE, TRUE, TRUE),
    external_support = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  screen <- screen_new_items(suggestions, review, protocol)
  dec <- setNames(screen$concepts$decision, screen$concepts$concept_id)
  ch <- setNames(screen$concepts$channel, screen$concepts$concept_id)
  expect_equal(unname(dec["COST"]), "exclude")
  expect_equal(screen$concepts$n_independent[screen$concepts$concept_id == "COST"], 2L)
  expect_equal(unname(dec["SLEEP"]), "admit")
  expect_equal(unname(ch["SLEEP"]), "B")
  expect_equal(unname(dec["TOOL"]), "redirect")
  expect_equal(unname(dec["TIME"]), "redirect")
  expect_equal(unname(dec["PGIC"]), "exclude")
  expect_false("COST" %in% screen$admitted$item_id)
})

test_that("coder disagreement stays pending until a third person adjudicates", {
  protocol <- preset_protocol("comet_cos")
  review <- data.frame(
    concept_id = "C1",
    standardized_name = "活动后呼吸困难恢复情况",
    construct = "new_item",
    coder_a = "hierarchy",
    coder_b = "new_item",
    n_independent = 3,
    stringsAsFactors = FALSE
  )
  pending <- screen_new_items(review = review, protocol = protocol)
  expect_equal(pending$concepts$decision, "pending")
  review$adjudicator <- "第三人"
  review$adjudicated_construct <- "new_item"
  decided <- screen_new_items(review = review, protocol = protocol)
  expect_equal(decided$concepts$decision, "admit")
  expect_equal(decided$concepts$channel, "A")
})

test_that("wording keeps the id and substantive modification restarts the scoring clock", {
  items <- delphi_items(data.frame(
    item_id = c("A", "B"),
    item_text = c("运动耐量", "中医症状改善"),
    introduced_round = c(1L, 1L),
    stringsAsFactors = FALSE
  ))
  changed <- classify_modification(
    items,
    data.frame(
      item_id = c("A", "B"),
      modification = c("wording", "substantive"),
      new_text = c("运动能力", "气虚血瘀证证候积分改善"),
      new_item_id = c(NA, "B2"),
      stringsAsFactors = FALSE
    ),
    enter_round = 3L
  )
  expect_equal(changed$items$item_text[changed$items$item_id == "A"], "运动能力")
  expect_equal(changed$items$introduced_round[changed$items$item_id == "A"], 1L)
  expect_false(changed$log$reset_scoring[changed$log$item_id == "A"])
  expect_equal(changed$items$introduced_round[changed$items$item_id == "B2"], 3L)
  expect_equal(changed$items$superseded_by[changed$items$item_id == "B"], "B2")
  expect_true(changed$log$reset_scoring[changed$log$item_id == "B"])
  expect_false("B" %in% .active_item_ids(changed$items))
  expect_true("B2" %in% .active_item_ids(changed$items))
})

test_that("a new item that meets 70/15 on its first scoring stays provisional", {
  protocol <- preset_protocol("comet_cos")
  items <- delphi_items(data.frame(
    item_id = c("OLD", "NEW-01"),
    item_text = c("原条目", "睡眠质量"),
    introduced_round = c(1L, 2L),
    origin = c("initial", "new"),
    stringsAsFactors = FALSE
  ))
  experts <- sprintf("E%02d", 1:10)
  ratings <- delphi_ratings(rbind(
    data.frame(expert_id = experts, item_id = "OLD", round = 1L, score = 8,
               dimension = "重要性", unable = FALSE),
    data.frame(expert_id = experts, item_id = "OLD", round = 2L, score = 8,
               dimension = "重要性", unable = FALSE),
    data.frame(expert_id = experts, item_id = "NEW-01", round = 2L, score = 8,
               dimension = "重要性", unable = FALSE)
  ))
  study <- delphi_study(
    protocol, data.frame(expert_id = experts, stakeholder = "临床"),
    items, ratings, validate = FALSE
  )
  res <- analyze_delphi(study)
  sub <- res$classification
  sub <- sub[sub$dimension == "重要性", ]
  old2 <- sub[sub$item_id == "OLD" & sub$round == 2L, ]
  new2 <- sub[sub$item_id == "NEW-01" & sub$round == 2L, ]
  expect_false(old2$provisional)
  expect_equal(old2$action, "keep")
  expect_true(new2$provisional)
  expect_equal(new2$action, "rescore")
  expect_true(startsWith(new2$class_zh, "暂时"))
  agenda <- consensus_meeting_agenda(res$classification, floor_items = data.frame(
    item_text = "照护者负担", stringsAsFactors = FALSE
  ))
  expect_equal(agenda$band[agenda$item_id == "NEW-01"], "iterate")
  expect_equal(agenda$band[agenda$band == "floor"], "floor")
})

test_that("round-2 questionnaire does not invent statistics for a new item", {
  skip_if_not_installed("openxlsx")
  protocol <- preset_protocol("comet_cos")
  items <- delphi_items(data.frame(
    item_id = c("OLD", "NEW-01"),
    item_text = c("原条目", "睡眠质量"),
    introduced_round = c(1L, 2L),
    origin = c("initial", "new"),
    stringsAsFactors = FALSE
  ))
  experts <- sprintf("E%02d", 1:6)
  ratings <- delphi_ratings(data.frame(
    expert_id = experts, item_id = "OLD", round = 1L, score = 8,
    dimension = "重要性", unable = FALSE
  ))
  study <- delphi_study(
    protocol, data.frame(expert_id = experts),
    items, ratings, validate = FALSE
  )
  res <- suppressWarnings(analyze_delphi(study))
  md <- delphi_questionnaire(res, round = 2, expert_id = "E01", items = "all")
  expect_true(grepl("首次评分", md))
  expect_true(grepl("NEW-01", md))
  new_part <- sub(".*首次评分", "", md)
  expect_false(grepl("组 Median", new_part))
  f <- tempfile(fileext = ".xlsx")
  delphi_questionnaire(res, round = 2, format = "xlsx", file = f, items = "all")
  rat <- openxlsx::read.xlsx(f, sheet = "评分")
  new_rows <- rat[rat$item_id == "NEW-01", ]
  expect_gt(nrow(new_rows), 0)
  expect_true(all(new_rows$first_scoring %in% TRUE))
  expect_true(all(is.na(new_rows$median_iqr) | !nzchar(as.character(new_rows$median_iqr))))
})

test_that("heart-failure case admits exactly three concepts and matches the published bands", {
  s1 <- simulate_chf_new_items(1)
  screen <- screen_new_items(s1$suggestions, s1$concept_review, s1$protocol, s1$items)
  expect_equal(screen$n_admitted, 3L)
  expect_equal(nrow(s1$items), 26L)
  expect_equal(screen$concepts$n_independent[screen$concepts$concept_id == "C-SLEEP"], 1L)
  expect_equal(screen$concepts$channel[screen$concepts$concept_id == "C-SLEEP"], "B")
  expect_equal(screen$concepts$decision[screen$concepts$concept_id == "C-COST"], "exclude")
  expect_equal(screen$concepts$decision[screen$concepts$concept_id == "C-MLHFQ"], "redirect")
  expect_equal(screen$concepts$decision[screen$concepts$concept_id == "C-D30"], "redirect")
  expect_equal(screen$concepts$decision[screen$concepts$concept_id == "C-TONGUE"], "merge")
  expect_equal(screen$concepts$decision[screen$concepts$concept_id == "C-WORD"], "wording")
  expect_true(screen$disposition_complete)
  admitted_ids <- screen$admitted$item_id
  study <- update_delphi_study(simulate_chf_new_items(3), items = screen$admitted)
  expect_equal(nrow(study$items), 29L)
  res2 <- analyze_delphi(study, up_to_round = 2)
  sub <- res2$classification
  sub <- sub[sub$round == 2 & sub$dimension == "重要性", ]
  incon <- sub$item_id %in% admitted_ids & sub$class == "consensus_in"
  expect_true(any(incon))
  expect_true(all(sub$provisional[incon]))
  expect_false(sub$provisional[sub$item_id == "NEW-03"])
  expect_equal(sub$class[sub$item_id == "NEW-03"], "no_consensus")
  expect_true(all(sub$action[incon] == "rescore"))
  res3 <- analyze_delphi(study, up_to_round = 3)
  last <- res3$last_classification
  expect_equal(last$class[last$item_id == "NEW-01"], "consensus_in")
  expect_false(last$provisional[last$item_id == "NEW-01"])
  expect_equal(last$class[last$item_id == "NEW-02"], "consensus_in")
  expect_equal(last$class[last$item_id == "NEW-03"], "no_consensus")
  expect_equal(last$action[last$item_id == "NEW-01"], "keep")
  log <- res3$item_log
  expect_true(any(grepl("医疗费用", log[[4]]) | grepl("C-COST", log[[1]])))
  expect_true(any(grepl("NEW-01", log[[1]])))
})

test_that("new-item rules are off for RAND and on for COMET without marking the preset as modified", {
  comet <- preset_protocol("comet_cos")
  rand <- preset_protocol("rand_ucla")
  expect_false(isTRUE(comet$modified_from_preset))
  expect_true(comet$new_items$allow)
  expect_true(comet$new_items$require_iteration)
  expect_false(rand$new_items$allow)
  expect_false(rand$new_items$require_iteration)
  tab <- protocol_table(comet)
  expect_true("allow_new" %in% tab$parameter)
  expect_false(isTRUE(tab$modified[tab$parameter == "allow_new"]))
})
