devtools::load_all("delphir", quiet = TRUE)
consensus_3round_demo <- simulate_delphi(
  n_experts = 24, n_items = 12, n_rounds = 3, seed = 20260917
)
consensus_3round_demo$meta$title <- "药物临床试验质量源于设计实施专家共识（三轮教学数据）"
dir.create("delphir/data", showWarnings = FALSE)
save(consensus_3round_demo, file = "delphir/data/consensus_3round_demo.rda", compress = "xz")
cat(
  "class=", paste(class(consensus_3round_demo), collapse = ","),
  " n_ratings=", nrow(consensus_3round_demo$ratings),
  " rounds=", paste(sort(unique(consensus_3round_demo$ratings$round)), collapse = ","),
  "\n", sep = ""
)
