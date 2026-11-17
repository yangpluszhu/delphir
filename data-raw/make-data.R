# Generate package datasets
pkg <- "delphir"
devtools::load_all(pkg, quiet = TRUE)

consensus_demo <- simulate_delphi(
  n_experts = 40, n_items = 20, n_rounds = 2, seed = 20260906
)
rand_ucla_demo <- simulate_rand_ucla(
  n_experts = 9, n_items = 24, n_rounds = 2, seed = 20260906
)
ram_classic_demo <- simulate_ram_classic()
ram_necessity_demo <- simulate_rand_necessity()
ram_recommendation_demo <- simulate_rand_recommendation()
cos_demo <- simulate_cos(
  n_clinicians = 40, n_patients = 25, n_researchers = 15,
  n_items = 25, n_rounds = 2, seed = 20260906
)

indicator_demo <- simulate_delphi(
  n_experts = 24, n_items = 12, n_rounds = 2, seed = 20260907
)
indicator_demo$protocol <- preset_protocol("indicator")
indicator_demo$meta$title <- "临床研究机构质量评价指标体系（模拟）"
indicator_demo$meta$objective <- "筛选可操作的质量指标，权重另用 AHP 确定。"

consensus_3round_demo <- simulate_delphi(
  n_experts = 24, n_items = 12, n_rounds = 3, seed = 20260917
)
consensus_3round_demo$meta$title <- "药物临床试验质量源于设计实施专家共识（三轮教学数据）"

save(consensus_demo, file = file.path(pkg, "data", "consensus_demo.rda"), compress = "xz")
save(rand_ucla_demo, file = file.path(pkg, "data", "rand_ucla_demo.rda"), compress = "xz")
save(ram_classic_demo, file = file.path(pkg, "data", "ram_classic_demo.rda"), compress = "xz")
save(ram_necessity_demo, file = file.path(pkg, "data", "ram_necessity_demo.rda"), compress = "xz")
save(ram_recommendation_demo, file = file.path(pkg, "data", "ram_recommendation_demo.rda"), compress = "xz")
save(cos_demo, file = file.path(pkg, "data", "cos_demo.rda"), compress = "xz")
save(indicator_demo, file = file.path(pkg, "data", "indicator_demo.rda"), compress = "xz")
save(consensus_3round_demo, file = file.path(pkg, "data", "consensus_3round_demo.rda"), compress = "xz")

standard_element_demo <- simulate_standard_delphi(seed = 2026)
save(standard_element_demo, file = file.path(pkg, "data", "standard_element_demo.rda"), compress = "xz")

chf_new_item_demo <- simulate_chf_new_items(up_to_round = 3)
save(chf_new_item_demo, file = file.path(pkg, "data", "chf_new_item_demo.rda"), compress = "xz")

delphi_ram_table4_demo <- simulate_delphi_ram("table4")
delphi_ram_ipras_demo <- simulate_delphi_ram("ipras")
delphi_ram_percent_demo <- simulate_delphi_ram("percent")
save(delphi_ram_table4_demo, file = file.path(pkg, "data", "delphi_ram_table4_demo.rda"), compress = "xz")
save(delphi_ram_ipras_demo, file = file.path(pkg, "data", "delphi_ram_ipras_demo.rda"), compress = "xz")
save(delphi_ram_percent_demo, file = file.path(pkg, "data", "delphi_ram_percent_demo.rda"), compress = "xz")

# sample CSV for inst/extdata
utils::write.csv(consensus_demo$ratings, "delphir/inst/extdata/consensus_ratings.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(consensus_demo$panel, "delphir/inst/extdata/consensus_panel.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(consensus_demo$items, "delphir/inst/extdata/consensus_items.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")

cat("datasets ok\n")
cat("consensus experts", nrow(consensus_demo$panel), "items", nrow(consensus_demo$items), "\n")
