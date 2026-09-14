# Generate package datasets
pkg <- "delphir"
devtools::load_all(pkg, quiet = TRUE)

consensus_demo <- simulate_delphi(
  n_experts = 40, n_items = 20, n_rounds = 2, seed = 20260906
)
rand_ucla_demo <- simulate_rand_ucla(
  n_experts = 9, n_items = 24, n_rounds = 2, seed = 20260906
)
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

save(consensus_demo, file = file.path(pkg, "data", "consensus_demo.rda"), compress = "xz")
save(rand_ucla_demo, file = file.path(pkg, "data", "rand_ucla_demo.rda"), compress = "xz")
save(cos_demo, file = file.path(pkg, "data", "cos_demo.rda"), compress = "xz")
save(indicator_demo, file = file.path(pkg, "data", "indicator_demo.rda"), compress = "xz")

# sample CSV for inst/extdata
utils::write.csv(consensus_demo$ratings, "delphir/inst/extdata/consensus_ratings.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(consensus_demo$panel, "delphir/inst/extdata/consensus_panel.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(consensus_demo$items, "delphir/inst/extdata/consensus_items.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")

cat("datasets ok\n")
cat("consensus experts", nrow(consensus_demo$panel), "items", nrow(consensus_demo$items), "\n")
