vignettes <- c("preset-protocol", "three-round-workflow", "standard-5point-workflow", "new-item-workflow")
guides <- file.path("delphir", "inst", "guides")
doc <- file.path("delphir", "inst", "doc")
dir.create(guides, recursive = TRUE, showWarnings = FALSE)
dir.create(doc, recursive = TRUE, showWarnings = FALSE)

devtools::install(
  "delphir",
  upgrade = FALSE,
  dependencies = FALSE,
  build_vignettes = FALSE,
  quick = TRUE,
  quiet = FALSE
)
library(delphir)
cat("loaded delphir ", as.character(utils::packageVersion("delphir")), "\n", sep = "")

for (v in vignettes) {
  src <- file.path("delphir", "vignettes", paste0(v, ".Rmd"))
  cat("knitting ", v, "\n", sep = "")
  html <- rmarkdown::render(
    src,
    output_dir = normalizePath(guides, winslash = "/", mustWork = TRUE),
    quiet = FALSE,
    envir = new.env(parent = globalenv())
  )
  file.copy(html, file.path(doc, paste0(v, ".html")), overwrite = TRUE)
  file.copy(src, file.path(doc, paste0(v, ".Rmd")), overwrite = TRUE)
  cat("wrote ", html, " size=", file.info(html)$size, "\n", sep = "")
}

cat("KNIT_DONE\n")
