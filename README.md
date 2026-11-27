# delphir

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-blue.svg)](https://cran.r-project.org/)
[![R-CMD-check](https://github.com/yangpluszhu/delphir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yangpluszhu/delphir/actions/workflows/R-CMD-check.yaml)
[![Release](https://img.shields.io/github/v/release/yangpluszhu/delphir?include_prereleases)](https://github.com/yangpluszhu/delphir/releases)

**Protocol-first toolkit for Delphi studies, RAND/UCLA appropriateness panels, and COMET core outcome sets.**

`delphir` runs the full analysis pipeline for medical expert consensus, appropriateness
evaluation, core outcome sets, and standard core technical elements (5-point scales)
under a protocol that is locked **before** round 1. First-round new items are screened
through a pre-specified dual pathway and must survive one additional iteration.
Questionnaires are generated and read back automatically (Word + Excel), and reports are
written as Word/HTML documents aligned with ACCORD, DCAT and CREDES. RAND/UCLA panels
follow the manual table 4 by default and support two-round docx/xlsx questionnaires plus
a final-round Word report. A separate modified-Delphi scenario locks table 4, IPRAS or a
custom percent rule, generates multi-round questionnaires and writes a docx report.
Installing the package also ships fourteen Chinese teaching guides, questionnaire
samples and example datasets.

- **Repository:** https://github.com/yangpluszhu/delphir
- **Issues:** https://github.com/yangpluszhu/delphir/issues
- **Author:** Ming Yang (`yangpluszhu@sina.com`)
- **License:** MIT

---

## Why delphir

A Delphi study is not an ordinary expert survey. This package encodes the following
methodological constraints into the analysis pipeline instead of reverse-engineering
"consensus" from percentages after the fact:

- Consensus thresholds must be written into `delphi_protocol()` before round 1; analysis
  functions never back-fit them to the observed results.
- Primary Likert statistics are **median, IQR, and high/low region proportions**; means,
  CV, Kendall's W and Wilcoxon tests are supplementary only.
- "Unable to judge / outside expertise" responses are excluded from the 1–9 denominator.
- Persistent disagreement is reported as *no consensus*, not resolved by endless voting.
- High expert agreement is not high-quality evidence; evidence level and consensus class
  are reported separately.
- The authority coefficient Cr = (Ca + Cs)/2 is only a supplement for domestic
  (Chinese) publications and is **not** a DCAT core requirement: it is scored once per
  expert, never per item, and never used for weighting.

Report structures follow [ACCORD (2024)](https://www.equator-network.org/reporting-guidelines/accord/),
CREDES and [DCAT (BMJ 2025)](https://www.bmj.com/content/388/bmj-2024-082006).

## Installation

Requires R >= 4.1 and the packages `ggplot2`, `rmarkdown`, `knitr`, `openxlsx` and
`officer`.

```r
install.packages(c("ggplot2", "rmarkdown", "knitr", "openxlsx", "officer"))

# Recommended: install from GitHub
install.packages("remotes")
remotes::install_github("yangpluszhu/delphir")

# Or use pak
# install.packages("pak")
# pak::pak("yangpluszhu/delphir")
```

If you have downloaded the source package:

```r
install.packages("delphir_0.9.0.tar.gz", repos = NULL, type = "source")
```

For local development (including vignettes):

```r
# From the parent directory of the package root
devtools::install("delphir", build_vignettes = TRUE)
```

Without building vignettes you can still open the precompiled HTML guides:

```r
library(delphir)
delphi_guides()
delphi_guide("questionnaire-workflow")
```

## Quick start

```r
library(delphir)

protocol <- preset_protocol("clinical_consensus")
study <- simulate_delphi(n_experts = 20, n_items = 10, seed = 1)
res <- analyze_delphi(study)
delphi_report(res, file = "consensus-report.html")
```

Built-in teaching datasets:

```r
data(consensus_demo)          # clinical consensus (two rounds)
data(consensus_3round_demo)   # clinical consensus (three rounds)
data(standard_element_demo)   # 5-point core technical elements (open round + 3 rating rounds)
data(chf_new_item_demo)       # heart-failure COS: first-round new items (three rounds)
data(rand_ucla_demo)          # RAND/UCLA appropriateness
data(ram_classic_demo)        # RAND/UCLA classic two-round appropriateness
data(ram_necessity_demo)      # RAND/UCLA necessity ratings
data(ram_recommendation_demo) # RAND/UCLA rule comparison / recommendations
data(delphi_ram_table4_demo)  # modified Delphi + table 4
data(delphi_ram_ipras_demo)   # modified Delphi + IPRAS
data(delphi_ram_percent_demo) # modified Delphi + percent consensus
data(cos_demo)                # COMET core outcome set
data(indicator_demo)          # indicator-system supplement
```

From a blank protocol to three-round questionnaires (docx + xlsx) and the final report:

```r
protocol <- preset_protocol("clinical_consensus")
bp <- delphi_blueprint(protocol, panel, items, meta = meta)
delphi_questionnaire(bp, round = 1, format = "both", file = "r1")
study1 <- read_survey_xlsx("r1", study = bp)
res1 <- run_delphi(study1, round = 1, report = TRUE, file = "round1.docx")
next_round_questionnaire(res1, format = "both", file = "r2")
# After reading back rounds 2 and 3 the same way:
res <- run_delphi(study3, report = TRUE, format = "both", file = "final")
```

## Features

| Scenario | Entry point | Highlights |
|---|---|---|
| Clinical consensus | `preset_protocol("clinical_consensus")` | 9-point Likert by default; `points = 5` or `7` fills bands and thresholds |
| RAND/UCLA appropriateness | `simulate_rand_ucla()` / `classify_rand()` | Median intervals + disagreement index; no forced consensus |
| Modified Delphi + RAM | `delphi_ram_protocol()` / `run_delphi_ram()` | Table 4, IPRAS or custom percent; docx/xlsx questionnaires |
| COMET core outcome set | `preset_protocol("comet_cos")` | 70/15 rule; stakeholder stratification |
| Standard core technical elements | `preset_protocol("standard_element")` | 5-point scale; mean and full-score ratio; coordination coefficient decides stopping |
| First-round new items | `screen_new_items()` / `simulate_chf_new_items()` | Five-question screen + dual pathway; first scoring only earns provisional consensus; changelog |
| Questionnaire workflow | `delphi_questionnaire()` / `read_survey_xlsx()` / `next_round_questionnaire()` | Open first round or scoring rounds; docx/xlsx; own previous score and group feedback |
| Authority coefficient | `score_authority()` / `expert_authority()` | Zeng Guang scale; expert-level Ca, Cs, Cr |
| Reporting | `run_delphi()` / `delphi_report()` | Round-by-round workflow; html/docx; protocol, questionnaires and references included |

Main analysis functions: `analyze_delphi()`, `summarize_items()`, `classify_consensus()`,
`prepare_feedback()`, `analyze_stability()`, `stakeholder_stratify()`, `dropout_bias()`,
`accord_checklist()`, `dcat_checklist()`, `credes_checklist()`.

Supplementary indicators (never used to classify items under the locked rules and never
used for weighting): response rate with reference lines, protocol-level Ca/Cs scoring
tables, full-score ratio, coefficient of variation, IPR, and Kendall's W for the final
and every round (with tie correction and chi-square test). Rater reliability adds
Cronbach's alpha, ICC(2,1) and ICC(2,k), plus Krippendorff's alpha when `irr` is
installed (`rater_reliability()`). `compare_rounds()` reports item-level median, IPR,
class and class changes, plus group-level consensus rate, mean IPR and Kendall's W
shifts (Wilcoxon signed-rank tests are exploratory descriptions only).
`threshold_sensitivity()` stress-tests consensus thresholds without replacing the
locked rules. `report_extended_section()`, `report_rounds_section()` and
`report_sensitivity_section()` weave these results into the five report templates with
explicit exploratory caveats.

Function help pages are written in Chinese and include input/output formats and runnable
examples: after installation, see `?analyze_delphi`.

## Vignettes

| # | Guide | Open with |
|---|---|---|
| 1 | Modified Delphi clinical consensus + ACCORD | `vignette("clinical-consensus", package = "delphir")` |
| 2 | RAND/UCLA appropriateness evaluation | `vignette("rand-ucla", package = "delphir")` |
| 3 | COMET core outcome set 70/15 | `vignette("comet-cos", package = "delphir")` |
| 4 | From round-1 questionnaire design to the methods report | `vignette("questionnaire-workflow", package = "delphir")` |
| 5 | Preset protocol parameters explained | `vignette("preset-protocol", package = "delphir")` |
| 6 | Three-round Delphi from scratch to a full report | `vignette("three-round-workflow", package = "delphir")` |
| 7 | 5-point core technical elements, from an open round to a full report | `vignette("standard-5point-workflow", package = "delphir")` |
| 8 | First-round new items, from questionnaire to a three-round report | `vignette("new-item-workflow", package = "delphir")` |
| 9 | Classic RAND two-round appropriateness | `vignette("rand-classic-workflow", package = "delphir")` |
| 10 | Necessity evaluation | `vignette("rand-necessity-workflow", package = "delphir")` |
| 11 | Rule comparison and modified recommendations | `vignette("rand-rules-workflow", package = "delphir")` |
| 12 | Modified Delphi with table 4 | `vignette("delphi-ram-table4", package = "delphir")` |
| 13 | Modified Delphi with IPRAS | `vignette("delphi-ram-ipras", package = "delphir")` |
| 14 | Modified Delphi with custom percent consensus | `vignette("delphi-ram-percent", package = "delphir")` |

Or open the precompiled HTML with `delphi_guide("clinical-consensus")` and friends. The
guide texts are written in Chinese.

## Project layout

```
delphir/
├── DESCRIPTION          # package metadata (authors, dependencies, URLs)
├── LICENSE / LICENSE.md
├── NEWS.md
├── R/                   # analysis, questionnaire and report functions
├── man/                 # roxygen help pages
├── vignettes/           # fourteen teaching guides (sources)
├── inst/
│   ├── questionnaires/  # round 1/2/3 questionnaire samples
│   ├── extdata/         # data-entry templates and mini examples
│   ├── guides/          # precompiled teaching HTML
│   └── rmarkdown/       # report templates
├── data/                # teaching datasets
└── tests/               # testthat
```

## Citing

If this package helps your research, please cite the software itself and the reporting
guidelines you follow (ACCORD, DCAT, CREDES).

```r
citation("delphir")
```

The **Cite this repository** button on the GitHub page uses the root `CITATION.cff`.

## Contributing

Please file issues and feature requests on
[GitHub Issues](https://github.com/yangpluszhu/delphir/issues).

Before submitting code:

1. Refresh the help pages with `devtools::document()`;
2. Run `devtools::test()`;
3. Never back-fit consensus thresholds to the observed results;
4. Never turn Cr into a weighting factor for ratings.

## Changelog

See [NEWS.md](NEWS.md). Current version: **0.9.0**.

## License

[MIT](LICENSE) © 2026 Ming Yang
