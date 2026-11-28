# delphir

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-blue.svg)](https://cran.r-project.org/)
[![R-CMD-check](https://github.com/yangpluszhu/delphir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yangpluszhu/delphir/actions/workflows/R-CMD-check.yaml)
[![Release](https://img.shields.io/github/v/release/yangpluszhu/delphir?include_prereleases)](https://github.com/yangpluszhu/delphir/releases)

**Protocol-first toolkit for Delphi studies, RAND/UCLA appropriateness panels, and COMET core outcome sets.**

以第一轮前锁定的方案，完成医学专家共识、适宜性评价、核心结局集，以及标准核心技术要素（5 分量表）的分析。第一轮新增条目按双通道审查，并要求再迭代一轮。自动生成问卷并回读，写出 Word 报告。安装后即获得十一套中文教学文档、问卷样稿与示例数据。RAND/UCLA 默认按手册表 4 判定，并可生成两轮 docx/xlsx 问卷与末轮 Word 报告。

- **Repository:** https://github.com/yangpluszhu/delphir
- **Issues:** https://github.com/yangpluszhu/delphir/issues
- **Author:** Ming Yang (`yangpluszhu@sina.com`)
- **License:** MIT

---

## Why delphir

Delphi 不是普通专家问卷。本包把下列方法学约束写进分析流程，而不是事后用百分比“凑共识”：

- 共识阈值必须在第一轮前写入 `delphi_protocol()`；分析函数不会根据结果回改。
- Likert 主指标为 **Median、IQR、高/低区比例**；均数、CV、Kendall W、Wilcoxon 仅为补充。
- 「无法判断 / 超出专业领域」不进入 1–9 分母。
- 稳定的分歧应报告为未达共识，而不是继续投票。
- 专家高度一致不等于高质量证据；证据等级与共识分类分列报告。
- 权威系数 \(Cr=(Ca+Cs)/2\) 仅供国内发表补充，**不是** DCAT 核心要求：按专家评一次，不按条目计算，不用于加权。

报告结构对照 [ACCORD (2024)](https://www.equator-network.org/reporting-guidelines/accord/)、CREDES 与 [DCAT (BMJ 2025)](https://www.bmj.com/content/388/bmj-2024-082006)。

## Installation

需要 R >= 4.1，以及 `ggplot2`、`rmarkdown`、`knitr`、`openxlsx`、`officer`。

```r
install.packages(c("ggplot2", "rmarkdown", "knitr", "openxlsx", "officer"))

# 推荐：从 GitHub 安装
install.packages("remotes")
remotes::install_github("yangpluszhu/delphir")

# 或使用 pak
# install.packages("pak")
# pak::pak("yangpluszhu/delphir")
```

若已下载源码包：

```r
install.packages("delphir_0.5.0.tar.gz", repos = NULL, type = "source")
```

本地开发安装（含 vignette）：

```r
# 在包根目录的上一级
devtools::install("delphir", build_vignettes = TRUE)
```

不编译 vignette 时，仍可用预置 HTML：

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

内置教学数据：

```r
data(consensus_demo)         # 医学专家共识（两轮）
data(consensus_3round_demo)  # 医学专家共识（三轮）
data(standard_element_demo)  # 5 分核心技术要素（开放轮 + 三轮评分）
data(chf_new_item_demo)      # 心衰核心指标集：第一轮新增条目（三轮）
data(rand_ucla_demo)         # RAND/UCLA 适宜性
data(cos_demo)               # COMET 核心结局集
data(indicator_demo)         # 指标体系补充示例
```

从空白方案到三轮问卷（docx + xlsx）与终稿：

```r
protocol <- preset_protocol("clinical_consensus")
bp <- delphi_blueprint(protocol, panel, items, meta = meta)
delphi_questionnaire(bp, round = 1, format = "both", file = "r1")
study1 <- read_survey_xlsx("r1", study = bp)
res1 <- run_delphi(study1, round = 1, report = TRUE, file = "round1.docx")
next_round_questionnaire(res1, format = "both", file = "r2")
# 第 2、第 3 轮同样回读后：
res <- run_delphi(study3, report = TRUE, format = "both", file = "final")
```

## Features

| 场景 | 入口 | 要点 |
|---|---|---|
| 医学专家共识 | `preset_protocol("clinical_consensus")` | 9 分 Likert；高度/基本共识、排除、两极分化 |
| RAND/UCLA 适宜性 | `simulate_rand_ucla()` / `classify_rand()` | Median 区间 + 分歧指数，不强迫共识 |
| COMET 核心结局集 | `preset_protocol("comet_cos")` | 70/15 规则；利益相关者分层 |
| 标准核心技术要素 | `preset_protocol("standard_element")` | 5 分；均数与满分比；协调系数决定是否结束 |
| 第一轮新增条目 | `screen_new_items()` / `simulate_chf_new_items()` | 五问与双通道；首次评分只记暂时共识；变更日志 |
| 问卷全流程 | `delphi_questionnaire()` / `read_survey_xlsx()` / `next_round_questionnaire()` | 开放式首轮或评分轮；docx/xlsx；本人上轮分数与组结果 |
| 权威系数 | `score_authority()` / `expert_authority()` | 曾光量表；专家级 Ca、Cs、Cr |
| 报告 | `run_delphi()` / `delphi_report()` | 指定轮次全流程；html/docx；含方案、各轮问卷与参考文献 |

主要分析函数：`analyze_delphi()`、`summarize_items()`、`classify_consensus()`、`prepare_feedback()`、`analyze_stability()`、`stakeholder_stratify()`、`dropout_bias()`、`accord_checklist()`、`dcat_checklist()`、`credes_checklist()`。

函数帮助均为中文，含输入输出格式与可运行示例：安装后 `?analyze_delphi`。

## Vignettes

| 文档 | 打开方式 |
|---|---|
| 1. 医学专家共识 Modified Delphi + ACCORD | `vignette("clinical-consensus", package = "delphir")` |
| 2. RAND/UCLA 适宜性评价 | `vignette("rand-ucla", package = "delphir")` |
| 3. COMET 核心结局集 70/15 | `vignette("comet-cos", package = "delphir")` |
| 4. 从第 1 轮问卷设计到方法学报告 | `vignette("questionnaire-workflow", package = "delphir")` |
| 5. 预设方案参数详解 | `vignette("preset-protocol", package = "delphir")` |
| 6. 三轮 Delphi 从零到完整报告 | `vignette("three-round-workflow", package = "delphir")` |
| 7. 5 分量表核心技术要素，从开放式首轮到完整报告 | `vignette("standard-5point-workflow", package = "delphir")` |
| 8. 第一轮新增条目，从问卷到三轮报告 | `vignette("new-item-workflow", package = "delphir")` |

或 `delphi_guide("clinical-consensus")` 等打开预编译 HTML。

## Project layout

```
delphir/
├── DESCRIPTION          # 包元数据（作者、依赖、URL）
├── LICENSE / LICENSE.md
├── NEWS.md
├── R/                   # 分析、问卷、报告函数
├── man/                 # roxygen 帮助文档
├── vignettes/           # 十一套教学文档源稿
├── inst/
│   ├── questionnaires/  # 第 1/2/3 轮问卷样稿
│   ├── extdata/         # 录入模板与迷你示例
│   ├── guides/          # 预编译教学 HTML
│   └── rmarkdown/       # 报告模板
├── data/                # 教学数据集
└── tests/               # testthat
```

## Citing

若本包对研究有帮助，请引用软件本身，并同时引用所遵循的报告规范（ACCORD、DCAT、CREDES）。

```r
citation("delphir")
```

GitHub 仓库页的 **Cite this repository** 使用根目录 `CITATION.cff`。

## Contributing

Issues 与功能建议请提交到 [GitHub Issues](https://github.com/yangpluszhu/delphir/issues)。

提交代码前请：

1. 用 `devtools::document()` 更新帮助；
2. 运行 `devtools::test()`；
3. 不要根据分析结果回改共识阈值；
4. 不要把 Cr 做成评分权重。

## Changelog

见 [NEWS.md](NEWS.md)。当前版本 **0.5.0**。

## License

[MIT](LICENSE) © 2026 Ming Yang
