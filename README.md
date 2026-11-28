# delphir

方案锁定后的 Delphi / RAND/UCLA / COMET 分析与报告工具。安装后即获得三套从零开始的教学文档和配套样本数据。

## 安装

```r
# 在包根目录的上一级
install.packages("devtools")
devtools::install("delphir", build_vignettes = TRUE)
```

若本机暂不编译 vignette，仍可通过预置 HTML 打开教程：

```r
library(delphir)
delphi_guides()
delphi_guide("clinical-consensus")
```

## 最短流程

```r
library(delphir)

protocol <- preset_protocol("clinical_consensus")
study <- simulate_delphi(n_experts = 20, n_items = 10, seed = 1)
res <- analyze_delphi(study)
delphi_report(res, file = "consensus-report.html")
```

内置教学数据：

```r
data(consensus_demo)   # 医学专家共识
data(rand_ucla_demo)   # RAND/UCLA 适宜性
data(cos_demo)         # COMET 核心结局集
data(indicator_demo)   # 指标体系补充示例
```

## 教学文档

| 文档 | 打开方式 |
|---|---|
| 医学专家共识 Modified Delphi + ACCORD | `vignette("clinical-consensus", package = "delphir")` |
| RAND/UCLA 适宜性评价 | `vignette("rand-ucla", package = "delphir")` |
| COMET 核心结局集 70/15 | `vignette("comet-cos", package = "delphir")` |

函数说明均为中文，含输入输出格式与可运行示例，安装后 `?analyze_delphi` 即可阅读。

## 方法学声明

- 共识阈值必须在第一轮前写入 `delphi_protocol()`，分析函数不会根据结果回改。
- Likert 主指标为 Median、IQR 与高/低区比例；Mean、CV、Kendall W、Wilcoxon 仅为补充。
- “无法判断”不进入 1–9 分母。
- 稳定的分歧应报告为未达共识，而不是继续投票。
- 专家高度一致不等于高质量证据。
- 权威系数 Cr 仅供国内发表补充，不是 DCAT 核心要求。

报告结构对照 ACCORD（2024）、CREDES 与 DCAT（BMJ 2025）。
