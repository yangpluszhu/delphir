# delphir 0.1.0

* 首次发布：多场景 Delphi 分析、RAND/UCLA 分歧指数、COMET 70/15、
  受控反馈、ACCORD/DCAT/CREDES 报告与三套中文教学文档。

# delphir 0.2.0

* 权威系数可从问卷勾选赋分：`authority_scales()`、`score_cs()`、`score_ca()`、
  `score_authority()`。Ca/Cs 按专家计算一次；逐条输入会报错。
* `simulate_delphi()` 改为生成熟悉程度与判断依据原始列再赋分。
* 随包第 1/2 轮问卷样稿、空白 CSV 与迷你录入示例；新增
  `write_survey_templates()`、`delphi_questionnaire()`。
* `delphi_report()` 支持 `stage = "round1"` 中间报告与 `"final"` 终稿；
  临床模板补充赋分表与第 2 轮动作。
* 新增教学文档 4：`vignette("questionnaire-workflow")`。
