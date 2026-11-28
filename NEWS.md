# delphir 0.3.0

* `preset_protocol()` 按场景填入默认阈值；新增 `protocol_table()` 与
  `vignette("preset-protocol")`，逐参数解读并用实例说明改阈值的后果。
* 问卷支持不少于 3 轮，输出 markdown/html/docx/xlsx（`format = "both"`）。
  新增 `delphi_blueprint()`、`read_survey_xlsx()`、`update_delphi_study()`、
  `next_round_questionnaire()`。
* `analyze_delphi(up_to_round)` 与 `run_delphi()`：指定轮次全流程，可选输出
  完整报告（必须能写 docx）。报告补全方案参数、各轮问卷、逐轮结果与参考文献。
* 新增三轮教学数据 `consensus_3round_demo` 与
  `vignette("three-round-workflow")`。

# delphir 0.2.0

* 权威系数可从问卷勾选赋分：`authority_scales()`、`score_cs()`、`score_ca()`、
  `score_authority()`。Ca/Cs 按专家计算一次；逐条输入会报错。
* `simulate_delphi()` 改为生成熟悉程度与判断依据原始勾选再赋分。
* 随包第 1/2 轮问卷样稿、空白 CSV 与迷你录入示例；新增
  `write_survey_templates()`、`delphi_questionnaire()`。
* `delphi_report()` 支持 `stage = "round1"` 中间报告与 `"final"` 终稿；
  临床模板补充赋分表与第 2 轮动作。
* 新增教学文档 4：`vignette("questionnaire-workflow")`。

# delphir 0.1.0

* 首次发布：多场景 Delphi 分析、RAND/UCLA 分歧指数、COMET 70/15、
  受控反馈、ACCORD/DCAT/CREDES 报告与三套中文教学文档。
