# delphir 0.6.0

* RAND/UCLA 预设规则改为 Fitch 等（2001）表 4，按实际应答人数查一致与分歧阈值。
  旧名 `classic` 也改为表 4，不再使用 `ceiling(n/3)`。人数为 10 或 16 时，
  与 0.5.0 的分歧判定可能不同。`both` 仍表示表 4 与 IPRAS 任一判分歧即分歧。
* IPRAS 的分歧改为 IPR 严格大于 IPRAS。分位数固定 type 7。改动 2.35、1.5
  或分位类型时，报告标为非标准参数。
* 适宜性默认最多 2 轮。不确定拆成「中位数 4–6」和「存在分歧」。中位数
  3.5 或 6.5 默认归不确定。超过 2 轮的适宜性评分会警告，并在报告中写成偏离。
* 新增必要性评价：只对适宜且无分歧的条目分类（Kahan 等，1994）。
  百分比规则仅用于推荐意见，并标明 RAM 修改版。
* 新增 `rand_questionnaire()`：第 1 轮独立评分，第 2 轮反馈后复评全部条目，
  可另生成必要性问卷。输出 docx 与 xlsx。xlsx 可用 `read_survey_xlsx()` 读回。
* 新增 `run_rand()`：末轮完成分类、轮次比较和规则敏感性。是否写报告、
  格式和路径由使用者选择；选择 html 时仍另写 docx。
* 新增 `rand_compare_rounds()`、`rand_sensitivity()`、`rand_panel_thresholds()`、
  `classify_necessity()` 和四幅 RAND 图形。
* 新增教学文档 9–11 与数据 `ram_classic_demo`、`ram_necessity_demo`、
  `ram_recommendation_demo`。安装后随 `delphi_guides()` 提供。

# delphir 0.5.0

* 第一轮新增条目按事先锁定的规则审查：五问、双通道（至少 2 名专家独立提出，
  或 1 名提出且有外部证据）、范围优先于人数。同一人重复书写只计 1 人。
* 新条目在下一轮标为首次评分，不编造上轮统计。只评过一次即使达到纳入线，
  也记为暂时共识，必须再迭代一轮。文字修改不重置轮次，实质性修改按新版本重评。
* 新增 `screen_new_items()`、`mark_iteration()`、`classify_modification()`、
  `item_changelog()`、`consensus_meeting_agenda()`、`audit_new_items()`
  与 `simulate_chf_new_items()`。问卷增加结构化开放题；第 3 轮可用
  `items = "iteration"`。
* 报告写入审查去向、变更日志和会议分组。会上临时提出、从未匿名评分的条目
  不得当场纳入。
* 新增教学文档 8：心衰核心指标集从第 1 轮问卷到三轮 docx 报告。
  9 分共识、RAND/UCLA 与 5 分核心技术要素的原有判定不变。

# delphir 0.4.0

* 新增场景 `standard_element`：5 分量表的核心技术要素。重要性以均数判定，
  报告调查人数、均值和满分比；开放式首轮、重要程度与熟悉程度分列；
  按 Kendall 协调系数、卡方检验和均数变化决定是否结束（最多 4 轮）。
* 新增 `full_score_ratio()`、`expert_profile()`、`panel_compliance()`、
  `coordination_decision()`、`appendix_b_table()`、`analyze_open_round()`、
  `simulate_standard_delphi()` 与教学数据 `standard_element_demo`。
* 新增报告模板与教学文档 7：从开放式问卷到完整 docx 报告。
  9 分医学共识、RAND/UCLA 与 COMET 的判定规则不变。

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
