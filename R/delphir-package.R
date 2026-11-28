#' delphir：多场景 Delphi / RAND/UCLA / COMET 分析
#'
#' 以第一轮前锁定的方案执行医学共识、适宜性评价和核心结局集分析，
#' 并生成 ACCORD、DCAT、CREDES 对照报告。
#'
#' @section 从这里开始:
#' * [delphi_guides()] 打开七套随包教学文档
#' * [preset_protocol()] / [protocol_table()] 选择场景并锁定、打印阈值
#' * [delphi_blueprint()] / [delphi_questionnaire()] / [read_survey_xlsx()]
#'   第 1 轮及后续轮次 docx/xlsx 问卷与回读
#' * [next_round_questionnaire()] 按分析结果生成下一轮
#' * [score_authority()] / [expert_authority()] 由自评勾选计算 Ca、Cs、Cr
#' * [preset_protocol()] 的 `standard_element`：5 分核心技术要素
#' * [simulate_delphi()] / [simulate_standard_delphi()] / [simulate_rand_ucla()] / [simulate_cos()] 生成示例
#' * [analyze_delphi()] / [run_delphi()] 全流程分析与可选完整报告
#' * [delphi_report()] 输出中间报告或 ACCORD 终稿（html / docx）
#'
#' @keywords internal
"_PACKAGE"
