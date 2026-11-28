#' 医学专家共识教学数据
#'
#' 由 [simulate_delphi()] 生成的两轮 Modified Delphi 研究，含脱落、无法判断、
#' 高度共识、临界、两极分化与自由意见，供第一套教学文档使用。
#'
#' @format `delphi_study` 对象。
#' @seealso [simulate_delphi()], vignette("clinical-consensus", package = "delphir")
#' @examples
#' data(consensus_demo, package = "delphir")
#' consensus_demo
"consensus_demo"

#' RAND/UCLA 适宜性教学数据
#'
#' 9 人面板对多项临床指征的两轮适宜性评分，含适宜、不适宜、不确定与分歧。
#'
#' @format `delphi_study` 对象。
#' @seealso [simulate_rand_ucla()], vignette("rand-ucla", package = "delphir")
#' @examples
#' data(rand_ucla_demo, package = "delphir")
#' rand_ucla_demo
"rand_ucla_demo"

#' COMET 核心结局集教学数据
#'
#' 医生、患者、研究者三组对候选结局的两轮 1–9 重要性评分，含“总体过线但
#' 患者组未过线”的条目。
#'
#' @format `delphi_study` 对象。
#' @seealso [simulate_cos()], vignette("comet-cos", package = "delphir")
#' @examples
#' data(cos_demo, package = "delphir")
#' cos_demo
"cos_demo"

#' 指标体系补充示例数据
#'
#' 多维度重要性评分，供 [kendall_w()] 与 [ahp_weights()] 示例使用。
#'
#' @format `delphi_study` 对象，场景为 `indicator`。
#' @examples
#' data(indicator_demo, package = "delphir")
#' indicator_demo$protocol$scenario
"indicator_demo"

#' 三轮医学专家共识教学数据
#'
#' 由 [simulate_delphi()] 生成的三轮 Modified Delphi，供
#' `vignette("three-round-workflow", package = "delphir")` 使用。含脱落、
#' 无法判断与轮次间收敛，计划轮次与预设 `max_rounds = 3` 对齐。
#'
#' @format `delphi_study` 对象。
#' @seealso [simulate_delphi()], [run_delphi()]
#' @examples
#' data(consensus_3round_demo, package = "delphir")
#' unique(consensus_3round_demo$ratings$round)
"consensus_3round_demo"
