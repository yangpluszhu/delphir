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

#' 5 分核心技术要素教学数据
#'
#' 按技术要求生成的模拟研究：36 名高级职称专家，第 1 轮开放式征询，
#' 第 2 至第 4 轮为重要程度与熟悉程度的 5 分评分。供
#' `vignette("standard-5point-workflow", package = "delphir")` 与帮助页示例使用。
#' 数据是教学模拟，不能当作真实标准条款。
#'
#' @format `delphi_study` 对象。`meta$initial_items` 为开放式轮次之前的 8 条要素。
#' @seealso [simulate_standard_delphi()], [appendix_b_table()]
#' @examples
#' data(standard_element_demo, package = "delphir")
#' sort(unique(standard_element_demo$ratings$round))
"standard_element_demo"

#' 心衰核心指标集新增条目教学数据
#'
#' 30 名专家、初始 26 项、三轮 9 分评分。第一轮开放建议经双通道审查后，
#' 3 项进入后续轮次。第 2 轮这三个条目是首次评分。供
#' `vignette("new-item-workflow", package = "delphir")` 使用。
#' 数据为教学模拟。
#'
#' @format `delphi_study` 对象，场景 `comet_cos`。
#' @seealso [simulate_chf_new_items()], [screen_new_items()]
#' @examples
#' data(chf_new_item_demo, package = "delphir")
#' sort(unique(chf_new_item_demo$ratings$round))
"chf_new_item_demo"
