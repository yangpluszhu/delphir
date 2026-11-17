#' 评分者信度（补充指标）
#'
#' 在完整病例上计算 Cronbach's \eqn{\alpha}、ICC(2,1) 与 ICC(2,k)。
#' 若已安装建议包 `irr`，同时计算 Krippendorff's \eqn{\alpha}（有序距离）。
#' 这些指标把有序评分当作等距或有序测量的补充描述，不决定单条去留，也不加权。
#'
#' @param ratings 评分长表、[delphi_study()] 或 [analyze_delphi()] 结果。
#' @param expert_col,item_col,score_col 长表列名。
#' @param round,dimension 传入研究时用于筛选。缺省取最后一轮和主维度。
#'
#' @return 列表：`cronbach_alpha`、`icc_2_1`、`icc_2_k`、`krippendorff_alpha`、
#'   `krippendorff_note`、`n_expert`、`n_item`、`note`。人数或条目不足时含 `error`。
#'
#' @section 方法学要点:
#' Cronbach 与 ICC 把 Likert 当作等距数据。ICC 为双向随机、绝对一致：
#' 条目是测量对象，专家是随机评分者。Krippendorff 使用有序距离；未安装 `irr` 时不计算。
#'
#' @examples
#' study <- simulate_delphi(n_experts = 8, n_items = 4, n_rounds = 1, seed = 3)
#' rater_reliability(study, round = 1)
#'
#' @export
rater_reliability <- function(ratings, expert_col = "expert_id", item_col = "item_id",
                              score_col = "score", round = NULL, dimension = NULL) {
  used_round <- round
  used_dimension <- dimension
  if (inherits(ratings, "delphi_result")) ratings <- ratings$study
  if (inherits(ratings, "delphi_study")) {
    study <- .match_study(ratings)
    ratings <- .ensure_unable(.ensure_dimension(study$ratings))
    if (is.null(used_round)) used_round <- max(ratings$round)
    ratings <- ratings[ratings$round == used_round, , drop = FALSE]
    if (is.null(used_dimension)) used_dimension <- .consensus_dimension(ratings$dimension)
    ratings <- ratings[ratings$dimension == used_dimension, , drop = FALSE]
  }
  mat <- tryCatch(
    {
      if (is.matrix(ratings)) {
        out <- ratings
        storage.mode(out) <- "numeric"
        out[stats::complete.cases(out), , drop = FALSE]
      } else {
        .complete_score_matrix(ratings, expert_col, item_col, score_col)
      }
    },
    error = function(e) e
  )
  if (inherits(mat, "error")) {
    return(list(error = conditionMessage(mat), note = "\u8bc4\u5206\u8005\u4fe1\u5ea6\u672a\u8ba1\u7b97\u3002"))
  }
  m <- nrow(mat)
  k <- ncol(mat)
  if (m < 2L || k < 2L) {
    return(list(
      error = "\u4fe1\u5ea6\u81f3\u5c11\u9700\u8981 2 \u4f4d\u4e13\u5bb6\u548c 2 \u4e2a\u6761\u76ee\u7684\u5b8c\u6574\u8bc4\u5206\u3002",
      n_expert = m,
      n_item = k,
      note = "\u4eba\u6570\u6216\u6761\u76ee\u4e0d\u8db3\uff0c\u672a\u8ba1\u7b97\u4fe1\u5ea6\u3002"
    ))
  }
  alpha <- .cronbach_alpha(mat)
  icc <- .icc_agreement(t(mat))
  kripp <- .krippendorff_ordinal(mat)
  structure(
    list(
      cronbach_alpha = alpha,
      icc_2_1 = icc$icc_2_1,
      icc_2_k = icc$icc_2_k,
      krippendorff_alpha = kripp$value,
      krippendorff_note = kripp$note,
      n_expert = m,
      n_item = k,
      round = used_round,
      dimension = used_dimension,
      note = paste0(
        "Cronbach's \u03b1 \u4e0e ICC \u628a\u6709\u5e8f\u8bc4\u5206\u5f53\u4f5c\u7b49\u8ddd\uff0c\u53ea\u4f5c\u8865\u5145\u3002",
        "ICC(2,1) \u4e0e ICC(2,k) \u4e3a\u53cc\u5411\u968f\u673a\u3001\u7edd\u5bf9\u4e00\u81f4\uff1a\u6761\u76ee\u662f\u5bf9\u8c61\uff0c\u4e13\u5bb6\u662f\u968f\u673a\u8bc4\u5206\u8005\u3002",
        "\u5b83\u4eec\u4e0d\u51b3\u5b9a\u5355\u6761\u53bb\u7559\uff0c\u4e5f\u4e0d\u52a0\u6743\u3002"
      )
    ),
    class = "delphi_reliability"
  )
}

.complete_score_matrix <- function(ratings, expert_col, item_col, score_col) {
  ratings <- .as_df(ratings)
  .need_cols(ratings, c(expert_col, item_col, score_col), "\u8bc4\u5206\u8005\u4fe1\u5ea6")
  if ("unable" %in% names(ratings)) {
    ratings[[score_col]][ratings$unable %in% TRUE] <- NA_real_
  }
  ratings <- ratings[!is.na(ratings[[score_col]]), , drop = FALSE]
  .stop_if(!nrow(ratings), "\u6ca1\u6709\u6709\u6548\u8bc4\u5206\u3002")
  wide <- stats::reshape(
    ratings[, c(expert_col, item_col, score_col)],
    idvar = expert_col,
    timevar = item_col,
    direction = "wide"
  )
  mat <- as.matrix(wide[, -1, drop = FALSE])
  storage.mode(mat) <- "numeric"
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  colnames(mat) <- sub(paste0("^", score_col, "\\."), "", colnames(mat))
  mat
}

.cronbach_alpha <- function(mat) {
  k <- ncol(mat)
  item_var <- apply(mat, 2, stats::var)
  total <- stats::var(rowSums(mat))
  if (!is.finite(total) || total <= .Machine$double.eps) return(NA_real_)
  as.numeric((k / (k - 1)) * (1 - sum(item_var) / total))
}

.icc_agreement <- function(mat) {
  # rows = targets (items), columns = raters (experts)
  n <- nrow(mat)
  k <- ncol(mat)
  empty <- list(icc_2_1 = NA_real_, icc_2_k = NA_real_)
  if (n < 2L || k < 2L) return(empty)
  grand <- mean(mat)
  row_means <- rowMeans(mat)
  col_means <- colMeans(mat)
  ss_rows <- k * sum((row_means - grand)^2)
  ss_cols <- n * sum((col_means - grand)^2)
  ss_tot <- sum((mat - grand)^2)
  ss_err <- ss_tot - ss_rows - ss_cols
  df_r <- n - 1
  df_c <- k - 1
  df_e <- df_r * df_c
  if (df_e <= 0L || df_r <= 0L || df_c <= 0L) return(empty)
  bms <- ss_rows / df_r
  jms <- ss_cols / df_c
  ems <- ss_err / df_e
  if (!is.finite(ems) || ems < 0) ems <- 0
  den1 <- bms + (k - 1) * ems + k * (jms - ems) / n
  den2 <- bms + (jms - ems) / n
  icc1 <- if (!is.finite(den1) || abs(den1) < .Machine$double.eps) NA_real_ else (bms - ems) / den1
  icc2 <- if (!is.finite(den2) || abs(den2) < .Machine$double.eps) NA_real_ else (bms - ems) / den2
  list(icc_2_1 = as.numeric(icc1), icc_2_k = as.numeric(icc2))
}

.krippendorff_ordinal <- function(mat) {
  if (!requireNamespace("irr", quietly = TRUE)) {
    return(list(
      value = NULL,
      note = "\u672a\u5b89\u88c5 irr\uff0c\u672a\u8ba1\u7b97 Krippendorff's \u03b1\u3002\u5b89\u88c5\u540e\u53ef\u7528\u6709\u5e8f\u5c3a\u5ea6\u91cd\u7b97\uff1ainstall.packages(\"irr\")\u3002"
    ))
  }
  fit <- tryCatch(
    irr::kripp.alpha(mat, method = "ordinal"),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(
      value = NULL,
      note = paste0("Krippendorff's \u03b1 \u672a\u8ba1\u7b97\uff1a", conditionMessage(fit))
    ))
  }
  list(
    value = as.numeric(fit$value),
    note = paste0(
      "Krippendorff's \u03b1 \u4f7f\u7528\u6709\u5e8f\u8ddd\u79bb\uff08irr::kripp.alpha, method = ordinal\uff09\u3002",
      "\u8fd9\u91cc\u4ecd\u7528\u5b8c\u6574\u75c5\u4f8b\uff0c\u4ee5\u4fbf\u4e0e \u03b1 \u548c ICC \u5bf9\u7167\u3002\u5b83\u4e0d\u51b3\u5b9a\u6761\u76ee\u53bb\u7559\u3002"
    )
  )
}

.consensus_dimension <- function(dims) {
  dims <- unique(as.character(dims[!is.na(dims) & nzchar(as.character(dims))]))
  if (!length(dims)) return(NA_character_)
  prefer <- c(
    "\u91cd\u8981\u7a0b\u5ea6", "\u91cd\u8981\u6027", "overall",
    "\u9002\u5b9c\u6027", "\u540c\u610f\u7a0b\u5ea6"
  )
  hit <- intersect(prefer, dims)
  if (length(hit)) hit[1] else dims[1]
}
