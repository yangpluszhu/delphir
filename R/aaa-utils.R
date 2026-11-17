# Internal utilities -------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}

.as_df <- function(x) {
  if (is.null(x)) return(NULL)
  as.data.frame(x, stringsAsFactors = FALSE)
}

.stop_if <- function(cond, msg) {
  if (isTRUE(cond)) stop(msg, call. = FALSE)
}

.warn_if <- function(cond, msg) {
  if (isTRUE(cond)) warning(msg, call. = FALSE)
}

.need_cols <- function(data, cols, what) {
  missing <- setdiff(cols, names(data))
  .stop_if(
    length(missing) > 0L,
    sprintf("%s \u7f3a\u5c11\u5fc5\u586b\u5217: %s", what, paste(missing, collapse = ", "))
  )
}

.pkg_version <- function() {
  tryCatch(as.character(utils::packageVersion("delphir")), error = function(e) "0.8.0")
}

.need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("\u9700\u8981\u5b89\u88c5\u5305 %s\uff1ainstall.packages(\"%s\")", pkg, pkg),
      call. = FALSE
    )
  }
}

.scale_range <- function(protocol) {
  c(protocol$scale_min, protocol$scale_max)
}

.in_range <- function(x, rng) {
  !is.na(x) & x >= rng[1] & x <= rng[2]
}

.valid_scores <- function(score, unable = NULL) {
  if (is.null(unable)) unable <- is.na(score)
  unable[is.na(unable)] <- FALSE
  as.numeric(score[!unable & !is.na(score)])
}

.safe_median <- function(x) {
  if (length(x) == 0L) return(NA_real_)
  as.numeric(stats::median(x, na.rm = TRUE))
}

.safe_quantile <- function(x, probs) {
  if (length(x) == 0L) return(rep(NA_real_, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, names = FALSE, type = 7, na.rm = TRUE))
}

.safe_mean <- function(x) {
  if (length(x) == 0L) return(NA_real_)
  mean(x, na.rm = TRUE)
}

.safe_sd <- function(x) {
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x, na.rm = TRUE)
}

.pct <- function(num, den) {
  if (is.na(den) || den <= 0) return(NA_real_)
  100 * num / den
}

.fmt_median_iqr <- function(median, q1, q3) {
  ifelse(
    is.na(median),
    NA_character_,
    sprintf("%.0f (%.0f\u2013%.0f)", median, q1, q3)
  )
}

.round_df <- function(data, digits = 2) {
  num <- vapply(data, is.numeric, logical(1))
  data[num] <- lapply(data[num], function(x) round(x, digits))
  data
}

.unique_sorted <- function(x) {
  sort(unique(x[!is.na(x)]))
}

.match_protocol <- function(x) {
  if (inherits(x, "delphi_study")) return(x$protocol)
  if (inherits(x, "delphi_result")) return(x$study$protocol)
  if (inherits(x, "delphi_protocol")) return(x)
  stop("\u9700\u8981 delphi_protocol / delphi_study / delphi_result \u5bf9\u8c61\u3002", call. = FALSE)
}

.match_study <- function(x) {
  if (inherits(x, "delphi_study")) return(x)
  if (inherits(x, "delphi_result")) return(x$study)
  stop("\u9700\u8981 delphi_study \u6216 delphi_result \u5bf9\u8c61\u3002", call. = FALSE)
}

.add_class <- function(x, cls) {
  class(x) <- unique(c(cls, class(x)))
  x
}

.ensure_unable <- function(ratings) {
  n <- nrow(ratings)
  if (is.null(n) || n == 0L) {
    if (!"unable" %in% names(ratings)) ratings$unable <- logical()
    ratings$unable <- as.logical(ratings$unable)
    return(ratings)
  }
  if (!"unable" %in% names(ratings)) {
    ratings$unable <- is.na(ratings$score)
  }
  ratings$unable <- as.logical(ratings$unable)
  ratings$unable[is.na(ratings$unable)] <- FALSE
  ratings$unable[is.na(ratings$score)] <- TRUE
  ratings
}

.ensure_dimension <- function(ratings) {
  n <- nrow(ratings)
  if (is.null(n) || n == 0L) {
    if (!"dimension" %in% names(ratings)) ratings$dimension <- character()
    ratings$dimension <- as.character(ratings$dimension)
    return(ratings)
  }
  if (!"dimension" %in% names(ratings) || all(is.na(ratings$dimension))) {
    ratings$dimension <- rep("overall", n)
  }
  ratings$dimension <- as.character(ratings$dimension)
  ratings
}

.item_key <- function(item_id, dimension) {
  paste(item_id, dimension, sep = "||")
}

.label_zh <- function(code) {
  map <- c(
    high_consensus = "\u9ad8\u5ea6\u5171\u8bc6",
    basic_consensus = "\u57fa\u672c\u5171\u8bc6",
    no_consensus = "\u672a\u8fbe\u5171\u8bc6",
    reject = "\u503e\u5411\u6392\u9664",
    borderline = "\u4e34\u754c",
    polarized = "\u4e24\u6781\u5206\u5316",
    consensus_in = "\u7eb3\u5165\u5171\u8bc6",
    consensus_out = "\u6392\u9664\u5171\u8bc6",
    appropriate = "\u9002\u5b9c",
    uncertain = "\u4e0d\u786e\u5b9a",
    inappropriate = "\u4e0d\u9002\u5b9c",
    keep = "\u4fdd\u7559",
    drop = "\u5220\u9664",
    discuss = "\u5272\u5165\u4f1a\u8bae\u8ba8\u8bba",
    retain = "\u7eb3\u5165\u6838\u5fc3\u6280\u672f\u8981\u7d20",
    not_retain = "\u672a\u7eb3\u5165",
    described = "\u63cf\u8ff0\u6027\uff08\u719f\u6089\u7a0b\u5ea6\uff09",
    stable = "\u7a33\u5b9a",
    unstable = "\u5c1a\u4e0d\u7a33\u5b9a"
  )
  unname(ifelse(code %in% names(map), map[code], as.character(code)))
}

.scenario_label <- function(scenario) {
  map <- c(
    clinical_consensus = "\u533b\u5b66\u4e13\u5bb6\u5171\u8bc6\uff08Modified Delphi\uff09",
    comet_cos = "COMET \u6838\u5fc3\u7ed3\u5c40\u96c6",
    rand_ucla = "RAND/UCLA \u9002\u5b9c\u6027\u8bc4\u4ef7",
    delphi_ram = "\u6539\u826f\u5fb7\u5c14\u83f2\u7ed3\u5408 RAM",
    indicator = "\u6307\u6807\u4f53\u7cfb\u7b5b\u9009",
    ranking = "\u4f18\u5148\u7ea7\u6392\u5e8f",
    definition = "\u5b9a\u4e49/\u8bca\u65ad\u6807\u51c6\u5171\u8bc6",
    policy = "Policy Delphi",
    standard_element = "\u6807\u51c6\u6838\u5fc3\u6280\u672f\u8981\u7d20\uff085 \u5206\u91cf\u8868\uff09"
  )
  unname(ifelse(scenario %in% names(map), map[scenario], scenario))
}

.check_locked_protocol <- function(protocol, new_args) {
  if (!isTRUE(protocol$locked)) return(invisible(TRUE))
  changed <- names(new_args)[vapply(names(new_args), function(nm) {
    !identical(new_args[[nm]], protocol$rules[[nm]]) &&
      !identical(new_args[[nm]], protocol[[nm]])
  }, logical(1))]
  .stop_if(
    length(changed) > 0L,
    paste0(
      "\u534f\u8bae\u5df2\u9501\u5b9a\uff0c\u4e0d\u5141\u8bb8\u5728\u5206\u6790\u9636\u6bb5\u4fee\u6539\u5171\u8bc6\u9608\u503c\uff08",
      paste(changed, collapse = ", "),
      "\uff09\u3002\u8bf7\u5728 delphi_protocol() \u4e2d\u4e8b\u5148\u89c4\u5b9a\u5e76\u586b\u5199 rationale\u3002"
    )
  )
  invisible(TRUE)
}
