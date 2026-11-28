.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "delphir ", utils::packageVersion("delphir"),
    "\uff1a\u65b9\u6848\u9501\u5b9a\u540e\u7684 Delphi / RAND/UCLA / COMET \u5206\u6790\u3002\n",
    "\u6559\u5b66\u6587\u6863: delphi_guides() \u6216 vignette(package = \"delphir\")"
  )
}

#' @importFrom ggplot2 .data
#' @importFrom stats median quantile sd complete.cases pchisq wilcox.test reshape runif
#' @importFrom utils head browseURL vignette packageVersion
NULL
