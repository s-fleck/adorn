#' adorn.
#'
#' @name adorn
#' @import assertthat
#' @importFrom magrittr %>%
#' @docType package
NULL




.onLoad <- function(...) {
  op <- options()

  op.testthis <- list(
    adorn.vspace = 4L
  )

  toset <- !(names(op.testthis) %in% names(op))
  if(any(toset)) options(op.testthis[toset])

  invisible()
}
