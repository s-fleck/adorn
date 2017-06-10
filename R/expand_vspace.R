#' Title
#'
#' @param spacing
#'
#' @return
#' @export
#'
#' @examples
set_vspace <- function(spacing){
  spacing <- as.integer(spacing)
  assert_that(is.number(spacing))

  context <- rstudioapi::getSourceEditorContext()
  doc   <- context$contents

  pos <- list(
    row = rstudioapi::getSourceEditorContext()$selection[[1]]$range$start[['row']]
  )

  runs     <- unclass(base::rle(base::trimws(doc) == ''))
  runs$row <- cumsum(runs$lengths)
  sel_run <- which((runs$row %/% pos$row) == 1)[[1]]

  pos$run_length <- runs$length[[sel_run]]
  pos$run_start  <- runs$row[[sel_run - 1L]] + 1L
  pos$run_stop   <- runs$row[[sel_run - 1L]] + runs$length[[sel_run]]

  rstudioapi::modifyRange(
    rstudioapi::document_range(
      start = rstudioapi::document_position(pos$run_start, 1L),
      end   = rstudioapi::document_position(pos$run_stop, 1L)
    ),
    text = paste(rep('\n', spacing-1L), collapse = ''),
    id = context$id
  )
}




set_vspace4 <- function() set_vspace(4L)
