#' Set vertical space
#'
#' Ensure the a speciefied number of vertical space at a given location in an
#' R script file.
#'
#' @param spacing integer. Number of blank rows desired at position.
#'   Defaults to `4`.
#' @param row integer. Row of the document at which to ensure the specified
#'   number of blank rows. Defaults to the current cursor position.
#'
#' @export
#' @md
set_vspace <- function(
  spacing = options('adorn.vspace'),
  row = NULL
){
  spacing <- as.integer(spacing)
  assert_that(is.number(spacing))

  context <- rstudioapi::getSourceEditorContext()
  doc     <- context$contents

  if(is.null(row)){
    pos <- list(
      row = context$selection[[1]]$range$start[['row']]
    )
  } else {
    pos <- list(
      row = row
    )
  }

  runs     <- unclass(base::rle(base::trimws(doc) == ''))
  runs$row <- cumsum(runs$lengths)
  sel_run <- which((runs$row %/% pos$row) == 1)[[1]]

  if(runs$value[sel_run] == FALSE){
    sel_run = sel_run + 1L
  }

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




#' Set vertical space for entire document
#'
#' Sets the vertical space between expressions in an R script file. This is
#' useful when working with function files in a package's \file{R} direcotry.
#'
#' @inheritParams set_vspace
#'
#' @return
#' @export
#'
#' @examples
set_vspace_document <- function(
  spacing = options('adorn.vspace')
){
  context <- rstudioapi::getSourceEditorContext()
  doc     <- context$contents
  dat     <- parse(text = doc)

  srcrefs <- attr(dat, "srcref")

  insert_pos <- lapply(srcrefs, function(x){
    rstudioapi::document_position(
      row    = x[[3]] + 1L,
      column = 1L)
  }) %>%
    rev()


  # Insert newline after each function
  lapply(insert_pos, rstudioapi::insertText, text = '\n', id = context$id)
  vspace_rows <- vapply(insert_pos, `[[`, double(1), 1L)

  # Expand vspace to param spacing
  vspace_rows <- vspace_rows + (rev(seq_along(vspace_rows)) - 1L)  # account for inserted newlines
  lapply(vspace_rows, function(x) set_vspace(spacing = spacing, row = x))

  # Ensure document ends with a single newline
  last_row <- length(rstudioapi::getSourceEditorContext()$contents)
  set_vspace(1L, row = last_row)
}
