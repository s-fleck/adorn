
#' Read Tab-separted Tables from Windows Clipboard
#'
#' `read_clipboard_tab()` is a simple wrapper for `read.table()`, with presets
#' set for tab separated table cells (such as cells copied from Excel or other
#' spreadhseet software)
#'
#' @param ... passed on to [read_table()]
#' @inheritParams utils::read.table
#'
#' @return a data.frame
#' @family windows clipboard utils
#' @export
#' @md
#'
read_clipboard_tab <- function(
  header = FALSE,
  stringsAsFactors = FALSE,
  ...
){
  assert_that(is_running_windows())
  read.table(
    file = "clipboard",
    sep = "\t", header = header,
    stringsAsFactors = stringsAsFactors,
    ...)
}




#' `paste_clipboard()` pastes tab-separted tables from windows clipboard to the
#'   variable `adorn_clip` in the global environment
#'
#' @rdname read_clipboard_tab
#' @export
#' @md
#'
paste_clipboard <- function(header = FALSE, ...){
  dat <- read_clipboard_tab(header  = header, ...)
  assign('adorn_clip', dat, envir = globalenv())
  invisible(dat)
}




#' Write an R Object to the Windows Clipboard
#'
#' @param x any R object
#'
#' @section Side Effects:
#'   Write to the Windows Clipboard
#'
#' @family windows clipboard utils
#' @return `x` (invisibly)
#' @md
#' @export
#'
write_clipboard <- function(x, ...){
  assert_that(is_running_windows())
  UseMethod('write_clipboard')
}




#' Write a Data Frame to the Windows Clipboard
#'
#' @param x a data.frame
#' @param sep a scalar character. Defaults to `\\t` (Tab) for compatibilty with
#'   Excel or similar software
#' @param ... further arguments passed on to [write.table()]
#' @inheritParams utils::write.table
#'
#' @section Side Effects:
#'   Write a Data Frame to the Windows Clipboard
#'
#' @family windows clipboard utils
#' @inherit write_clipboard return
#' @export
#' @md
write_clipboard.data.frame <- function(
  x,
  row.names=FALSE,
  sep="\t",
  ...
){
  write.table(x, "clipboard", row.names = row.names, sep = "\t", ...)
  invisible(x)
}
