#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
git_file_history <- function(file = rstudioapi::getSourceEditorContext()$path){

  rel_path <- do.call(file.path, as.list((setdiff(
  unlist(strsplit(path.expand(file), "/")),
  unlist(strsplit(rprojroot::find_package_root_file(), "/"))
  ))))


  rstudioapi::terminalExecute(
    command = paste(
      "git",
      sprintf("log --follow -p -- %s", rel_path)
    )
  )
}
