is_running_windows <- function(x){
  identical(Sys.info()['sysname'], 'Windows')
}



on_failure(is_running_windows) <- function(call, env) {
  "Operation only supported on Windows operating systems."
}
