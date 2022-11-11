#' Make function from string
#'
#' generate UDF from a string containing math expression
#'
#'
#' @param body string containing parametric math expression
#' @param param_names parameters of the UDF
#'
make_function_robust <- function(body, param_names=paste0('Q', 1:3)){
  args <- paste(param_names, collapse = ', ')
  eval(parse(text = paste('f <- function(', args, ') { return(' , body , ')}', sep='')))
}
