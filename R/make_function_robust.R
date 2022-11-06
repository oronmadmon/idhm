make_function_robust <- function(body, param_names=paste0('Q', 1:3)){
  args <- paste(param_names, collapse = ', ')
  eval(parse(text = paste('f <- function(', args, ') { return(' , body , ')}', sep='')))
}
