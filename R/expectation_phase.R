#' Expectation phase
#'
#' implement the expectation phase by evaluating the symbolic probabilities at a given vector Q
#'
#' @param idsTable table of possible households and their parametric probabilities
#' @param Q previous estimator, to be evaluated by
#' @param dig precision of the returned values
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0")
#' dict <- build_dict(obs_list,3)
#' imputationsTable <- create_imputations_table(dict, obs_list_to_dict(obs_list,3), 3)
#' expectation_phase(imputationsTable, c(0.7, 0.8, 0.8), dig=4)
#'
#' @export
expectation_phase <- function(idsTable, Q, dig=7){
  distributionTable <- data.frame(idsTable["imputation_prob"], idsTable["id2_imputed"])
  distributionTable$imputation_prob <- apply(distributionTable["imputation_prob"],1,function(r) round(do.call(what = make_function_robust(r[[1]]), args = as.list(Q)),digits = dig))
  return (distributionTable[c("imputation_prob", "id2_imputed")])
}
