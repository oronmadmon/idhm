#' Preparing for EM
#'
#' create all possible households, and calculate both the conditional probabilities and the unconditional one
#' @param dict probabilistic model
#' @param observation_table partial observations table
#' @param risk_groups number of risk groups in the model
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0")
#' preparing_for_EM(build_dict(obs_list,3), obs_list_to_dict(obs_list,3), 3)
#'
#' @export
preparing_for_EM <- function(dict, observation_table, risk_groups){
  imputations_table <- create_imputations_table(dict, observation_table, risk_groups)
  imputations_table$imputed_prob <- dict[match(imputations_table$id2_imputed,dict$id2),]$prob
  return(imputations_table)
}
