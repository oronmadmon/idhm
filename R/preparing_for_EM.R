#' Preparing for EM
#'
#' create all possible households, and calculate both the conditional probabilities and the unconditional one
#' @param dict probabilistic model
#' @param observation_table partial observations table
#' @param risk_groups number of risk groups in the model
#' @param obs_weights useful when the index case isn't known, optional
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0")
#' preparing_for_EM(build_dict(obs_list,3), obs_list_to_dict(obs_list,3), 3)
#'
#' @export
preparing_for_EM <- function(dict, observation_table, risk_groups, obs_weights=NULL){
  imputations_table <- create_imputations_table(dict, observation_table, risk_groups)
  imputations_table$imputed_prob <- dict[match(imputations_table$id2_imputed,dict$id2),]$prob

  if (is.null(obs_weights)) { return(imputations_table) }
  if (nrow(observation_table) != length(obs_weights)) {
    print("obs_weights must be consisent with observation_table - skipped weighing")
    return(imputations_table)
  }

  for (i in 1:nrow(imputations_table)){
    obs_weight <- obs_weights[imputations_table$observation_index[i]]
    if (obs_weight < 1){
      imputations_table$imputed_prob[i] <- paste0(obs_weight,'*(',imputations_table$imputed_prob[i],')')
    }
  }
  return(imputations_table)
}
