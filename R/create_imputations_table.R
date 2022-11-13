#' Imputations table
#'
#' create all possible households for a table of partial observations and calculate,
#' based on the probabilistic model, the conditional probabilities
#'
#' @param dict probabilistic model
#' @param observation_table partial observations table
#' @param risk_groups number of risk groups in the model
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0")
#' create_imputations_table(build_dict(obs_list,3), obs_list_to_dict(obs_list,3), 3)
#'
#' @export
create_imputations_table <- function(dict, observation_table, risk_groups){
  observationsNum <- nrow(observation_table)
  if (observationsNum == 0) {return(NULL)}
  imputationsTable <- create_imputation_possibilities(dict, observation_table[1,], risk_groups)
  imputationsTable[is.na(imputationsTable)] <- 1
  if (observationsNum == 1) {return(imputationsTable)}
  for (i in 2:observationsNum){
    imputationsTable <- rbind(imputationsTable, create_imputation_possibilities(dict, observation_table[i,], risk_groups))
    imputationsTable[is.na(imputationsTable)] <- i
  }
  return(imputationsTable)
}
