#' Imputation table
#'
#' create all possible households for a partial observation and calculate,
#'  based on the probabilistic model, the conditional probabilities
#'
#' @param dict probabilistic model
#' @param id3_template partial observation
#' @param risk_groups number of risk groups in the model
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0")
#' create_imputation_possibilities(build_dict(obs_list,3), obs_list_to_dict(obs_list,3)[2,], 3)
#'
#' @export
create_imputation_possibilities <- function(dict, id3_template, risk_groups){
  n <- id3_template[1:risk_groups]
  d <- id3_template[(risk_groups+1) : (2*risk_groups)]
  c <- id3_template[(2*risk_groups+1) : (3*risk_groups)]

  # sub family that did not tested
  maxImpute <- n-c

  ag_names <- 1:risk_groups
  names(maxImpute) <- ag_names
  I_ag <- lapply(maxImpute,function(m) 0:m)
  I_names <- paste0('I.',ag_names)
  names(I_ag) <- I_names

  imputationTable <- expand.grid(c(I_ag))
  imputationTable$id2_imputation <- apply(imputationTable[,I_names],1,function(r) paste0(c(maxImpute, r),collapse=','))
  imputationTable$id2_imputed <- apply(imputationTable[,I_names],1,function(r) paste0(c(n, d+r),collapse=','))
  imputationTable$observation_index <- NA

  # adding probability for each imputation - normalizing the conditional distribution
  imputationTable$imputation_prob_numerator <- apply(imputationTable["id2_imputed"],1,function(r) dict$prob[dict$id2 == r])
  normalization_factor_sym <- paste(imputationTable["imputation_prob_numerator"][[1]], collapse = '+')
  imputationTable$imputation_prob <- apply(imputationTable["imputation_prob_numerator"],1,function(r) paste('(', r, ')/(', normalization_factor_sym, ')'))
  return (imputationTable[c("observation_index", "id2_imputation", "id2_imputed", "imputation_prob")])
}
