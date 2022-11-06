#' Calculate maximal household
#'
#' calculating the maximal household, element-wise, based on the given observation.
#'
#'
#' @param obs_list list of observations
#' @param risk_groups number of risk groups in the model
#'
#' @export
calc_max_members <- function(obs_list,risk_groups){
  maximal_vec <- numeric(risk_groups)
  for (obs in obs_list){
    obs_vec <- unlist(strsplit(obs, split = ','))
    for (r in 1:risk_groups){
      maximal_vec[r] <- max(maximal_vec[r], obs_vec[r])
    }
  }
  return(maximal_vec)
}
