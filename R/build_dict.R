#' Build household model from obs
#'
#' shell function for calculating the maximal household and calculating recursively build the parametric model based on the given observation.
#'
#'
#' @param obs_list list of observations
#' @param risk_groups number of risk groups in the model
#' @param past_dict optional
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0")
#' build_dict(obs_list,3)
#'
#' @export
build_dict <- function(obs_list, risk_groups, past_dict = NULL){
  # extract maximal family size according to the data
  max_members <- calc_max_members(obs_list,risk_groups)

  # create symbolic dictionary
  dict <- create_symbolic_dictionary(max_members, past_dict)
  return(dict)
}
