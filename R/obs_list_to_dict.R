#' Observations list to dict
#'
#' transfers obs_list to a dictionary.
#'
#' @param obs_list list of observations
#' @param risk_groups number of risk groups in the model
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0")
#' obs_list_to_dict(obs_list,3)
#'
#' @export
obs_list_to_dict <- function(obs_list, risk_groups){
  observations_dict <- data.frame(t(sapply(obs_list,function(r) as.numeric(strsplit(r, split=',')[[1]]))))
  if (length(observations_dict) == 2*risk_groups){
    for (r in 1:risk_groups){
      observations_dict[,2*risk_groups + r] <- observations_dict[,r]
    }
  }
  names(observations_dict) <- c(paste0("N.",1:risk_groups),paste0("Pt.",1:risk_groups),paste0("T.",1:risk_groups))
  return(observations_dict)
}
