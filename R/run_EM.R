#' Run EM
#'
#' run EM algorithm on the weighted observations to find the estimators
#'
#' @import tidyr
#' @importFrom stats optim
#' @importFrom stats rbeta
#'
#' @param imputationsTable weighted obs
#' @param risk_groups number of risk groups in the model
#' @param initialization number of starting points to consider
#' @param max_iter maximal steps to take from each starting point
#' @param tolerance stopping criterion
#' @param full_log whether to return the result from each step or just the best step
#'
#' @examples
#' obs_list <- list("1,2,0,1,1,0,1,1,0", "2,1,0,2,0,0,2,0,0", "0,0,1,0,0,0,0,0,0")
#' risk_groups <- 3
#' dict <- build_dict(obs_list,risk_groups)
#' number_of_families <- 15
#' new_obs_list <- generate_obs_list(dict, risk_groups, number_of_families)
#' observations_dict <- obs_list_to_dict(new_obs_list, risk_groups)
#' imputationsTable <- preparing_for_EM(dict, observations_dict, risk_groups)
#' run_EM(imputationsTable, risk_groups)
#'
#' run_EM(imputationsTable, risk_groups, full_log=TRUE)
#'
#' @export
run_EM <- function(imputationsTable, risk_groups, initialization=3, max_iter=10, tolerance=0.01, full_log=FALSE){
  # define the minus of the log-likelihood to minimize at each step
  table_to_func <- function(Q){
    evaluated_functions <- apply(imputationsTable['imputed_prob'], 1, function(r) log(do.call(what=make_function_robust(r[[1]]), args=as.list(Q))))
    # return weighted sum
    return(-idsProbs$imputation_prob %*% evaluated_functions)
  }
  # running EM iterations for initialization loops with up to max_iter steps within each loop
  prev2_max <- 1
  expectation_by <- list()
  maximizer <- list()
  max_val <- list()
  session_num <- list()
  for (loop in 1:initialization){
    prev_max <- round(rbeta(risk_groups, 8, 4),digits = 3)
    for (iter in 1:max_iter){
      # taking one step
      idsProbs <- expectation_phase(imputationsTable, prev_max, dig=4)
      optimal <- optim(round(prev_max, digits = 2), table_to_func)

      # logging the results
      expectation_by <- append(expectation_by, paste0(prev_max, collapse = ','))
      maximizer <- append(maximizer, paste0(round(optimal$par, digits = 3), collapse = ','))
      max_val <- append(max_val, -optimal$value)
      session_num <- append(session_num, loop)

      # checking stopping criteria
      if (sum(abs(prev_max-round(optimal$par,digits = 3)))<tolerance){
        break
      }
      else{
        prev_max <- round(optimal$par, digits = 3)
      }
    }
    if(prev2_max==1 || -optimal$value > prev2_max){
      prev2_max <- -optimal$value
      candidate <- list(maximizer = paste0(round(optimal$par, digits = 3), collapse = ','), max_val = -optimal$value)
    }
  }
  if (full_log){
    table_val <- tibble(unlist(session_num), unlist(expectation_by), unlist(maximizer), unlist(max_val))
    return(table_val)
  }
  return(tibble(maximizer=candidate$maximizer,max_val=candidate$max_val))
}
