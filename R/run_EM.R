run_EM <- function(obs_list, risk_groups, dict=NULL, initialization=3, max_iter=10, full_log=FALSE){

  # model
  param_names <- paste0('Q', 1:risk_groups)
  if(is.null(dict)){
    dict <- build_dict(obs_list, risk_groups)
  }

  # list observations to dict
  observations_dict <- obs_list_to_dict(obs_list, risk_groups)

  # prepering for EM and clearing unnecessary data
  imputationsTable <- preparing_for_EM(observations_dict, dict)
  rm(dict, observations_dict)

  # define the minus of the log-likelihood to minimize at each step
  table_to_func <- function(Q){
    evaluated_functions <- apply(imputationsTable['imputed_prob'], 1, function(r) log(do.call(what=make_function_robust(r[[1]]), args=as.list(Q))))
    # return weighted sum
    return(-idsProbs$imputation_prob %*% evaluated_functions)
  }
  # running EM iterations for initialization loops with up to max_iter steps within each loop
  prev2_max <- 1
  for (loop in 1:initialization){
    prev_max <- round(rbeta(3, 8, 4),digits = 3)
    expectation_by <- list()
    maximizer <- list()
    max_val <- list()
    session_num <- list()
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
      if (sum(abs(prev_max-round(optimal$par,digits = 3)))<0.01){
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
    table_val <- tibble(session_num, expectation_by, maximizer, max_val)
    return(table_val)
  }
  return(tibble(maximizer=candidate$maximizer,max_val=candidate$max_val))

}
