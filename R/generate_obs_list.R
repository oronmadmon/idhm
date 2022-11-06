#' Generate observations
#'
#' generate household observations based on a given probabilistic model.
#'
#'
#' @param model_dict probabilistic model
#' @param risk_groups number of risk groups in the model
#' @param number_of_families number of observations to generate
#' @param families_generator parameters for the poisson distribution, for generating household members
#' @param models_generator parameters for the parametric model
#' @param missing_mechanism the generated observations will contain 1-missing_mechanism% missing tests
#' @param seed seed is used for reproducability
#'
#' @export
generate_obs_list <- function(model_dict, risk_groups, number_of_families, families_generator = c(2,1,1), models_generator = c(0.8, 0.8, 0.87), missing_mechanism = 0.8, seed = 3){
  set.seed(seed)

  param_names <- paste0('Q', 1:risk_groups)
  legit_ids <- model_dict$id1[!duplicated(model_dict$id1)]

  model_dict$prob_val <- NaN

  families <- list()
  while(length(families)<=number_of_families-1){
    idx_vec <- rpois(3,families_generator)
    idx <- paste0(idx_vec, collapse = ',')
    if(sum(legit_ids==idx)==1){
      families <- append(families, idx)
    }
  }
  partial_obs <- list()
  for (i in 1:number_of_families){
    # i<-1
    idx <- families[i]
    idx_vec <- as.numeric(strsplit(idx, split=',')[[1]])
    temp_dict <- model_dict[model_dict$id1==idx,]
    if (anyNA(temp_dict$prob_val)){
      # evaluate probabilities if needed
      model_dict[model_dict$id1==idx,]$prob_val <- apply(temp_dict["prob"],1,function(r) do.call(what = make_function_robust(r), args = as.list(models_generator)))
      temp_dict <- model_dict[model_dict$id1==idx,]
    }
    idx2 <- sample(temp_dict$id2, size = 1, prob = temp_dict$prob_val)

    idx_pos_vec <- as.numeric(strsplit(idx2, split=',')[[1]])[-c(1:risk_groups)]
    tested_pos <- rbinom(3, idx_pos_vec, missing_mechanism)
    tested_neg <- rbinom(3, idx_vec-idx_pos_vec, missing_mechanism)
    tested_vec <- tested_neg+tested_pos
    partial_obs <- append(partial_obs, paste0(c(idx,tested_pos,tested_pos+tested_neg),collapse = ','))
    # # observations_dict <- rbind(observations_dict,c(idx_vec,tested_pos,tested_pos+tested_neg))
  }
  return(partial_obs)
}
