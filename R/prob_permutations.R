#' Prob permutations
#'
#' calculating the symbolic probabilities of all permutations of the risk groups
#'
#' @import stringr
#'
#' @param risk_groups number of risk groups in the model
#' @param sym_prob symbolic probability of the original household
#' @param id2 id2 of the original household
#'
#' @examples
#' prob_permutations(3,"Q1*Q2","1,1,0,0,0,0")
#' prob_permutations(3, 'Q1^2*Q2', '2,1,0,0,0,0')
#'
#' @export
prob_permutations <- function(risk_groups, sym_prob, id2){
  param_names <- paste0('Q', 1:risk_groups)
  perms <- id2_permutations(risk_groups, id2)
  outer <- param_names
  names(outer) <- paste0('Q.', 1:risk_groups)
  inner <- names(outer)
  perms$prob <- NA
  for (j in 1:nrow(perms)){
    names(inner) <- param_names[c(unlist(perms[j,"permutation"]))]
    perms[j,"prob"] <- str_replace_all(str_replace_all(sym_prob,inner),outer)
  }
  return (perms)
}
