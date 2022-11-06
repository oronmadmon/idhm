prob_permutations <- function(risk_groups, sym_prob, id2){
  # sym_prob <- "Q1*Q2"
  # id2 <- "1,1,0,0,0,0"
  param_names <- paste('Q.', 1:risk_groups)
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
