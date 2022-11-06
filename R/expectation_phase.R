expectation_phase <- function(idsTable, Q, B=NULL, dig=7){
  # Calculate E[idsTable|Q,B] where idsTable represent a distribution (not normalized, the sum is n).
  # col(idsTable) = (original observation index, id2_imputation, id2_imputed, symbolic probability for that imputation)
  # dict$prob[dict$id2 == id2_imputation] is the probability of the value id2_imputed.
  # the returned value is a data frame ["imputation_prob", "id2_imputed"] where each line represent a possible imputation
  #    for some observation with it probability given Q,B ()
  distributionTable <- data.frame(idsTable["imputation_prob"], idsTable["id2_imputed"])
  distributionTable$imputation_prob <- apply(distributionTable["imputation_prob"],1,function(r) round(do.call(what = make_function_robust(r[[1]]), args = as.list(Q)),digits = dig))
  return (distributionTable[c("imputation_prob", "id2_imputed")])
}
