create_imputation_possibilities <- function(id3_template){
  # Create a table for a family where not all house members were tested.
  # Each row in the table is a possible imputation.
  # id2_imputation is the id2 for the sub-family that did not tested
  # id2_imputed is the id2 for the entire family given the imputation

  # n=number of family members for each risk group
  # c=number of family members from each group that were tested
  # d=number of positive tests among the c that tested
  # id3_vec <- as.numeric(strsplit(id3_template[1], split=',')[[1]])
  n <- id3_template[1:risk_groups]
  d <- id3_template[(risk_groups+1) : (2*risk_groups)]
  c <- id3_template[(2*risk_groups+1) : (3*risk_groups)]

  # sub family that tested (index case excluded)
  # testedId2 <- paste(c(id3_template[(2*risk_groups+1) : (3*risk_groups)],id3_template[(risk_groups+1) : (2*risk_groups)]), collapse = ',')
  # testedProb <- dict$prob[dict$id2 == testedId2]

  # sub family that did not tested
  maxImpute <- n-c

  # create a table with possible imputations
  # the k-th possible imputation - imputationTable[[k]] is
  # ( imputation  , numerator , denominator, imputed family Id2 , imputed family prob )
  # where numerator/denominator = imputation probability


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

  # create the numeric functions and evaluate at point Q
  # imputationTable$imputation_prob_f <- apply(imputationTable["imputation_prob"],1,function(r) make_function_robust(r, risk_groups))
  # imputationTable$imputation_prob_f[[1]](0.4,0.4,0.4)

  return (imputationTable[c("observation_index", "id2_imputation", "id2_imputed", "imputation_prob")])
}
