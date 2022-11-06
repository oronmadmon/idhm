preparing_for_EM <- function(observation_table, dict){

  imputations_table <- create_imputations_table(observation_table)
  imputations_table$imputed_prob <- dict[match(imputations_table$id2_imputed,dict$id2),]$prob
  return(imputations_table)
}
