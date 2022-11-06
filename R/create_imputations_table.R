create_imputations_table <- function(observationsTable){
  # observationsTable <- observations

  # any observation should be a list of size 3*r
  # if the observations presented as strings (obs="3,2,2,1,1,1,2,1,1") we need to call
  # create_imputation_possibilities(strsplit(obs, split=',')[[1]])

  # Create one table whose sub-tables are create_imputation_possibilities(observation)
  observationsNum <- nrow(observationsTable)
  imputationsTable <- create_imputation_possibilities(observationsTable[1,])
  imputationsTable[is.na(imputationsTable)] <- 1
  for (i in 2:observationsNum){
    imputationsTable <- rbind(imputationsTable, create_imputation_possibilities(observationsTable[i,]))
    imputationsTable[is.na(imputationsTable)] <- i
  }
  return(imputationsTable)
}
