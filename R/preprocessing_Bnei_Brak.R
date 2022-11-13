#' Preprocessing Bnei Brak data
#'
#' create all possible households, and calculate both the conditional probabilities and the unconditional one
#'
#' @import readr
#'
#' @param threshold an individual result considered as certain if either 1. a serological test result is available 2. at least one PCR positive result or 3. threshold negative results
#'
#' @export
preprocessing_Bnei_Brak <- function(threshold = 2){
  df <- read_csv("data/families_data_with_sero.csv")

  families_keys <- df$family_hash[!duplicated(df$family_hash)]

  df$risk_group <- apply(df[,'age'], 1, function(r) if (r>19) 1 else (if (r>9) 2 else 3))
  df$sero_result_extended <- apply(df[,'sero_result'], 1, function(r) if (is.na(r)) "NA" else r)
  df$pcr_result <- apply(df[,'sick'], 1, function(r) if (r) "POS" else "NEG")
  df$combined_result <- apply(df[,c('pcr_result','sero_result_extended')], 1, function(r) if (r[1]=="POS" | r[2]=="POS") "POS" else "NEG")

  nms <- c('sick', 'sero_result', paste0('result', threshold))
  df$tested <- apply(df[,nms], 1, function(r) if (r[1]=="TRUE" | !is.na(r[2]) | !is.na(r[3])) TRUE else FALSE)
  missing_prob <- 1 - sum(df$tested)/nrow(df)



  families_data <- lapply(families_keys, function(r) df[df$family_hash == r[1],])

  obs_with_prob <- list()
  for (k in 1:length(families_data)){
    fam <- families_data[[k]]
    n_full <- numeric(3)
    p_full <- numeric(3)
    t_full <- numeric(3)
    for (i in 1:nrow(fam)){
      n_full[fam$risk_group[i]] <- n_full[fam$risk_group[i]] + 1
      if (fam$tested[i] == TRUE){
        t_full[fam$risk_group[i]] <- t_full[fam$risk_group[i]] + 1
        if (fam$combined_result[i]=="POS"){
          p_full[fam$risk_group[i]] <- p_full[fam$risk_group[i]] + 1
        }
      }
    }
    for (r in 1:3){
      prob <- sum(fam$first_case_in_family[fam$risk_group == r])
      if (prob > 0){
        e_r <- numeric(3)
        e_r[r] <- 1
        id_partial <- paste(c(n_full - e_r, p_full - e_r, t_full - e_r),collapse=',')
        obs_with_prob <- append(obs_with_prob,
                                list(c('family'=fam$family_hash[1],
                                       'prob'=prob, 'id'=id_partial)))
      }
    }
  }
  data <- list('Threshold'=threshold,
               'missing_prob'=1 - sum(df$tested)/nrow(df),
               'weighted_obs'=obs_with_prob)
  #saveRDS(data, paste0("./data/obs_with_prob_",threshold,"-PCR.RDS"))
  return(list('missing_prob'=1 - sum(df$tested)/nrow(df),
              'weighted_obs'=obs_with_prob))
}

