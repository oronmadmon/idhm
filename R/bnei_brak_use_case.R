#' Bnei Brak data
#'
#' estimation of the susceptibilities of adults/adolescents/children using a dataset containing SARS-CoV-2 testing results, collected from the city of Bnei Brak, Israel.
#'
#' @param initialization EM parameter, see ?run_EM
#' @param max_iter EM parameter, see ?run_EM
#' @param tolerance EM parameter, see ?run_EM
#' @param full_log EM parameter, see ?run_EM
#'
#' @export
bnei_brak_use_case <- function(initialization=3, max_iter=10, tolerance=0.01, full_log=FALSE){
  # threshold
  t <- 2
  # model
  risk_groups <- 3
  param_names <- paste0('Q', 1:risk_groups)
  dict <- readRDS('data/Bnei_Brak_dict.RDS')

  # observations
  temp <- preprocessing_Bnei_Brak(t)
  observations_dict <- data.frame(do.call(rbind, temp[[2]]))
  nms <- c("N.1","N.2","N.3","Pt.1","Pt.2","Pt.3","T.1","T.2","T.3")
  observations_dict[,nms] <- t(apply(observations_dict['id'], 1, function(r) as.numeric(strsplit(r, split=',')[[1]])))

  # getting ready for the E-M
  imputationsTable <- preparing_for_EM(dict, observations_dict[,nms], risk_groups, observations_dict$prob)
  rm(dict, observations_dict, temp)

  return (run_EM(imputationsTable, risk_groups, initialization=initialization, max_iter=max_iter, tolerance=tolerance, full_log=full_log))
}
