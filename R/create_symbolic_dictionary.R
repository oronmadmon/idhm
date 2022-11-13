#' Build household model
#'
#' recursively build the parametric model up to the maximal household.
#'
#' @import Ryacas
#' @import tidyr
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#'
#' @param max_members maximal members per risk group
#' @param past_dict optional
#'
create_symbolic_dictionary <- function(max_members,past_dict = NULL) {
  ysym("MaxEvalDepth(100000)")

  # Creating an empty dictionary with all the option for number of
  # people and number of infected
  risk_groups <- length(max_members)
  ag_names <- 1:risk_groups
  names(max_members) <- ag_names
  N_ag <- lapply(max_members,function(m) 0:m)
  I_ag <- N_ag
  N_names <- paste0('N.',ag_names)
  I_names <- paste0('I.',ag_names)
  names(N_ag) <- N_names
  names(I_ag) <- I_names

  dict <- expand.grid(c(N_ag,I_ag))
  dict <- dict[-1,]
  dict <- dict[-which(apply(dict[,N_names]-dict[,I_names],1,function(r) any(r<0))),]
  rownames(dict) <- 1:nrow(dict)
  dict$id1 <- apply(dict[,N_names],1,function(r) paste0(r,collapse=','))
  dict$id2 <- apply(dict[,c(N_names,I_names)],1,function(r) paste0(r,collapse=','))

  # if there is a past dictionary add it, if not add an empty column for prob
  if(!is.null(past_dict)){
    dict <- left_join(dict, select(past_dict,"prob","id2"),by = c("id2"="id2"))
  } else {
    dict$prob <- NA
  }

  for(i in 1:nrow(dict)) {
    if(is.na(dict[i,]$prob)){
      dict <- calc_household_prob_symbolic(dict,i,N_names,I_names)
      perm_prob <- prob_permutations(risk_groups, dict[i,]$prob, dict[i,]$id2)
      # perm_prob = ['index', 'permutation', 'id2', 'prob']
      for(j in 1:nrow(perm_prob)){
        perm_index <- match(perm_prob[j,]$id2, dict$id2, nomatch = NA)
        if(!is.na(perm_index) & is.na(dict[perm_index,]$prob)){
          dict[perm_index,]$prob <- perm_prob[j,]$prob
        }
      }
    }
  }
  return (as_tibble(dict))
}
