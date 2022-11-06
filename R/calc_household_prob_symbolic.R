calc_household_prob_symbolic <- function(dict,idx,N_names,I_names) {


  ag_num <- length(N_names)

  stopifnot(length(N_names)==ag_num)
  stopifnot(length(I_names)==ag_num)
  stopifnot(idx<=nrow(dict))


  Q <- paste0("Q",1:ag_num) %>% ysym()
  Qtext <- paste0(",{",  paste0("Q",1:ag_num,collapse = ","),"})")


  if(is.na(dict[idx,]$prob)) {

    N <- as.numeric(dict[idx,N_names])
    I <- as.numeric(dict[idx,I_names])
    n <- sum(N)
    k <- sum(I)
    if(k<n) {
      ni <- N-I
      bin_fact <- prod(choose(N,I))
      prob <-  bin_fact*prod( Q^( ni*(k+1) ))
      id <- paste0(c(I,I),collapse=',')
      idx2 <- which(dict$id2==id)
      if(length(idx2) > 0) {
        prob <- prob*dict[idx2,]$prob %>%
          yac_str()
        prob_str <- paste0("Expand(",prob,Qtext)
        prob <- ysym(prob_str)
      }
    } else {
      id <- paste0(N,collapse=',')
      idx2 <- which(dict$id1==id)
      idx2 <- setdiff(idx2,idx)
      prob <- ysym(dict[idx2[1],]$prob)
      if(length(idx2)>1) {
        for(i in 2:length(idx2)) {
          prob <- prob + ysym(dict[idx2[i],]$prob)
          prob_str <- paste0("Expand(",prob,Qtext)
          prob <- ysym(prob_str)
        }
      }
      prob <- 1-prob
    }
    dict[idx,]$prob <- yac_str(prob)
  }
  return (dict)
}
