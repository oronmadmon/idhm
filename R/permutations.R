permutations <- function(n, original_call=TRUE){
  if( (floor(n)!=n) | (n<1) ) stop('n must be a natural number')
  perms_list <- vector(mode="list", length=factorial(n))
  if (n==1) {
    perms_list[1] <- list(1)
    return(perms_list)
  }
  prev_list <- permutations(n-1, original_call = FALSE)
  ind <- 1
  for(perm in prev_list){
    perms_list[[ind]] <- append(n, perm)
    ind <- ind+1
    for(loc in 2:n){
      perms_list[[ind]] <- append(perm, n, after=loc-1)
      ind <- ind+1
    }
  }
  if (isFALSE(original_call)){return(perms_list)}
  perms_df <- data.frame(index=1:factorial(n))
  perms_df$permutation <- apply(perms_df['index'], 1, function(r) as.list(perms_list[[r]]))
  return(perms_df)
}
