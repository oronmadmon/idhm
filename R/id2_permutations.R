id2_permutations <- function(risk_groups, id2){
  id_2 <- strsplit(id2, split=',')[[1]]
  permutations_dict <- permutations(risk_groups)
  perm_dict <- permutations_dict[-nrow(permutations_dict),c('index', 'permutation')]
  perm_dict$id2 <- apply(perm_dict['permutation'], 1, function(r) paste0(id_2[c(unlist(r),unlist(r)+risk_groups)], collapse=','))
  perm_dict <- perm_dict[!duplicated.data.frame(perm_dict['id2']),]
  row.names(perm_dict) <- 1:nrow(perm_dict)
  return (perm_dict)
}
