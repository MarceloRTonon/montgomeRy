#' @importFrom purrr discard
#' @importFrom purrr is_empty
hadOnly<- function(.lData, .lFun){
  #pela amor de deus tonon comenta o que tu quer fazer em cada função até a gnt terminar isso aqui
  
  hadOrigIndex <- which(as.character(.lFun)=="*")
  hadUsedIndex <- hadOrigIndex-(1:length(hadOrigIndex)-1)

  ldataSemi <- .lData
  for(index in hadUsedIndex){
    
    nameOfTemp <- paste0(names(ldataSemi[index]), "_times_", names(ldataSemi[index+1]))
    tempVec <- list(hmatrix_expansion(ldataSemi[[index]], ldataSemi[[index+1]])) %>%
      setNames(nameOfTemp)
    ldataSemi <- ldataSemi[0:(index-1)] %>%
      append(values = tempVec) %>%
      append(ldataSemi[(index+2:length(ldataSemi))]) %>%
      discard(is_empty)
  }

  ldataSemi
}
