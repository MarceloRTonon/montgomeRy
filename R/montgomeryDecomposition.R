montgomeryDecompostion <- function(.l, .decFormula){
  require(magrittr)
  require(rlang)
  require(purrr)
  # The structure of .l must be first the variables, then time: list$vars$time
  stopifnot(!(purrr::is_null(names(.l))))

  if(stringr::str_detect(.decFormula, "\\+|\\(|\\)|\\-|\\[")){
    stop("These operators are not supported in this package")
  }

  .dismantledDec <- .decFormula %>%
    decDismantle()

  if((".l" %in% .dismantledDec$Variables) | (".l" %in% names(.l))){
    stop(".l is a forbiden name in this function")
  }
  .varsInDec <- .dismantledDec$Variables %in%
    names(.l)

  if(!all(.varsInDec)){
    stop(
      paste0("Not all variables in .DecFormula are in the `.l` argument. \n The missing vars are: ",
             stringr::str_c(.dismantledDec$Variables[!.varsInDec], collapse = ";"),
             ".")
    )

  }

  VarsDim <- .dismantledDec$Variables %>%
    purrr::map(~ .l[[.x]]) %>%
    setNames(.dismantledDec$Variables) %>%
    purrr::map(purrr::map, dim)

  VarsDim %>%
    purrr::map(function(x) purrr::map2(x[1:(length(x)-1)],
                                       x[2:length(x)],
                                       all.equal)) %>%
    purrr::map(purrr::map_lgl, isTRUE) %>%
    purrr::map_lgl(all) %>% all %>%
    `!` %>% if(.) stop("The variables have different dimensions in different years")


  VarsDim %>%
    purrr::transpose() %>%
    purrr::pluck(1) %>%
    purrr::map(as.list) %>%
    purrr::transpose() %>%
    innermap_lgl(~.x[[2]]==.y[[1]]) %>%
    all() %>%
    `!` %>%
    if(.)  stop("The dimensions of the variables in .l are incompatible with the decomposition.")

 fList <- .dismantledDec$Operators
 fList[fList == "%*%"] <- "mmatrix_expansion"
 fList[fList == "*"] <- "hmatrix_expansion"

 fListNum <- fList
 fListNum[fListNum == "mmatrix_expansion"] <- "1"
 fListNum[fListNum == "hmatrix_expansion"] <- "0"
 fListNum <- fListNum %>%
   as.numeric()


 rankIndex <- map(1:2, append, fListNum) %>%
   map(accumulate, `+`) %>%
   transpose() %>%
   map(flatten_dbl) %>%
   setNames(.dismantledDec$Variables)

 min2maxRank <- c(min(unlist(rankIndex)):max(unlist(rankIndex)))

 revRankIndex <- rankIndex %>%
   map(~ (min2maxRank %in% .x)) %>%
   map(~min2maxRank[!.x])


 simplifyInDepth <- function(x) map_depth(x, -2, simplify2array)

 if(all(fListNum==1)){
   yearOutput <-.dismantledDec$Variables %>%
     purrr::map(~ .l[[.x]]) %>%
     setNames(.dismantledDec$Variables) %>%
     purrr::transpose() %>% # Supondo tudo como %*% nesse momento
     map(reduce2, fList,
         function(x1,x2,f0) rlang::exec(.fn = f0, x1, x2))


 }

  logMeanedOutput <- yearOutput %>%
    reduce(logMean)

  logMeaned_treed_byVars <- revRankIndex %>%
    map(function(index) purrr::array_tree(logMeanedOutput, margin = index)) %>%
    setNames(.dismantledDec$Variables)

 lnVars <- .dismantledDec$Variables %>%
  purrr::map(~ .l[[.x]]) %>%
   setNames(.dismantledDec$Variables) %>%
   map(reduce, logDiv)


 MontDecRaw <- lnVars %>%
   map2(logMeaned_treed_byVars,
        function(x,y) map_depth(y, .depth = -1, `*`, x))

 # Falta criar agora uma versão que devolve o MontDec somado para cada varíavel (agregadão), bem como o montDec para cada valor do produto (sem estar expandido)

 # lista para ir testando
  list(logMeanedOutput = logMeanedOutput,
       yearOutput = yearOutput,
       rankIndex = rankIndex,
       VarsDim = VarsDim,
       decVars = .dismantledDec$Variables,
        revRankIndex = revRankIndex,
        logMeaned_treed_byVars = logMeaned_treed_byVars,
       lnVars = lnVars,
       MontDecRaw= MontDecRaw
       )
}

listA <- list("A" = list("t0" = matrix(runif(6), ncol=3), "t1" = matrix(runif(6), ncol=3)),
              "B" = list("t0" = matrix(runif(27), nrow=3), "t1" = matrix(runif(27), nrow=3)),
              "C" = list("t0" = matrix(runif(9), nrow=3), "t1" = matrix(runif(9), nrow=3)),
              "D" = list("t0" = matrix(runif(27), nrow =3), "t1"= matrix(runif(27), nrow=3)))

# testA = yearOutput arrays must have the dims of c(2,3,3,9)
testA0 <- listA %>%
  montgomeryDecompostion("A%*%C%*%B")

testA1 <- listA %>%
  montgomeryDecompostion("A%*%C")

listB <- list("A" =  matrix(runif(6), ncol=3),
              "C" =  matrix(runif(9), nrow=3))

x <- listB$A
y <- listB$C

x%*%y
mmatrix_expansion(x,y)

listC <- list("A" = list("t0" = matrix(runif(6), ncol=3), "t1" = matrix(runif(6), ncol=3)),
              "C" = list("t0" = matrix(runif(9), nrow=3), "t1" = matrix(runif(9), nrow=3)),
              "B" = list("t0" = matrix(runif(27), nrow=3), "t1" = matrix(runif(27), nrow=3))) %>%
  purrr::map(`[[`, "t0")

reduce(listC, mmatrix_expansion)

listD <- listC %>%
  reduce(mmatrix_expansion) %>%
  list(.) %>%
  append(list(matrix(runif(27), ncol =9)))
