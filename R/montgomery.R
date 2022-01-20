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


 RankIndex <- map(1:2, append, fListNum) %>%
   map(accumulate, `+`) %>%
   transpose() %>%
   map(flatten_dbl)

 simplifyInDepth <- function(x) map_depth(x, -2, simplify2array)

  yearOutput <- .dismantledDec$Variables %>%
    purrr::map(~ .l[[.x]]) %>%
    setNames(.dismantledDec$Variables) %>%
    purrr::transpose() %>% # Supondo tudo como %*% nesse momento
    map(reduce2, fList,
        function(x1,x2,f0) rlang::exec(.fn = f0, x1, x2))

 yearOutput %>% vec_depth() %>% `:`(1)


}

listA <- list("A" = list("t0" = matrix(runif(6), ncol=3), "t1" = matrix(runif(6), ncol=3)),
              "B" = list("t0" = matrix(runif(27), nrow=3), "t1" = matrix(runif(27), nrow=3)),
              "C" = list("t0" = matrix(runif(9), nrow=3), "t1" = matrix(runif(9), nrow=3)),
              "D" = list("t0" = matrix(runif(27), nrow =3), "t1"= matrix(runif(27), nrow=3)))


listA %>% montgomeryDecompostion("A%*%C%*%B")

testeA <- listA %>%
  montgomeryDecompostion("A%*%C%*%B")

testeA0 <- listA %>%
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
