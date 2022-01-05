montgomeryDecompostion <- function(.l, .decFormula){
  require(magrittr)
  require(rlang)
  require(purrr)
  # The structure of .l must be first the variables, then time: list$vars$time
  stopifnot(!(purrr::is_null(names(.l))))

  if(stringr::str_detect(.decFormula, "\\+|\\(|\\)|\\-|\\[")){
    stop("These operators are not yet supported in this package")
  }

  .dismantledDec <- .decFormula %>%
    decDismantle()

  if((".l" %in% .dismantledDec$Variables) | (".l" %in% names(.l))){
    stop(".l is a forbiden name in this ")
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
#  purrr::map(~ .x[c(1, length(.x))]) #%>%
#  innermap(~ .x[2]==.y[1])




  mmatrix_expansion <- function(x,y){

    if(!is.array(x)){
      x <- as.matrix(x, nrow = length(x))
    }

    dimO <- c(dim(x), dim(y)[c(-1)])

    ldimX <- x %>%
      dim() %>%
      length() %>%
      `-`(1) %>%
      `:`(1)

    Output.l <-  x %>%
        array_tree(margin = ldimX) %>%
        purrr::map_depth(-1, `*`, y) %>%
      unlist() %>% array(dimO)

  }
  .Vars <- .dismantledDec$Variables %>%
    purrr::map(~ .l[[.x]]) %>%
    setNames(.dismantledDec$Variables) %>%
    purrr::transpose() %>%
    map(innermap, mmatrix_expansion)

return(.Vars)
}

listA <- list("A" = list(matrix(runif(6), ncol=3), matrix(runif(6), ncol=3)),
              "B" = list(matrix(runif(27), nrow=3), matrix(runif(27), nrow=3)),
              "C" = list(matrix(runif(9), nrow=3), matrix(runif(9), nrow=3)))


listA %>% montgomeryDecompostion("A%*%C%*%B")

testeA <- listA %>%
  montgomeryDecompostion("A%*%C%*%B")



testeA <- teste[1:2]

vars <- testeA

