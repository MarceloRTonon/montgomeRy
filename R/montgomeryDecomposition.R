#' Montgomery Decomposition for Input-Output model
#'
#' @param .l A list with the variables from the equation to be decomposed.
#' @param .decFormula A string that contains
#' @param .output The desired display form of the decomposition results. Must be a character. See details for more information
#' @return The output of the function depends on the
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr map2
#' @importFrom purrr array_branch
#' @importFrom purrr map_depth
#' @importFrom purrr map_dbl
#' @importFrom purrr vec_depth
#' @importFrom purrr reduce
#' @importFrom rlang squash
#' @importFrom purrr array_tree
#' @importFrom purrr accumulate
#' @importFrom rlang flatten_dbl
#' @importFrom purrr discard
#' @importFrom purrr map_lgl
#' @importFrom purrr transpose
#' @importFrom purrr pluck
#' @importFrom purrr prepend
#' @importFrom purrr map_lgl
#' @importFrom rlang is_empty
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stats setNames
#' @importFrom stringr str_replace_all
#' @export
#'
#' @description TO INCLUDE. A description of the Montgomery Decomposition
#'
#' @details TO INCLUDE. The details of how the function works.
#'
#'
#'
#' @return
#'
#' The function output value will vary accordingly to the .output parameter.
#'
#'  * If .output parameter is "default" or, for shortcut, "D", the function will return a list with the contributions of each variable in a matrices with dimensions of the model output.
#'
#'  * If .output parameter is "aggregated" or, for shortcut, "Agg", the function will return a list with the aggregated value for each variable.
#'
#'  * If .output parameter is "vars" or, for shortcut, "V", the function will return a list with the contribution of each variable in the matrices in their dimensions.
#'
#'  * If .output parameter is "raw" or, for shortcut, "R", the funciton will return list with the contribuitions of each variable in an array with all the margins.
#'
#'  * If .output parameter is "all" or, for shortcut, "A", the function will return a list containg all of the cases above.
#'
#'  If the .output parameter is not given it will be set to "default". If one is given, but is it is none of the above, a warning will be displayed and the .output will be set to "all".
#' @examples
#' lofVars <- list("A" = list("t0" = matrix(runif(8), ncol=4), "t1" = matrix(runif(8), ncol=4)),
#'                 "C" = list("t0" = matrix(runif(27), nrow=3), "t1" = matrix(runif(27), nrow=3)),
#'                 "B" = list("t0" = matrix(runif(12), nrow=4), "t1" = matrix(runif(12), nrow=4)),
#'                 "D" = list("t0" = matrix(runif(27), nrow =3), "t1"= matrix(runif(27), nrow=3)))
#'
#' lofVars %>% montgomeryDecomposition("A%*%B%*%C", "all")
#'
#' lofVars %>% montgomeryDecomposition("A%*%B%*%C*D", "all")
#'
#'
montgomeryDecomposition <- function(.l, .decFormula, .output = "default"){

  if(length(.output)>1){
    stop("The .output. argument cannot have a length higher than 1!")
  }
  if(!is.character(.output)){
    stop(".output must be a character of ")
  }
  validOutputs <- c("default", "D",
                    "raw", "R",
                    "all", "A",
                    "aggregated", "Agg",
                    "vars", "V")
  if(!(.output %in% validOutputs)){
    warning('.output not valid! \n Changing the value to "all". \n Check the documentation to more information.')
  }
  # The structure of .l must be first the variables, then time: list$vars$time
  if(is.null(names(.l))){
    stop(".l must have names!")
    }

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
             str_c(.dismantledDec$Variables[!.varsInDec], collapse = ";"),
             ".")
    )

  }

  VarsDim <- .dismantledDec$Variables %>%
    map(~ .l[[.x]]) %>%
    setNames(.dismantledDec$Variables) %>%
    map(map, dim)

  VarsDim %>%
    map(function(x) map2(x[1:(length(x)-1)],
                                       x[2:length(x)],
                                       all.equal)) %>%
    map(map_lgl, isTRUE) %>%
    map_lgl(all) %>% all %>%
    `!` %>% if(.) stop("The variables have different dimensions in different years")


  VarsDim %>%
    transpose() %>%
    pluck(1) %>%
    map(as.list) %>%
    transpose() %>%
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

 revRankIndex <- rankIndex |>
   map(~ (min2maxRank %in% .x)) |>
   map(~min2maxRank[!.x])


 if(any(fListNum==0)){
   hadOnly<- function(.lData, .lFun){
     hadOrigIndex <- which(fListNum==0)
     hadUsedIndex <- hadOrigIndex-(1:length(hadOrigIndex)-1)

     ldataSemi <- .lData
     for( index in hadUsedIndex){
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
   yearOutput<- .dismantledDec$Variables %>%
     map(~ .l[[.x]]) %>%
     setNames(.dismantledDec$Variables) %>%
     transpose() %>%
     map(hadOnly, .lFun = fListNum) %>%
     map(reduce, mmatrix_expansion)

 }else{
   yearOutput <-.dismantledDec$Variables %>%
     map(~ .l[[.x]]) %>%
     setNames(.dismantledDec$Variables) %>%
     transpose() %>% # Supondo tudo como %*% nesse momento
     map(reduce, mmatrix_expansion)

 }


  logMeanedOutput <- yearOutput %>%
    reduce(logMean)

  logMeaned_treed_byVars <- revRankIndex %>%
    map(function(index) array_tree(logMeanedOutput, margin = index)) %>%
    setNames(.dismantledDec$Variables)


 lnVars <- .dismantledDec$Variables %>%
  map(~ .l[[.x]]) %>%
   setNames(.dismantledDec$Variables) %>%
   map(reduce, logDiv)


 MontDecRaw <- lnVars %>%
   map2(logMeaned_treed_byVars,
        function(x,y) map_depth(y, .depth = -1, `*`, x))


 if(.output %in% c("raw", "R")){
   output <- MontDecRaw
 }

 if(.output %in% c("all", "A", "default", "D")){
   MontDecDefault <- MontDecRaw %>%
     .compile_matrices(rankIndex)
   if(.output %in% c("default", "D")){
     output <- MontDecDefault
   }
   if(.output %in% c("all", "A", "aggregated", "Agg")){
     MontDecAgg <- MontDecDefault %>%
       map(sum)
     if(.output %in% c("aggregated", "Agg")){
       output <- MontDecAgg
     }
   }

 }

 if(.output %in% c("all", "A", "vars", "V")){
   MontDecVars <- MontDecRaw %>%
     map(squash) %>%
     map(reduce, `+`)
   if(.output %in% c("vars", "V")){
     output <- MontDecVars
   }
 }

 if(.output %in% c("all", "A")){
   output <- list(default = MontDecDefault,
                  aggregated = MontDecAgg,
                  vars = MontDecVars,
                  raw = MontDecRaw)
 }
  return(output)
}


