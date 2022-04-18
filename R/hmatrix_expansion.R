# Matrix Hadamard Multiplication  Expansion
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#' @importFrom purrr some
#' @importFrom rlang is_scalar_double
#' @importFrom rlang is_scalar_integer
#' @importFrom purrr every
#' @importFrom purrr when
#' @importFrom purrr map_at
#' @importFrom purrr compose
#' @examples
hmatrix_expansion <- function(x,y){

  isScalar <- list(x,y) %>%
    when(some(.,
                            is_scalar_double) ~ TRUE,
                some(.,
                             is_scalar_integer) ~ TRUE) %>%
    is.null() %>%
    `!`

    if(isScalar) return(x*y)


  areBare <- list(x,y) %>%
    every(is.vector) %>%
    when(. ~ reduce(map(list(x,y),length), `==`),
         isFALSE(.) ~ "Doesn't Apply")

  if(isTRUE(areBare)) return(x*y)
  if(isFALSE(areBare)) stop("x and y are atomic vectors with different lengths!")


  dim_x <- dim(x)
  dim_y <- dim(y)

  # dimComTest is as it is because dim_x and dim_y can have different margins
  dimComTest <- list(dim_x, dim_y) %>%
    map(rev) %>%
    map_at(.at =1, `[`, 1:length(dim_y)) %>%
    reduce(`==`) %>%
    all()

  if(!dimComTest) stop("The x and y do not have compatible dimensions.")
  difMarg <- list(dim_x, dim_y) %>%
    map(length) %>%
    reduce(`-`)

  if(difMarg <0 ) stop("y has higher margin than x.")
  if(difMarg ==0)    return(x*y)

  PermOrder <- difMarg %>%
   `+`(1) %>%
    `:`(length(dim_x)) %>%
    map(function(x) c(x, 1:(x-1)))

  x %>%
    array_tree(margin = 1:difMarg) %>%
    map_depth(-1, `*`, y) %>%
    list(.) %>%
    append(PermOrder) %>%
    reduce(function(x,y) map_depth(x,
                                          .depth = -2,
                                          function(l) aperm(a= simplify2array(l),
                                                            perm=y)))



}
