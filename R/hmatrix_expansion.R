# Matrix Hadamard Multiplication  Expansion
hmatrix_expansion <- function(x,y){

  isScalar <- list(x,y) %>%
    purrr::when(purrr::some(.,
                            rlang::is_scalar_double) ~ TRUE,
                purrr::some(.,
                             rlang::is_scalar_integer) ~ TRUE) %>%
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

  dimComTest <- list(dim_x, dim_y) %>%
    purrr::map(rev) %>%
    purrr::map_at(.at =1, `[`, 1:length(dim_y)) %>%
    purrr::reduce(`==`) %>%
    all()

  if(!dimComTest) stop("The x and y do not have compatible dimensions.")
  difMarg <- list(dim_x, dim_y) %>%
    purrr::map(length) %>%
    reduce(`-`)

  if(difMarg <0 ) stop("y has higher margin than x.")
  if(difMarg ==0)    return(x*y)

  PermOrder <- difMarg %>%
   `+`(1) %>%
    `:`(length(dim_x)) %>%
    purrr::map(function(x) c(x, 1:(x-1)))

  x %>%
    purrr::array_tree(margin = 1:difMarg) %>%
    purrr::map_depth(-1, `*`, y) %>%
    list(.) %>%
    append(PermOrder) %>%
    purrr::reduce(function(x,y) map_depth(x,
                                          .depth = -2,
                                          function(l) aperm(a= simplify2array(l),
                                                            perm=y)))



}
