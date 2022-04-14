# Matrix multiplicatin expansion
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
mmatrix_expansion <- function(x,y){

  if(is.null(dim(x)) | (length(dim(x)) ==1)){
    x <- as.matrix(x, nrow = length(x))
  }

  if(is.null(dim(y)) | (length(dim(y)) ==1)){
    x <- as.matrix(y, nrow = length(y))
  }

  dimO <- c(dim(x), dim(y)[c(-1)])


  ldimX <- x %>%
    dim() %>%
    length() %>%
    `-`(1) %>%
    `:`(1) %>% rev()

  PermOrder <- 3:length(dimO) %>%
    map(function(x) c(x, 1:(x-1)))


  output.l <-  x %>%
    array_tree(margin = ldimX) %>%
    map_depth(-1, `*`, y) %>%
    list(.) %>%
    append(PermOrder) %>%
    reduce(function(x,y) map_depth(x,
                                          .depth = -2,
                                          function(l) aperm(a= simplify2array(l),
                                                            perm=y)))

  return(output.l)
}
