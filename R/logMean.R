logMean <- function(x,y){

    stopifnot(!any(x>0 & y<0),
              !any(x<0 & y>0)) # x and y must have the same sign!

  # Changing Zero values to .Machine$double.eps
  x[x==0]<- ifelse(y[x==0]==0, 0, # if both x and y are 0, the values stays 0
                   ifelse(y[x==0&y!=0]>0,
                   .Machine$double.eps,
                   -.Machine$double.eps))
  y[y==0]<- ifelse(x[y==0]==0, 0,
                   ifelse(x[y==0& x!=0]>0,
                          .Machine$double.eps,
                          -.Machine$double.eps))

  list(x,y) %>%
    transpose() %>%
    purrr::map_if(.p = ~ .x[[1]] != .x[[2]], # The values are different
                  .f = function(x) (x[[1]]-x[[2]])/log(x[[1]]/x[[2]]),
                  .else = ~  .x[[1]]) # If the values are the same, they stay the same.
}
