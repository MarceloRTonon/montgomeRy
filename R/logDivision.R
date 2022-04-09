# The function that returns the montgomery decomposition

logDiv <- function(x,y ){
  stopifnot(!any(x>0 & y<0),
            !any(x<0 & y>0)) # x and y must have the same sign!

  x[x==0]<- ifelse(y[x==0]>=0,
                   +.Machine$double.eps,
                   -.Machine$double.eps)
  y[y==0]<- ifelse(x[y==0]>=0,
                   +.Machine$double.eps,
                   -.Machine$double.eps)

  stopifnot(!any(c(any(x==0),
                   any(y==0),
                   any(x>0 & y<0),
                   any(x<0 & y>0))
               ))
  return(log(x/y))
}
