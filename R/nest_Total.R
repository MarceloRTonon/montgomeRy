nest_Total <- function(.l, .NestedEquation){
  if(is.null(names(.l))){
    stop(".l must have names!")
  }

  list2env(.l, envir = environment())

  eval(parse(text = .NestedEquation))
}
