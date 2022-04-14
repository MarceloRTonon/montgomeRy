#' @importFrom purrr map_lgl
.compile_matrices <- function(.loa, .ranks){

  stopifnot(length(.loa)==length(.ranks))

  targetRank <- list(r1 = min(unique(unlist(.ranks))),
                     rn = max(unique(unlist(.ranks))))

  allRanks <- .ranks %>%
    unlist %>%
    unique

  stopifnot(sort(allRanks) == allRanks)

  eOrder <- .ranks %>%
    map(function(x) c(x, rev(allRanks[!(allRanks %in% x)])))

  rnBefore <- targetRank %>%
    map(function(x) map(eOrder, `%in%`, x)) %>%
    map(map, which) %>% transpose %>%
    map_lgl(reduce, `>`)


  rTobranch <- eOrder %>%
    map(`%in%`, unlist(targetRank)) %>% map(`!`) %>%
    map(which)

  lFn <- list(function(x) map_depth(x, .depth = -2, simplify2array)) %>%
    rep(vec_depth(.loa)-2)

  output <- .loa %>%
    map(compose(!!!lFn)) %>%
    map2(rTobranch, array_branch) %>%
    map(reduce, `+`) %>%
    map_at(.at = which(rnBefore), t)
}

