
# To
decDismantle <- function(.dec){

  dismantledDec <- list()

  dismantledDec$Variables <- .dec %>%
    stringr::str_replace_all("\\%\\*\\%|\\*", "\\'\\,\\'") %>%
    paste0("c('",., "')") %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy() %>%
    stringr::str_replace_all(" ", "")

  dismantledDec$Operators <- .dec %>%
    stringr::str_match_all("\\%\\*\\%|\\*") %>%
    unlist()

  dismantledDec
}

innermap <- function(x, .f, ..., distance = 1L) purrr::map2(.x= x[1:(length(x)-distance)],
                                                            .y = x[(1+distance):length(x)],
                                                            .f,...)

innermap_lgl <- function(x, .f, ..., distance = 1L) purrr::map2_lgl(.x= x[1:(length(x)-distance)],
                                                            .y = x[(1+distance):length(x)],
                                                            .f,...)

swap <- function(true.x, fake.x, .LHS = "y", .f0, ...){
.f0 <- rlang::quo(!!.f0)
.LHS0 <- rlang::quo(!!.LHS)
map(.x = fake.x, .f = exec, .fn = !!.f0, !!.LHS0 := true.x, ...)
}
swap(true.x = 2, fake.x = 1:9, .LHS = "a", .f=function(x,a) x*a)
