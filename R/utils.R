
#require(magrittr)
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

# swap is useful for when you are piping x but suddenly need to map y as .x and x as an argument.

swap <- function(true.x, fake.x, true.LHS = "y", .f0, ...){

  .f0 <- rlang::quo(!!.f0) %>%
    rlang::quo_get_expr()


  true.LHS <- rlang::enexpr(true.LHS)

  true.xArg <- list(true.x) %>%
  set_names(true.LHS)


purrr::map(.x = fake.x,
           .f = rlang::exec,
           .fn = .f0, !!!true.xArg, ...)
}

