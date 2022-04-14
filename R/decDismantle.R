#' @importFrom rlang eval_tidy
#' @importFrom rlang parse_expr
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_match_all
decDismantle <- function(.dec){

  dismantledDec <- list()

  dismantledDec$Variables <- .dec %>%
    stringr::str_replace_all("\\%\\*\\%|\\*", "\\'\\,\\'") %>%
    paste0("c('",., "')") %>%
    parse_expr() %>%
    eval_tidy() %>%
    str_replace_all(" ", "")

  dismantledDec$Operators <- .dec %>%
    str_match_all("\\%\\*\\%|\\*") %>%
    unlist()

  dismantledDec
}
