#' function that writes markdown and code
#'
#' @param reslist_mytab list of results produced by code factories in previous steps
#' @param title_level level of the titles for the sections. Defaults to Header 3
#'
#' @return character vector containing the markup and code later interpreted by knitr::knit_child(text = unlist(src))
#'
#' @examples
#' utils::data(package = "palmerpenguins", "penguins")
#' penguins$pes <- 1
#' # define function parameters
#' param <- fes_param_list("penguins", c("species"), c("island"), w = "pes")
#' # extract results into a list
#' reslist_mytab <- chunkfactory:::aplica_funcio(param)
#' # extract code with results
#' src <- chunkfactory:::escriu_codi(reslist_mytab)
escriu_codi <- function(reslist_mytab, title_level = 3){
  src <- purrr::map_chr(seq_along(reslist_mytab), ~ {
    tab_name <- names(reslist_mytab[.x])
    n_hash <- paste0(rep("#", title_level), collapse="")
    knitr::knit_expand(text = c(
      "\n",
      "{{n_hash}} {{tab_name}}\n",
      "```{r echo = TRUE}\n",
      "eval(parse(text = (reslist_mytab[[{{.x}}]][[1]])))",
      "eval(parse(text = (reslist_mytab[[{{.x}}]][[2]])))",
      "```",
      "\n"))
  })

  return(src)
}
