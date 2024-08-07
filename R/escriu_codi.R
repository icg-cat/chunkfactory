#' funció que escriu codi dins d'un chunk, i amb títols 3 per cada secció
#'
#' @param reslist_mytab llista de resultats d'aplicar la fàbrica de codi, a les passes anteriors
#'
#' @return vector caràcter amb el markdown i el codi que han d'interpretar-se posteriorment per  knitr::knit_child(text = unlist(src))
#' @export
#'
#' @examples
#' utils::data(package = "palmerpenguins", "penguins")
#' # defineix els paràmetres per alimentar a la funció d'anàlisi
#' param <- fes_param_list(c("species"), c("island"), "penguins")
#' # extreu resultats en llista
#' reslist_mytab <- aplica_funcio(param)
#' # extreu el codi amb resultats
#' src <- escriu_codi(reslist_mytab)
escriu_codi <- function(reslist_mytab){
  src <- purrr::map_chr(seq_along(reslist_mytab), ~ {
    tab_name <- names(reslist_mytab[.x])
    knitr::knit_expand(text = c(
      "\n",
      "### {{tab_name}}\n",
      "```{r echo = TRUE}\n",
      "eval(parse(text = (reslist_mytab[[{{.x}}]][[1]])))",
      "eval(parse(text = (reslist_mytab[[{{.x}}]][[2]])))",
      "```",
      "\n"))
  })

  return(src)
}
