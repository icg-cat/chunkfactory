#' fabrica els chunks, amb els seus títols, chunks i codi
#'
#' @param vd vector de noms de les variables dependents
#' @param vi vector de noms de les variables independents
#' @param d nom de les dades, com a cadena de caràcter
#' @param w nom de la variable de ponderació
#'
#' @return markdown per generar seccions (sota títol 3), chunks i codi que serà posteriorment renderitzat en un rmd.
#' @export
#'
#' @examples
#' utils::data(package = "palmerpenguins", "penguins")
#' fabrica_chunks(c("sex", "species"), c("island"), "penguins")
fabrica_chunks <- function(vd, vi, d, w){
  # defineix els paràmetres per alimentar a la funció d'anàlisi
  param <- fes_param_list(vd, vi, d, w)
  # extreu resultats en llista
  reslist_mytab <- aplica_funcio(param)
  # extreu el codi amb resultats
  src <- escriu_codi(reslist_mytab)
  # retorna markdown per compilar
  return(src)
}
