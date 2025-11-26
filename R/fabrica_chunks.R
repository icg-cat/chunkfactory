#' fabricate chunks, with section titles, chunks delimitations, and code.
#'
#' @param d character vector with name of dataset
#' @param vi character vector with name of independent variable
#' @param vd character vector with name of dependent variable
#' @param w character vector with name of weighting variable
#'
#' @return markdown per generar seccions (sota títol 3), chunks i codi que serà posteriorment renderitzat en un rmd.
#' @export
#'
#' @examples
#' utils::data(package = "palmerpenguins", "penguins")
#' penguins$pes <- 1
#' fabrica_chunks("penguins", c("sex", "species"), c("island"), "pes")
fabrica_chunks <- function(d, vi, vd, w){
  # defineix els paràmetres per alimentar a la funció d'anàlisi
  param <- fes_param_list(d = d, vi = vi, vd = vd, w = w)
  # extreu resultats en llista
  reslist_mytab <- aplica_funcio(param)
  # extreu el codi amb resultats
  src <- escriu_codi(reslist_mytab)
  # retorna markdown per compilar
  return(src)
}
