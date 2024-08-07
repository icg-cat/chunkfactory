## peça 1: defineix paràmetres
#' Defineix paràmetres per l'anàlisi
#'
#' @param vd nom o vector de noms de les variables dependents
#' @param vi nom o vector de noms de les variables independents
#' @param d nom del conjunt de dades
#' @param w nom de la variable de ponderació
#'
#' @return llista amb els noms i l'ordre dels arguments tal i com ho requereix purrr::pmap i les funcions d'anàlisi
#' @export
#'
#' @examples
#' fes_param_list(c("bill_length_mm", "bill_depth_mm"), c("species"), "penguins")
fes_param_list <- function(vd, vi, d, w){
  param <- expand.grid(
    dades = d,
    VD    = vd,
    VI    = vi,
    pes   = w
  )

  param <- lapply(param, function(x){
    as.list(as.character(x))
  })

  return(param)

}
