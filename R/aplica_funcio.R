#' selecciona i aplica la funció d'anàlisi a executar.
#' @description segona peça de la fàbrica de chunks. Tria la funció a aplicar segons si les variables dependents són qualitatives o quantitatives.
#' @param param vector de paràmetres per la funció. Creat al pas anterior
#'
#' @return vector caràcter amb el codi a executar, amb els noms de les variables a utilitzar
#' @export
#'
#' @examples
#' utils::data(package = "palmerpenguins", "penguins")
#' param <- expand.grid(
#'   dades = "penguins",
#'   VD    = c("bill_length_mm", "bill_depth_mm"),
#'   VI    = c("island"),
#'   pes   = NULL) %>%
#'   lapply(., function(x){as.list(as.character(x))})
#' aplica_funcio(param)
aplica_funcio <- function(param){
  # browser()
  # checks integritat dades
  classe_vds <- lapply(param$VD, function(x){
    class(get(param$dades[[1]])[[x]])
  }) %>% unlist(.)

  classe_vis <- lapply(param$VI, function(x){
    class(get(param$dades[[1]])[[x]])
  }) %>% unlist(.)

  if(!(all(classe_vds == "factor") |
     all(classe_vds %in% c("numeric", "integer", "double")))) stop("Les variables dependents no tenen totes la mateixa classe. Cal proporcionar un vector de variables factor o numeric")

  if(!all(classe_vis == "factor"))  stop("Les variables independents han de ser totes de classe factor")

  # identifica classe de la primera VD
  myfunc <- ifelse(is.factor(get(param$dades[[1]])[[param$VD[[1]]]]), mytab_txt, quant_txt)

  # assigna funció i executa
  reslist_mytab <- purrr::pmap(param, myfunc)
  names(reslist_mytab) <- paste(param$VD, "x", param$VI)

  # desa reslist_mytab al Global.env
  assign("reslist_mytab", reslist_mytab, envir = .GlobalEnv)

  return(reslist_mytab)

}
