#' select and execute analysis function for bivariate descriptive analysis
#'
#' @description second piece in the chunk factory. Select the function according to the class of the dependent variable.
#' @param param vector of parametres for the function.
#'
#' @return character vector caracter with the code to be executed.
#' @export
#'
#' @examples
#' data(package = "palmerpenguins", "penguins")
#' param <- expand.grid(
#'   dades = "penguins",
#'   VI    = c("island"),
#'   VD    = c("bill_length_mm", "bill_depth_mm"),
#'   pes   = "pes") %>%
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

  # assigna funcio i executa
  reslist_mytab <- purrr::pmap(param, myfunc)
  names(reslist_mytab) <- paste(param$VD, "x", param$VI)

  # desa reslist_mytab al Global.env
  assign("reslist_mytab", reslist_mytab, envir = .GlobalEnv)

  return(reslist_mytab)

}
