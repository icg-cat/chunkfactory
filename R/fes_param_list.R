#' Define a list of parameters for descriptive statistics functions, in the order needed by the followong functions, notably purrr::pmap.
#'
#' @param d name of the dataset
#' @param vi name of the independent variable
#' @param vd name of the dependent variable
#' @param w name of the weighting variable
#'
#' @return list with all the combinations of the different elements
#' @export
#'
#' @examples
#' fes_param_list("penguins", c("species"), c("bill_length_mm", "bill_depth_mm"), w = "pes")
#'
fes_param_list <- function(d, vi, vd, w){
  param <- expand.grid(
    dades = d,
    VI    = vi,
    VD    = vd,
    pes   = w
  )

  param <- lapply(param, function(x){
    as.list(as.character(x))
  })

  return(param)

}
