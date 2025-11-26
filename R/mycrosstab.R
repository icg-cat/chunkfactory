#' @description Compute a crosstaband add adjusted standardized residuals. Percentages are only computed using independent variable marginals.
#' @title tailored cross tabulation
#' @param dades dataset
#' @param VI name of the independent variable
#' @param VD name of the dependent variable
#' @param pes name of the weighting variable
#'
#' @return a tibble including counts (n), weighted counts (N), marginals of the independent variable (TT), percentages within independent variable groups (PP = N/TT*100), and adjusted standardized residuals.
#'
#' @examples
#'data(package = "palmerpenguins", "penguins")
#'penguins$pes <- 1
#'chunkfactory:::mycrosstab(penguins, "island", "sex", "pes")

mycrosstab <- function(dades, VI, VD, pes){
  # browser()
  r1 <- fes_taula(dades, VI, VD, pes)
  r2 <- computa_residusAS(dades, VI, VD, pes)

  r3 <- dplyr::left_join(r1, r2) %>%
    tidyr::separate("key1", into = c("VI", "VD"))

  return(r3)
}
