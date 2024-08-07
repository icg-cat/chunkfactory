#' @description Calcula una taula de contingència amb residus ajustats estandarditzats, fent servir NSE. Es donen només els % sobre els marginals de la VI.
#' @title taula de contingència a mida
#' @param dades dades
#' @param VD variable dependent (NSE)
#' @param VI variable independent (NSE)
#' @param pes pes (NSE)
#'
#' @return una tibble amb els recomptes (n), els recomptes ponderats (N), els totals de VI (TT), els percentatges (PP) i els residus ajustats estandarditzats
#' @export
#'
#' @examples
#'data(package = "palmerpenguins", "penguins")
#'penguins$pes <- 1
#'mycrosstab(penguins, island, sex, pes)

mycrosstab <- function(dades, VD, VI, pes){
  r1 <- fes_taula_nse(dades, VD, VI, pes)
  r2 <- computa_residusAS_nse(dades, VI, VD, pes)

  r3 <- dplyr::left_join(r1, r2) %>%
    tidyr::separate("key1", into = c("VI", "VD"))

  return(r3)
}
