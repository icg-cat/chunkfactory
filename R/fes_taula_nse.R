#' Fes una taula de contingència ponderada, fent servir NSE
#'
#' @param dades dades
#' @param VD variable dependent (NSE)
#' @param VI variable independent (NSE)
#' @param pes pes (NSE)
#'
#' @return una tibble amb els recomptes (n), els recomptes ponderats (N), els totals de VI (TT), els percentatges (PP)
#' @export
#'
#' @examples
#'data(package = "palmerpenguins", "penguins")
#'penguins$pes <- 1
#'fes_taula_nse(penguins, island, sex, pes)
fes_taula_nse <- function(dades, VD, VI, pes){
  dades %>%
    dplyr::select({{pes}}, {{VD}}, {{VI}}) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::group_by({{VI}}) %>%
    dplyr::mutate(TT = sum(pes),
                  tt = n()) %>%
    dplyr::group_by({{VD}}, .add = T) %>%
    dplyr::summarise(
      N = sum({{pes}}),
      n = n(),
      TT = unique(TT),
      PP = round(N/TT*100, 2)
    ) %>%
    tidyr::unite("key1", c(1,2), na.rm = T)
}
