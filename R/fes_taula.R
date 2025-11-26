#' Compute a weighted crosstab.
#'
#' @param dades dataset
#' @param VI independent variable name (as character string)
#' @param VD dependent variable name (as character string)
#' @param pes weight variable name (as character string)
#'
#' @return a tibble including counts (n), weighted counts (N), marginals of the independent variable (TT), and percentages within independent variable groups (PP = N/TT*100)
#'
#' @export
#' @examples
#'data(package = "palmerpenguins", "penguins")
#'penguins$pes <- 1
#'fes_taula(penguins, "island", "sex", "pes")
fes_taula <- function(dades, VI, VD, pes){
  # browser()
  dades %>%
    dplyr::select(tidyselect::all_of(base::c(VI, VD, pes))) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::group_by(.data[[VI]]) %>%
    dplyr::mutate(TT = base::sum(.data[[pes]]),
                  tt = dplyr::n()) %>%
    dplyr::group_by(.data[[VD]], .add = T) %>%
    dplyr::summarise(
      N = base::sum(.data[[pes]]),
      n = dplyr::n(),
      TT = base::unique(TT),
      PP = base::round(N/TT*100, 2)
    ) %>%
    tidyr::unite("key1", c(1,2), na.rm = T)
}
