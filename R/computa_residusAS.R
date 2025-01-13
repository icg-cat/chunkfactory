#' Computes weighted crosstabs, including adjusted standardized residuals.
#'
#' @param VI Idependent variable (rows), as character string
#' @param VD Dependent variable (columns), as character string
#' @param data dataset
#' @param pes Weights variable, as character string
#'
#' @return tibble with 2 columns: key1 és la clau primària, una combinació de les categories de les VI & VD, ASres és el valor dels residus ajustats i estandarditzats, segons es calculen a chisquare::chisquare() "stand.res[i,j]/sqrt((1−sr[i]/n)∗(1−sc[j]/n)). Els resultats d'aquesta funció estan pensats per fer servir en combinació amb "
#' @export
#'
#' @examples
#' data(package = "palmerpenguins", "penguins")
#' penguins$pes <- 1
#' computa_residusAS(data = penguins, VI = "species", VD = "sex", pes = "pes")

computa_residusAS <- function(data, VI, VD, pes){
  # fes una taula de freqüències ponderada
  t1 <- questionr::wtd.table(
    x = data[[VI]],
    y = data[[VD]],
    weights = data[[pes]],
    useNA = "no"
  ) %>%
    as.data.frame.matrix(.)

  # computa chisquare
  t2 <- extraccio_RAE(t1) # versió simplificada, només residus ajustats estand.

  # extreu resultats chisquare
  t3 <- t2 %>%
    dplyr::mutate(rownames = rownames(t2)) %>%
    tidyr::pivot_longer(-rownames,
                 names_to = glue::glue("VD"),
                 values_to = "ASres") %>%
    tidyr::unite("key1", -"ASres", na.rm = T)

  return(t3)

}

# t2 <- chisquare::chisquare(t1, B = 50) # versió inicial amb chisquare
# computa_residusAS <- function(VI, VD, data, pes){
#   # browser()
#   dt <- get(data)
#   # fes una taula de freqüències ponderada
#   t1 <- questionr::wtd.table(
#     x = dt[[VI]],
#     y = dt[[VD]],
#     weights = dt[[pes]],
#     useNA = "no"
#   ) %>%
#     as.data.frame.matrix(.)
#
#   # computa chisquare
#   t2 <- chisquare::chisquare(t1, B = 50)
#
#   # extreu resultats chisquare
#   t3 <- t2$post.hoc$adj.stand.resid %>%
#     mutate(rownames = rownames(t2$post.hoc$adj.stand.resid)) %>%
#     pivot_longer(-rownames,
#                  names_to = glue::glue("{VI}"),
#                  values_to = "ASres") %>%
#     tidyr::unite("key1", -"ASres", na.rm = T)
#
#   return(t3)
#
# }
