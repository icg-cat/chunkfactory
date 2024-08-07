#' Computa residus ajustats i estandarditzat per taules de contingència, partint d'una taula ponderada
#'
#' @param VI Variable independent en NSE (a files)
#' @param VD Variable dependent  en NSE (a columnes)
#' @param data Conjunt de dades
#' @param pes Variable amb els pesos en NSE
#'
#' @return tibble amb dues columnes: key1 és la clau primària, una combinació de les categories de les VI & VD, ASres és el valor dels residus ajustats i estandarditzats, segons es calculen a chisquare::chisquare() "stand.res[i,j]/sqrt((1−sr[i]/n)∗(1−sc[j]/n)). Els resultats d'aquesta funció estan pensats per fer servir en combinació amb "
#' @export
#'
#' @examples
computa_residusAS_nse <- function(data, VI, VD, pes){
  # browser()
  vi <- rlang::ensym(VI)
  vd <- rlang::ensym(VD)
  ps <- rlang::ensym(pes)

  # dt <- data
  # fes una taula de freqüències ponderada
  t1 <- questionr::wtd.table(
    x = data[[vi]],
    y = data[[vd]],
    weights = data[[ps]],
    useNA = "no"
  ) %>%
    as.data.frame.matrix(.)

  # computa chisquare
  t2 <- chisquare::chisquare(t1, B = 50)

  # extreu resultats chisquare
  t3 <- t2$post.hoc$adj.stand.resid %>%
    dplyr::mutate(rownames = rownames(t2$post.hoc$adj.stand.resid)) %>%
    tidyr::pivot_longer(-rownames,
                 names_to = glue::glue("VD"),
                 values_to = "ASres") %>%
    tidyr::unite("key1", -"ASres", na.rm = T)

  return(t3)

}

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
