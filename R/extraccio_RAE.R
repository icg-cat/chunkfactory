#' Compute adjusted standardized residuals (ASR).
#' @description he extret el codi de chisquare::chisquare(), que te altament verbose output, per obtenir només la part de residus ajustats estandarditzats que volia.
#'
#' @param mytab crosstab with absolute frequencies, as produced by table(), or questionr::wtd.table()
#'
#' @return table containing a primary key and AER.
#' @export
#'
#' @examples
#' data(package = "palmerpenguins", "penguins")
#' mytab <- table(penguins$island, penguins$sex)
#' computa.ASR(mytab)
extraccio_RAE <- function(mytab){

  df <- mytab # per mantenir referències de codi de chisquare

  # referencies estructura taula
  n  <- sum(df)
  sr <- rowSums(df)
  sc <- colSums(df)
  nr <- as.integer(nrow(df))
  nc <- as.integer(ncol(df))

  # simplificacio de la sub-funcio calc() al codi original
  exp.freq <- function(dt) {
    n <- sum(dt)
    exp.freq <- round(outer(rowSums(dt), colSums(dt), "*")/n,
                      3)
    return(exp.freq)
  }

  # frequencies esperades
  exp.freq <- round(exp.freq(df), 3)

  # residus (Agresti, 2007)
  stand.res <- round((df - exp.freq)/sqrt(exp.freq), 3)

  adj.stand.res <- stand.res
  for (i in 1:nr) {
    for (j in 1:nc) {
      adj.stand.res[i, j] <- round(stand.res[i, j]/sqrt((1 - sr[i]/n) * (1 - sc[j]/n)), 3)
    }
  }

  return(adj.stand.res)

}
