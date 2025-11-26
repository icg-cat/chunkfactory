#' code factory to analyse numerical variables.
#'
#' @param dades character vector with the name of the dataset
#' @param VI character vector with the name of the independent variable
#' @param VD character vector with the name of the dependent variable
#' @param pes character vector with the name of the weighting variable
#'
#' @return list with the code to execute a table of descriptive statistics and grouped boxplots.
#'
#' @examples
#' data(package = "palmerpenguins", "penguins")
#' penguins$pes <- 1
#' chunkfactory:::quant_txt("penguins", "island", VD = c("body_mass_g", "bill_length_mm"), "pes")
quant_txt <- function(dades, VI, VD, pes){
  tau <- glue::glue(
    "tab <-
  {dades} %>%
  group_by({VI}, .add = T) %>%
  summarise(
    n       = n(),
    N       = sum({pes}),
    mitjana = Hmisc::wtd.mean(x = {VD}, weights = {pes}, na.rm = T),
    mediana = Hmisc::wtd.quantile(x = {VD}, weights = {pes}, probs = 0.5, na.rm = T),
    desv_tip= sqrt(Hmisc::wtd.var(x = {VD}, weights = {pes}, na.rm = T)),
    margin  = (qt(p = 0.05/2, df = n, lower.tail = F)) * (desv_tip/sqrt(n)),
    lower   = mitjana - margin,
    upper   = mitjana + margin
  )
  tab"
  )

  pp <- glue::glue(
    'p <- {dades} %>%
    ggplot2::ggplot(.,
      ggplot2::aes( x = {VI},
                y = {VD},
                weight = {pes})) +
    ggplot2::geom_boxplot() +
    ggplot2::expand_limits(y = 0) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
        title = paste0("{VD}", " segons ", "{VI}"),
        caption = "proves"
      )
  p'
  )

  return(list(tau, pp))
}
