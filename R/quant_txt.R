#' fàbrica de codi per analitzar variables numèriques
#'
#' @param dades nom del conjunt de dades a analitzar, com a cadena de caràcters
#' @param VD nom, o conjunt de noms, de les variables dependents
#' @param VI nom, o conjunt de noms, de les variables independents
#' @param pes nom de la variable de ponderació. Si no existeix, s'assigna 1
#'
#' @return llista amb el codi per executar la taula d'estadístics escriptius i el diagrama de caixes
#' @export
#'
#' @examples
#' data(package = "palmerpenguins", "penguins")
#' quant_txt("penguins", "island", VI = c("body_mass_g", "bill_length_mm"))
quant_txt <- function(dades, VD, VI, pes){
  tau <- glue::glue(
    "tab <-
  {dades} %>%
  group_by({VI}, .add = T) %>%
  summarise(
    n       = n(),
    N       = sum(pes),
    mitjana = Hmisc::wtd.mean(x = {VD}, weights = pes, na.rm = T),
    mediana = Hmisc::wtd.quantile(x = {VD}, weights = pes, probs = 0.5, na.rm = T),
    desv_tip= sqrt(Hmisc::wtd.var(x = {VD}, weights = pes, na.rm = T)),
    margin  = (qt(p = 0.05/2, df = n, lower.tail = F)) * (desv_tip/sqrt(n)),
    lower   = mitjana - margin,
    upper   = mitjana + margin
  )
  tab"
  )

  pp <- glue::glue(
    'p <- {dades} %>%
    ggplot(.,
           aes( x = {VI},
                y = {VD},
                weight = pes)) +
    geom_boxplot() +
    expand_limits(y = 0) +
    coord_flip() +
    theme_minimal() +
    labs(
        title = paste0("{VD}", " segons ", "{VI}"),
        caption = "proves"
      )
  p'
  )

  return(list(tau, pp))
}
