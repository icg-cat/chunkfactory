#' Fàbrica de codi per analitzar variables categòriques
#'
#' @param dades nom del conjunt de dades a analitzar, com a cadena de caràcters
#' @param VD nom, o conjunt de noms, de les variables dependents
#' @param VI nom, o conjunt de noms, de les variables independents
#' @param pes nom de la variable de ponderació. Si no existeix, s'assigna 1
#'
#' @return llista amb el codi per executar la taula de freqüències i el diagrama de barres
#' @export
#'
#' @examples
#'data(package = "palmerpenguins", "penguins")
#'mytab_txt("penguins", "island", "species")
mytab_txt <- function(dades, VD, VI, pes){

  tau <- glue::glue(
    'tb <- mycrosstab({dades}, {VI}, {VD}, {pes})
    tb'
  )

  plot <- glue::glue(
    ' p <- tb %>%
        ggplot(.,
           aes(
             x =  {VI},
             y = PP,
             fill = {VD}
           )) +
      geom_bar(
        stat = "identity",
        position = "fill"
      ) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      theme_light() +
      labs(
        title = paste0("{VD}", " segons ", "{VI}"),
        x = "\n",
        y = "(%)",
        caption = "proves",
        fill = NULL
      ) +
  guides(fill = guide_legend(ncol = 2, position = "bottom",
          direction = "vertical")) +
  scale_fill_manual(
  values = colorRampPalette(RColorBrewer ::brewer.pal(11, "Spectral"))(11)
  )

  p
  '
  )

  return(list(tau, plot))
}


# mytab_txt <- function(dades, VD, VI, pes){
#
#   tau <- glue::glue(
#     'tb <- {dades} %>%
#     select(pes, {VD}, {VI}) %>%
#     filter(complete.cases(.)) %>%
#     group_by({VI}) %>%
#     mutate(TT = sum(pes),
#            tt = n()) %>%
#     group_by({VD}, .add = T) %>%
#     summarise(
#       N = sum(pes),
#       n = n(),
#       TT = unique(TT),
#       PP = round(N/TT*100, 2)
#    )
#   tb'
#   )
#
#   plot <- glue::glue(
#     ' p <- tb %>%
#         ggplot(.,
#            aes(
#              x =  {VI},
#              y = PP,
#              fill = {VD}
#            )) +
#       geom_bar(
#         stat = "identity",
#         position = "fill"
#       ) +
#       scale_y_continuous(labels = scales::percent) +
#       coord_flip() +
#       theme_light() +
#       labs(
#         title = paste0("{VD}", " segons ", "{VI}"),
#         x = "\n",
#         y = "(%)",
#         caption = "CISEELAP - 2024",
#         fill = NULL
#       ) +
#   guides(fill = guide_legend(ncol = 2, position = "bottom",
#           direction = "vertical")) +
#   scale_fill_manual(
#   values = colorRampPalette(RColorBrewer ::brewer.pal(11, "Spectral"))(11)
#   )
#
#   p
#   '
#   )
#
#   return(list(tau, plot))
# }
