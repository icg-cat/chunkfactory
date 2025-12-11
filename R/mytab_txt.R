#' Code factory for the analysis of categorical variables.
#'
#' @param dades character string with the name of the dataset.
#' @param VI character string with the names of the independent variables
#' @param VD character string with the names of the dependent variables
#' @param pes character string with the names of the weighting variables.
#'
#' @return list with the code to execute a crosstab and a stacked bar chart.
#'
#' @examples
#'data(package = "palmerpenguins", "penguins")
#'penguins$pes <- 1
#'chunkfactory:::mytab_txt("penguins", "island", "species", "pes")
mytab_txt <- function(dades, VI, VD, pes){
  # browser()
  tau <- glue::glue(
    'tb <- chunkfactory:::mycrosstab({dades}, "{VI}", "{VD}", "{pes}")
    tb'
  )

  plot <- glue::glue(
    ' p <- tb %>%
        ggplot2::ggplot(.,
          ggplot2::aes(
             x =  VI,
             y = PP,
             fill = VD
           )) +
      ggplot2::geom_bar(
        stat = "identity",
        position = "fill"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::coord_flip() +
      ggplot2::theme_light() +
      ggplot2::labs(
        title = paste0("{VD}", " segons ", "{VI}"),
        x = "\n",
        y = "(%)",
        fill = NULL
      ) +
  ggplot2::guides(fill = guide_legend(ncol = 2, position = "bottom",
          direction = "vertical")) +
  ggplot2::scale_fill_manual(
  values = colorRampPalette(RColorBrewer ::brewer.pal(11, "Spectral"))(11)
  )

  p
  '
  )

  return(list(tau, plot))
}
