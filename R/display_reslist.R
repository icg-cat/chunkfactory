#' display a list of results in a given order o layout in a markdown document. Works in coordination with chunkfactory_display
#'
#' @param myset named list containing set of results to be displayed
#' @param mylayout definition of the layout or order that the results should be displayed in
#'
#' @return block of markdown to be interpreted by knitr::knit_child
#'
#' @examples
#' \dontrun{
#' display_reslist(myset)
#' }

display_reslist <- function(myset, mylayout = "layout1"){
  if(mylayout == "layout1"){
    list(
      titol = myset$title,
      text = myset$text,
      plot = myset$plot
    )
  } else if(mylayout == "layout2"){
    list(
      titol = myset$title,
      plot = myset$plot,
      text = myset$text
    )
  }
}
