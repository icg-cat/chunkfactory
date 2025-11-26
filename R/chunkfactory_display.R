#' chunkfactory_display
#'
#' @param myfunc funció que es vol aplicar iterativament sobre la llista de resultats. Veure exemple de funció per display en chunkfactory:::display_reslist()
#' @param display_list named list of results to display
#' @param title_level nivell dels títols que es desitja per la organització del document
#'
#' @returns markup que s'interpreta via knitr::knit_child()
#'
#' @examples
#' \dontrun{
#' chunkfactory_display(myres)
#' }
chunkfactory_display <- function(myfunc, display_list, title_level = 1){
  ## versió de chunk factory per llista de resultats generats externament (en lloc de codi que s'avalua in situ).

  ## millores pendents: ara per ara, està hard-coded si els elements pel display són t´titols, text o gràfics. caldria que sigui més flexible (p.e., titols, text i resultats, que pugui incloure qualsevol objecte d'output de R ja siguin taules, gràfics, etc)

  # aplica funció a llista de paràmetres
  reslist <- purrr::pmap(display_list, myfunc)
  assign("reslist", reslist, envir = .GlobalEnv)

  # fes llista de resultats
  src <- purrr::map_chr(seq_along(reslist), ~ {
    current_result <- reslist[[.x]]

  # defineix altres elements pel display
    tab_name <- current_result$titol
    n_hash <- paste0(rep("#", title_level), collapse = "")

    # genera blocs de contingut
    content_elements <- names(current_result)[-1]

    chunk_content <- purrr::map_chr(content_elements, function(element_name) {
      if(element_name == "text") {
        return(gsub("```\\n |\\n ```", "", current_result[[element_name]]))
      } else if(element_name == "plot") {
        return(glue("```{{r}}\nprint(reslist[[{.x}]]${element_name})\n```"))
      } else {
        # print altres elements diferents a text o plot
        return(glue("```{{r}}\nreslist[[{.x}]]${element_name}\n```"))
      }
    })

    # combina
    full_content <- paste(c(
      "\n",
      paste(n_hash, tab_name),
      "",
      chunk_content
    ), collapse = "\n\n")

    return(full_content)
  })

  return(src)
}
