#' Turn any custom function into a chunk factory.
#'
#' @param myfunc custom function you'd like to execute. Data arguments need to be treated as character strings. See example.
#' @param param_list list of parameters needed by the function. Need to be specified with the same names, in the same order as the function arguments.
#' @param sections level of the titles for the sections. Defaults to Header 3
#'
#' @return a character string containing the markdown generating the document sections  (under a title 3), the chunks and the code to be rendered by the .rmd document.
#' @export
#'
#' @examples
#' utils::data(package = "palmerpenguins", "penguins")
#' myfunc <- function(data_name, v1){
#'   data <- get(data_name)
#'   res1 <- table(data[[v1]])
#'   res2 <- ggplot2::ggplot(data) + ggplot2::geom_bar(ggplot2::aes(x = .data[[v1]]))
#'   res3 <- prop.table(res1)
#'   return(list(res1, res2, res3))
#' }
#' myfunc_params <- list(data_name = "penguins", v1 = c("species", "island", "sex"))
#' fabrica_chunks_myfunc(myfunc, myfunc_params, sections = "1")
fabrica_chunks_myfunc <- function(myfunc, param_list, sections = "3"){

  sections <- dplyr::case_when(
    sections == "3" ~ "  ### {{tab_name}}\n",
    sections == "2" ~ "  ## {{tab_name}}\n",
    sections == "1" ~ "  # {{tab_name}}\n",
    .default = sections
  )

  reslist <- .reslistify(myfunc, param_list)

  .chunkify(reslist, sections)

}


#' make reslist structure for fabrica_chunks_myfunc
#'
#' @param myfunc custom function
#' @param param_list named list of parameters
#'
#' @returns a list
.reslistify <- function(myfunc, param_list){
  #1. define parameters
  param <- base::expand.grid(
    param_list
  )
  param <- base::lapply(param, function(x){
    base::as.list(base::as.character(x))
  })

  # checks
  args_param    <- base::names(param_list)
  args_function <- methods::formalArgs(myfunc)

  if(!(base::all(args_param %in% args_function))) stop("Thre's a mismatch between the arguments passed to the list of parameters and the arguments of the function that was given. Please verify: 1) if the function is not custom, precede it with the name of the package; 2) review case & spelling of the names of the arguments in the parameter list, to make shure they match exactly the spelling and order in the function. ")

  #2. extract function results into a list
  reslist <- purrr::pmap(param, myfunc)
  name_grid <- base::do.call("cbind", param) %>%
    base::data.frame() %>%
    tidyr::unite(col = "noms", sep = ".") %>%
    dplyr::pull(.data$noms)
  base::names(reslist) <- name_grid

  return(reslist)
}

#' make a text string that can be parsed by knitr, containing a chunk for each output in the list
#'
#' @param reslist list of results as produced by .reslistify
#' @param sections text string defining the kind of separation of the sections
#'
#' @returns a text string
.chunkify <- function(reslist, sections){
  # desa reslist al Global.env per fer accessible des del markdown ------------
  base::assign("reslist", reslist, envir = .GlobalEnv)

  # escriu codi pels chunks ---------------------------------------------------

  src <- purrr::map_chr(seq_along(reslist), ~ {

    nivells_sublist <- base::length(reslist[[.x]])

    tab_name <- base::names(reslist[.x])

    evals <- base::unlist(base::lapply(base::seq_along(1:nivells_sublist), function(i){
      glue::glue("eval(parse(text = 'reslist[[{.x}]][[{i}]]'))")
    }))

    # n_hash <- base::paste0(rep("#", title_level), collapse="")

    knitr::knit_expand(text = c(
      "\n",
      sections,
      "```{r echo = TRUE}\n",
      evals,
      "```",
      "\n"))
  })

  return(src)

}

