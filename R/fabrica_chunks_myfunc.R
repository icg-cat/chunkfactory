#' Turn any custom function into a chunk factory.
#'
#' @param myfunc custom function you'd like to execute. Data arguments need to be treated as character strings. See example.
#' @param param_list list of parameters needed by the function. Need to be specified with the same names, in the same order as the function arguments.
#' @param title_level level of the titles for the sections. Defaults to Header 3
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
#' fabrica_chunks_myfunc(myfunc, myfunc_params)
fabrica_chunks_myfunc <- function(myfunc, param_list, title_level = 3){

  #1. define parameters
  param <- base::expand.grid(
    param_list
  )
  param <- base::lapply(param, function(x){
    base::as.list(base::as.character(x))
  })

  # param_length <- length(param_list)


  #2. extract function results into a list
  args_param    <- base::names(param_list)
  args_function <- methods::formalArgs(myfunc)

  if(!(base::all(args_param %in% args_function))) stop("Thre's a mismatch between the arguments passed to the list of parameters and the arguments of the function that was given. Please verify: 1) if the function is not custom, precede it with the name of the package; 2) review case & spelling of the names of the arguments in the parameter list, to make shure they match exactly the spelling and order in the function. ")

  reslist <- purrr::pmap(param, myfunc)
  name_grid <- base::do.call("cbind", param) %>%
                base::data.frame() %>%
                tidyr::unite(col = "noms", sep = ".") %>%
                dplyr::pull(noms)
  base::names(reslist) <- name_grid

  # desa reslist al Global.env
  base::assign("reslist", reslist, envir = .GlobalEnv)

  # 3. escriu codi chunks ---------------------------------------------------

  src <- purrr::map_chr(seq_along(reslist), ~ {
      # browser()

    nivells_sublist <- base::length(reslist[[.x]])

    tab_name <- base::names(reslist[.x])

    evals <- base::unlist(base::lapply(base::seq_along(1:nivells_sublist), function(i){
      glue::glue("eval(parse(text = 'reslist[[{.x}]][[{i}]]'))")
    }))

    n_hash <- base::paste0(rep("#", title_level), collapse="")

    knitr::knit_expand(text = c(
      "\n",
      "{{n_hash}} {{tab_name}}\n",
      "```{r echo = TRUE}\n",
      evals,
      "```",
      "\n"))
  })

  return(src)

}
