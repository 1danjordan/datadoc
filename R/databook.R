#' A Databook is a bookdown book generated from a dataset
#'
#' generate_databook creates a bookdown book with one "chapter" for each variable
#'
#' @param dir which directory to generate the databook in
#' @param templates a list of templates to generate the databook from
#' @param parameters a list of lists with data to fill each template

generate_databook <- function(dir, templates, parameters) {

  fs::dir_create(dir)

  # copy in the relevant bookdown files

  # check that the length of templates and parameters matches
  if(length(templates) != length(parameters)) {
    stop("`templates` and `parameters` are not the same length")
  }

  templs <- lapply(templates, readLines)

  # call whisker.render for each template
  for(i in seq_along(templates)) {
    writeLines(
      whisker::whisker.render(templs[[i]], parameters[[i]]),
      # need a way to name Rmd files appropriately
      paste0(dir,"/", "0", i, "-databook.Rmd")
    )
  }
}

#' get_template just has to return a character vector with paths to the template
#' you want to use - this could simply return a single path, or be dependent on
#' the data types of each variable
#'
#' @param .data a dataset
#'
#' @return a character vector of paths to template .Rmd files

get_template <- function(template) {

  find_template <- function(template) system.file(paste0("templates/", template), package = "databook")

  templ <- switch(
    template,
    "numeric" = find_template("numeric-template.Rmd"),
    "categorical" = find_template("categorical-template.Rmd")
  )

  # check that a template actually gets returned
  if(is.null(templ)) stop("No template was found")

  templ
}

#' fill_template takes a vector and returns a list of data about that vector.
#' It is used to build up a list that is passed into whisker.render.
#' This should be named something else - it's just taking the name from
#' the argument in whisker.render...
#'
#' This one just returns the name of each column and returns it in a list
#'
#' Depending on your template, you need to make sure that your template is properly
#' filled
#'
#' In datadown we need to be able to read the data and that's about it...
#' Do what you want from there


fill_template <- function(.data) {
  lapply(names(.data), function(i) {
    list(
      var_name = i,
      file = file,
      reader = reader
    )
  })
}
