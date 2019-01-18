#' A Databook is a bookdown book generated from a dataset
#'
#' generate_databook creates a bookdown book with one "chapter" for each variable
#'
#' @param file data file to read

generate_databook <- function(
  file,
  dest,
  reader = readr::read_csv,
  get_template = get_template,
  get_data = get_data
  ) {

  # check the file exists and is accessible
  fs::file_exists(file)

  # check that the destination exists
  fs::dir_exists(dest)

  # check `reader` is a function
  rlang::isfunction(reader)

  # try to read the file with `reader`, if unsuccessful capture the error, explain why
  # functions stopping and return error
  # `reader` unable to read data
  # `error`
  .data <- reader(file)

  templates <- get_template(.data, ...)
  data <- get_data(.data, ...)

  # for each variable create a Rmd that
  # fill the template
  for(i in seq_along(vars)){

    writeLines(
      whisker::whisker.render(
        readLines(template[i]),
        data[i]
        )
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

get_template <- function(.data) {
  types <-  vapply(.data, class, character(1))

  # check that system.files returns something?
  find_template <- function(template) system.file("templates", template, package = "databook")

  vapply(
    types,
    switch,
    character(1),
    "numeric" = find_template("numeric.Rmd"),
    "integer" = find_template("numeric.Rmd"),
    "character" = find_template("character.Rmd"),
    "factor" = find_template("character.Rmd"),
    ""
  )
}

#' get_data takes a vector and returns a list of data about that vector.
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


get_data <- function(.data) {
  lapply(names(.data), function(i) {
    list(
      var_name = i,
      file = file,
      reader = reader
    )
  })
}
