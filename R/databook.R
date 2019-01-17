#' A Databook is a bookdown book generated from a dataset
#'
#' generate_databook creates a bookdown book with one "chapter" for each variable
#'
#' @param file data file to read

generate_databook <- function(file, dest, reader = readr::read_csv, template = get_template) {

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

  # get all the variable names
  vars <- colnames(.data)

  # for each variable create a Rmd that
  walk()
}

#' get_template just has to return a character vector with paths to the template
#' you want to use - this could simply return a single path, or be dependent on
#' the data types of each variable

get_template <- function(.data) {
  types <-  purrr::map_chr(vctrs::vec_type)

}
