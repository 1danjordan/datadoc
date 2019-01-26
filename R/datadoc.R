#' Take a dataset and generte a datadoc bookdown project
#'
#' @param data an expression that evaluates to a data frame or list
#' @param templater a function that takes data and any other arguments
#'   returns a list of lists, each list containing a template and a list
#'   parameters to fill that template
#' @param ... any arguments to pass into the templater
#' @param dir directory to generate datadoc in

datadoc <- function(data, templater = get_template, ..., dir) {

  # Capture the data expression and evaluate it in a clean
  # environment to make sure it will evaluate in the Rmd template properly
  data_expr <- enexpr(data)
  d <- with_handlers(
    eval_tidy(data_expr, env = new_environment()),
    error = ~ abort(paste("The expression \n", quo_name(data_expr), "\ncouldn't be evaluated"))
  )

  if(!is_list(d)) abort(paste("`data` is a", type_of(d), "not a data frame/list"))

  templater_nm <- quo_name(templater)
  templates <- with_handlers(
    templater(data, ...),
    error = ~ abort(paste(templater_nm, "couldn't be evaluated"))
  )

  # make sure templater returns a list
  if(!is_list(templates)) abort(paste(templater_nm, "must return a list, not a", type_of(templates)))

  # check that templates has the names templates and data
  check_nms <- any(!has_name(templates, c("templates", "data")))
  if(check_nms) abort(paste(templater_nm, "must have `templates` and `data`. Instead it has names:", names(templates)))

  generate_datadoc(data, templates, parameters, dir)
}

#' A datadoc is a bookdown book generated from a dataset
#'
#' generate_datadoc creates a bookdown book with one "chapter" for each variable
#'
#' @param dir which directory to generate the datadoc in
#' @param templates a list of templates to generate the datadoc from
#' @param parameters a list of lists with data to fill each template

generate_datadoc <- function(templates, parameters, dir) {

  fs::dir_create(dir)

  # copy in the relevant bookdown files

  # check that the length of templates and parameters matches
  if(length(templates) != length(parameters)) {
    abort("`templates` and `parameters` are not the same length")
  }

  templs <- lapply(templates, readLines)
  inc <- incrementer()

  for(i in seq_along(templates)) {
    writeLines(
      whisker::whisker.render(templs[[i]], parameters[[i]]),
      paste0(dir,"/", inc(), "-datadoc.Rmd")
    )
  }
}

#' templater templater takes a data frame and any other arguments
#' and decides which templates to use and the data that goes in them.
#'
#' This is just a standard datadoc templater, but you can write your own.
#' It just needs to return a named list containing two lists of equal length:
#'
#'     templates: list of paths to templates files
#'     data: list of parameters to fill those templates

templater <- function(data) {

  # First chapter will be a summary of the entire dataset
  # the rest will be be one chapter for each variable in the data frame
  template_types <- c("summary", lapply(data, type_of))
  templates <- lapply(template_types, find_template)

  parameters <- list(
    data = rep(expr_deparse(data_expr, length(template_types))),
    var_nms = c("Summary", names(data))
  )
}

#' get_template returns a path to a template on your file system
#' @param template the type of template you want

get_template <- function(template) {

  find_template <- function(template) system.file(paste0("templates/", template), package = "datadoc")

  templ <- switch(
    template,
    "double"    = find_template("numeric-template.Rmd"),
    "integer"   = find_template("numeric-template.Rmd"),
    "character" = find_template("categorical-template.Rmd"),
    "factor"    = find_template("categorical-template.Rmd"),
    "summary"   = find_template("summary-template.Rmd")
  )

  # check that a template actually gets returned
  if(is.null(templ)) abort(paste("No template was found for", template))

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


#' Helper for prepending an increasing number for file names so bookdown
#' sees them in the correct order

incrementer <- function(start = 0) {
  i <- start
  function() {
    i <<- i + 1
    if (i < 10) paste0("0", i)
    else as.character(i)
  }
}

#' @import rlang
