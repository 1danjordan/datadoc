#' Take a dataset and generte a datadoc bookdown project
#'
#' @param data_expr an expression that evaluates to a data frame or list
#' @param dir target directory for generating datadoc - defaults to working directory
#' @param title the title of your datadoc
#' @param templater a function that takes data and any other arguments
#'   returns a list of lists, each list containing a template and a list
#'   parameters to fill that template
#' @param ... any arguments to pass into the templater

datadoc <- function(data_expr, dir = getwd(), title = "datadoc", template = templater, ...) {

  # Capture the data expression and evaluate it in a clean
  # environment to make sure it will evaluate in the Rmd template properly
  d <- with_handlers(
    eval_tidy(data_expr, env = environment()),
    error = ~ abort(paste("The expression \n", quo_text(data_expr), "\ncouldn't be evaluated"))
  )

  if(!is.data.frame(d)) abort(paste("`data` is a", type_of(d), "not a dataframe"))

  templates <- template(data_expr = data_expr, data = d, title = title, ...)

  generate_datadoc(templates, dir = dir)
}

#' A datadoc is a bookdown book generated from a dataset
#'
#' generate_datadoc creates a bookdown book with one "chapter" for each variable
#'
#' @param templates a list of templates to generate the datadoc from
#' @param dir which directory to generate the datadoc in

generate_datadoc <- function(templates, dir) {

  fs::dir_create(dir)

  # Render each template with its data
  for(i in seq_along(templates)) {
    writeLines(
      whisker::whisker.render(
        template = readLines(templates[[i]][["template"]]),
        data = templates[[i]][["data"]]
      ),
      paste0(dir,"/", templates[[i]][["filename"]])
    )
  }
}

#' templater returns a list of templates and data
#' to be rendered into a datadoc

templater <- function(data_expr, data, title, ...) {

  inc <- incrementer()

  build_template <- function(var_name, var_type) {
    list(
      list(
        data = list(variable = var_name),
        template = get_template(var_type),
        filename = paste0(inc(), "-", var_name, ".Rmd")
      ))
  }

  # build up the templates for each variable in the dataset
  var_names <- colnames(data)
  var_types <- vapply(data, class, "")
  var_templates <- mapply(build_template, var_names, var_types)

  # build up the basic bookdown templates
  bookdown_templates <- list(
    index = list(
      template = get_template("index"),
      data = list(title = title, data = quo_text(data_expr)),
      filename = "index.Rmd"
    ),
    output = list(
      template = get_template("_output"),
      data = list(title = title),
      filename = "_output.yml"
    )
  )

  append(bookdown_templates, var_templates)
}

#' get_template returns a path to a template on your file system
#' @param template the type of template you want

get_template <- function(template) {

  find_template <- function(template) system.file(paste0("templates/", template), package = "datadoc")

  templ <- switch(
    template,
    "index"     = find_template("index.Rmd"),
    "_output"   = find_template("_output.yml"),
    "numeric"   = find_template("numeric-template.Rmd"),
    "integer"   = find_template("numeric-template.Rmd"),
    "character" = find_template("categorical-template.Rmd"),
    "factor"    = find_template("categorical-template.Rmd"),
    "Date"      = find_template("date-template.Rmd")
  )

  # check that a template actually gets returned
  if(is.null(templ)) abort(paste("No template was found for", template))

  templ
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
