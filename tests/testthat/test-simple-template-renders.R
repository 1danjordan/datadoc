context("Render Template")
library(fs)

replace_ext <- function(x, ext) sub('\\..*$', ext, x)

test_that("Simple template is filled and renders", {
  template <-
'---
title: "Databook Template: {{variable}}"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Read data and plot {{variable}}.

```{r}
df <- {{data}}
plot(df${{variable}})
```'

  data <- list(variable = "Height", data = "datasets::trees")
  tmp <- file_temp(ext = ".Rmd")

  # Render template
  writeLines(whisker::whisker.render(template, data), tmp)
  # Render Rmd
  rmarkdown::render(tmp)
  # Check Rmd is successfully rendered as a html file
  expect_true(file_exists(replace_ext(tmp, ".html")))
})

test_that("Example templates render", {
  tmp_categorical <- file_temp(ext = ".Rmd")
  categorical_template <- get_template("categorical")
  categorical_data <- list(variable = "Species", data = "datasets::iris")

  # Render template
  writeLines(
    whisker::whisker.render(categorical_template, categorical_data),
    tmp_categorical
  )
  # Render Rmd
  rmarkdown::render(tmp_categorical)
  # Check Rmd is successfully rendered as a html file
  expect_true(file_exists(replace_ext(tmp_categorical, ".html")))
})

test_that("generate-databook renders templates and data", {
  path_tmp <- paste0(fs::path_temp(), "/databook")

  templates <- c(get_template("categorical"), get_template("numeric"))
  parameters <- list(
    list(variable = "Species", data = "datasets::iris"),
    list(variable = "Height", data = "datasets::trees")
  )

  generate_databook(path_tmp, templates, parameters)

  # very naive for the moment - just check the files are there
  # future you, compare them line for line / check that they render
  expect_true(
    file_exists(paste0(path_tmp, "/01-databook.Rmd")) &
      file_exists(paste0(path_tmp, "/02-databook.Rmd"))
  )

})
