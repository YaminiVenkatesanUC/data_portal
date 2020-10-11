library(testthat)
library(lintr)
source("../linters.R")

context("Test code formating")

test_that("Linting", {
  core_dependencies <-
    list.files(
    "../../R/",
    full.names = TRUE,
    recursive = TRUE
  )

  for (dependency in core_dependencies) {
    expect_equal(
      length(lintr::lint(dependency, linters = linterList)),
      0
    )
  }
})
