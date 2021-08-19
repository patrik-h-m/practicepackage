library(testthat)

test_that("file names are generated correctly", {
  expect_that(make_filename(2012), equals("accident_2012.csv.bz2"))
  expect_that(make_filename(1996), equals("accident_1996.csv.bz2"))
  expect_that(make_filename(96), equals("accident_96.csv.bz2"))
})

