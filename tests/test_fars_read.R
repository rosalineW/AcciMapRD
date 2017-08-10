library(testthat)
expect_that(fars_read("accident_2016.csv.bz2"),throws_error("file 'accident_2016.csv.bz2' does not exist"))

