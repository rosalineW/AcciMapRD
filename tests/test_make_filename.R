library(testthat)
expect_that(make_filename(2014),is_identical_to("accident_2014.csv.bz2"))
