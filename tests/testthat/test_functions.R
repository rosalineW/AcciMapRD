library(AcciMapRD)

test_that("The file functions work",{
  expect_that(fars_read_years(2017),gives_warning("invalid year: 2017"))
  expect_that(make_filename(2014),is_identical_to("accident_2014.csv.bz2"))
})
