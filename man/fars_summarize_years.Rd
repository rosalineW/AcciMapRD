#' Create Summary Table of Year and Month
#'
#' This is a function that generate a summary table of number of observations of
#' each year and each month.
#'
#' @inheritParams years fars_read_years
#'
#' @return This function returns a table that shows the number of observation of
#'     each month in each year.
#'
#' @details If input list contains the year, the file of which does not exist, the
#'     warning will be generated to show the invalid year.
#'
#' @importFrom group_by function and summarize function from dplyr package, spread
#'     function from tidyr package.
#'
#' @examples
#' ys<-c(2012,2013,2014,2015)
#' fars_summarize_years(ys)
#' fars_summarize_years(c(2011,2014,2017))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
