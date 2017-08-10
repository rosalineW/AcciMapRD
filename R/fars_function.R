globalVariables(c("STATE","n","year","MONTH"))

#' Generate File Name
#'
#' This is a function that generate the file name in the format of "accident_year.csv.bz2".
#'
#' @param year A integer or numeric that given the year that will be wrapped in the file name.
#'
#' @return This function returns a character string as the file name. As a side effect, the function will also print out the file name.
#'
#' @details The function will wrap the input, which generate the character string in the format of "accident_year.csv.bz2". If the input is character, it will lead to an error. If the input is character within quotation marks, it will generate a warning, and print out "accident_NA.csv.bz2".
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Check File Existence
#'
#' This is a simple function that check the existence of the file, and read the file.
#'
#' @param filename A character string that gives the name of the file that the function will check and read. If it does not contain an absolute path, the file name is relative to the current working directory, getwd().
#'
#' @return This function returns a data frame that will be deprecated or an error message. As a side effect, the function will also show few rows of the data frame in the table format.
#'
#' @details The input has to be character within quotation marks, or it will lead to the error message. If the file does not exist in the current working dirctory, it will also lead to error message. If the file exists, it will read the file and show it in table format, and then deprecate it.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'
#' \dontrun{
#' fars_read('accident_2013.csv.bz2')
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Read Years and Month
#'
#' This is a function that select year and month columns from files of each year that is included in the input list.
#'
#' @param years A vector of years.
#'
#' @return This function returns NULL or a list of data frames that have 2 columns, year and MONTH. The list has the same length as the input, years.
#'
#' @details If input list contains the year, the file of which does not exist, the corresponding output element of that year will be NULL, and the warning will be generated to show the invalid year.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' \dontrun{
#' fars_read_years(c(2011,2014,2017))
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Create Summary Table of Year and Month
#'
#' This is a function that generate a summary table of number of observations of
#' each year and each month.
#'
#' @param years A vector of years.
#'
#' @return This function returns a table that shows the number of observation of each month in each year.
#'
#' @details If input list contains the year, the file of which does not exist, the warning will be generated to show the invalid year.
#'
#' @importFrom tidyr spread
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2011,2014,2017))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map Accident Points in Certain Year and State
#'
#' This is a function that plot the map of the points that have accidents happened in the given state and given year.
#'
#' @param state.num The number representing the state that the function will map.
#' @param year A integer or numeric that given the year that will be wrapped in the file name.
#'
#' @return This function returns the graphic plot, which shows the accident points of the state in certain year. Or it returns the message of "no accident to plot". Or it returns the error message with the invalid state number.
#'
#' @details If it cannot find the input state.num in file, it will show an error with the invalid state number. If the dataset filtered according to the input state number has zero number of rows, the message "no accidents to plot" will be printed out.
#'
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @importFrom maps map
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
