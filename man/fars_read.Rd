#' Check File Existence
#'
#' This is a simple function that check the existence of the file, and read the file.
#'
#' @param filename A character string that gives the name of the file that the function
#'    will check and read. If it does not contain an absolute path, the file name is
#'    relative to the current working directory, getwd().
#'
#' @return This function returns a data frame that will be deprecated or an error message.
#'    As a side effect, the function will also show few rows of the data frame in the
#'    table format.
#'
#' @details The input has to be character within quotation marks, or it will lead
#'    to the error message. If the file does not exist in the current working dirctory,
#'    it will also lead to error message. If the file exists, it will read the file and
#'    show it in table format, and then deprecate it.
#'
#' @importFrom read_csv function from readr package, tbl_df function from dplyr package.
#'
#' @examples
#' fars_read("file.csv")
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
