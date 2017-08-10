#' Map State with Given Year
#'
#' This is a function that plot the map of the points that have accidents happened in
#' the given state and given year.
#'
#' @param state.num The number representing the state that the function will map.
#' @inheritParams year make_filename
#'
#' @return This function returns the graphic plot, which shows the accident points of
#'     the state in certain year. Or it returns the message of "no accident to plot".
#'     Or it returns the error message with the invalid state number.
#'
#' @details If it cannot find the input state.num in file, it will show an error with
#'     the invalid state number. If the dataset filtered according to the input state
#'     number has zero number of rows, the message "no accidents to plot" will be
#'     printed out.
#'
#' @importFrom filter function from dplyr package, map function from maps package, points
#'     function from graphics package.
#'
#' @examples
#' fars_map_state(1,2015)
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
