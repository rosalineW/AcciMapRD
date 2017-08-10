\name{fars_map_state}
\alias{fars_map_state}
\title{Map Accident Points in Certain Year and State
}
\description{
This is a function that plot the map of the points that have accidents happened in the given state and given year.
}
\usage{
fars_map_state(state.num, year)
}
\arguments{
  \item{state.num}{
The number representing the state that the function will map.
}
  \item{year}{
A integer or numeric that given the year that will be wrapped in the file name.
}
}
\details{
If it cannot find the input state.num in file, it will show an error with the invalid state number. If the dataset filtered according to the input state number has zero number of rows, the message "no accidents to plot" will be printed out.
}
\value{
This function returns the graphic plot, which shows the accident points of the state in certain year. Or it returns the message of "no accident to plot". Or it returns the error message with the invalid state number.
}

\examples{
fars_map_state(1,2015)
}
