\name{fars_read_years}
\alias{fars_read_years}
\title{
Read Years and Month
}
\description{
This is a function that select year and month columns from files of each year that is included in the input list.
}
\usage{
fars_read_years(years)
}
\arguments{
  \item{years}{
A vector of years.
}
}
\details{
If input list contains the year, the file of which does not exist, the  corresponding output element of that year will be NULL, and the warning will be generated to show the invalid year.
}
\value{
This function returns NULL or a list of data frames that have 2 columns, year and MONTH. The list has the same length as the input, years.
}
\examples{
ys<-c(2012,2013,2014,2015)
fars_read_years(ys)
fars_read_years(c(2011,2014,2017))

## The function is currently defined as
function (years)
{
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) \%>\% dplyr::select(MONTH,
                year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
  }
}
