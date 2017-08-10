\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{
Create Summary Table of Year and Month
}
\description{
This is a function that generate a summary table of number of observations of each year and each month.
}
\usage{
fars_summarize_years(years)
}
\arguments{
  \item{years}{
A vector of years.
}
}
\details{
If input list contains the year, the file of which does not exist, the warning will be generated to show the invalid year.
}
\value{
This function returns a table that shows the number of observation of each month in each year.
}
\examples{
ys<-c(2012,2013,2014,2015)
fars_summarize_years(ys)
fars_summarize_years(c(2011,2014,2017))

## The function is currently defined as
function (years)
{
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) \%>\% dplyr::group_by(year, MONTH) \%>\%
        dplyr::summarize(n = n()) \%>\% tidyr::spread(year, n)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
