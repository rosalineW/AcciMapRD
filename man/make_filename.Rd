\name{make_filename}
\alias{make_filename}
\title{
Generate File Name
}
\description{
This is a function that generate the file name in the format of "accident_year.csv.bz2".
}
\usage{
make_filename(year)
}
\arguments{
  \item{year}{
A integer or numeric that given the year that will be wrapped in the file name.
}
}
\details{
The function will wrap the input, which generate the character string in the format of "accident_year.csv.bz2". If the input is character, it will lead to an error. If the input is character within quotation marks, it will generate a warning, and print out "accident_NA.csv.bz2".
}
\value{
This function returns a character string as the file name. As a side effect, the function will also print out the file name.
}
\examples{
make_filename(2013)
}
