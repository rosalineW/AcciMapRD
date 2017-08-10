\name{fars_read}
\alias{fars_read}
\title{
Check File Existence
}
\description{
This is a simple function that check the existence of the file, and read the file.
}
\usage{
fars_read(filename)
}
\arguments{
  \item{filename}{
A character string that gives the name of the file that the function will check and read. If it does not contain an absolute path, the file name is relative to the current working directory, getwd().
}
}
\details{
The input has to be character within quotation marks, or it will lead to the error message. If the file does not exist in the current working dirctory, it will also lead to error message. If the file exists, it will read the file and show it in table format, and then deprecate it.
}
\value{
This function returns a data frame that will be deprecated or an error message. As a side effect, the function will also show few rows of the data frame in the table format.
}
\examples{
fars_read("file.csv")
}
