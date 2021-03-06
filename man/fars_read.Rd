% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read CSV file}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{the name of the file which the data are to be read from.
Each row of the table appears as one line of the file. If it does not contain an absolute path,
the file name is relative to the current working directory, getwd().}
}
\value{
This function returns a dataframe containing the data in the filename if it existst. In case filename
doesn't exist, an error is displayed.
}
\description{
Reads a CSV file in table format and creates a data frame from it.
If the filame doesn not exist, a message is displayed.
}
\examples{
path = system.file("extdata", "accident_2013.csv.bz2", package = "fars")
fars_read(path)

}
