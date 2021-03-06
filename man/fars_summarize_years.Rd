% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Produce summary showing the number accidents for each month}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{a vector of years for which to read data}
}
\value{
a tibble with columns "MONTH" (which has values 1-12) and one column
      per year. The values in these columns show the number of accidents
      in that month and year.
}
\description{
This function reads FARS data using \code{fars_read_years} and produces a
summary that shows the number of cases for each month.
}
\details{
The function returns an error if \code{fars_read_years} is unable to read
data for specified years.
}
\examples{
\dontrun{
 y <- c(2017, 2018, 2019)
 fars_summarize_years(y)
}

}
