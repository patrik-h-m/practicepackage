% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plot accidents on a map}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{The number of the US state to plot. The states are numbered
alphabetically (1 = Alabama, 2 = Alaska etc.)}

\item{year}{the year whose accidents to plot on the map}
}
\value{
This function draws a map. It does not return a value.
}
\description{
The function is used to create a map showing accidents in a specified US
state during a specified year.
}
\details{
The function results in an error if \code{fars_read} fails to read data or if
the data contains no accidents found for the given state.
}
\examples{
\dontrun{
 fars_map_state(6, 2013)
 fars_map_state(49, 2015)
}

}
