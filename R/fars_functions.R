#' Read data from csv file
#'
#' This function reads data reads data from a csv file into a tibble.
#' Internally the function uses \code{readr::read_csv} to read the file.
#'
#' If \code{filename} does not exist an error is thrown.
#'
#' @param filename the name of the file from which to read the data
#'
#' @return a \code{link{tbl_df}}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("data/accident_2015.csv.bz2")
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

#' Create a file name for certain year's data
#'
#' This is a helper function that creates "accident_2020.csv.bz2" style
#' file names.
#'
#' @param year an integer to be used in the file name to indicate the year
#'
#' @return a string in format accident_YYYY.csv.bz2 where YYYY is replaced by
#'      \code{year}
#'
#' @examples
#' make_filename(2020)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read accident data from files
#'
#' This function reads data from files named in format accident_YYYY.csv.bz2
#' where YYYY denotes the year which the data represents. Argument \code{years}
#' specifies which years' data is read.
#'
#' The function results in an error no file for a specified year is found or
#' the file contains invalid data.
#'
#' @param years a vector of years for which to read data
#'
#' @return a list of tibbles, each containing data for one year
#'
#' @examples
#' y <- c(2017, 2018, 2019)
#' fars_read_years(y)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Produce summary showing the number accidents for each month
#'
#' This function reads FARS data using \code{fars_read_years} and produces a
#' summary that shows the number of cases for each month.
#'
#' The function returns an error if \code{fars_read_years} is unable to read
#' data for specified years.
#'
#' @param years a vector of years for which to read data
#'
#' @return a tibble with columns "MONTH" (which has values 1-12) and one column
#'       per year. The values in these columns show the number of accidents
#'       in that month and year.
#'
#' @examples
#' y <- c(2017, 2018, 2019)
#' fars_summarize_years(y)
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot accidents on a map
#'
#' The function is used to create a map showing accidents in a specified US
#' state during a specified year.
#'
#' The function results in an error if \code{fars_read} fails to read data or if
#' the data contains no accidents found for the given state.
#'
#' @param state.num The number of the US state to plot. The states are numbered
#'     alphabetically (1 = Alabama, 2 = Alaska etc.)
#' @param year the year whose accidents to plot on the map
#'
#' @return This function draws a map. It does not return a value.
#'
#' @examples
#' fars_map_state(6, 2013)
#' fars_map_state(49, 2015)
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
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
