##############################################################################
############################## FarsKevin Package #############################
##############################################################################

# fars_read function ------------------------------------------------------

#' fars_read
#'
#' This function shows a quick view of your data in an object of class tibble,
#' according to a specified CSV filename.
#'
#' @param filename A character string giving the filename the function will
#' visualize.
#'
#' @return This function returns a data frame of class tibble with the data
#' from the specified filename. If the filename does not exist, there will
#' be a message informing about it.
#'
#' @note There will be errors if the filename is absent, there are no
#' quotation marks in the filename argument or the file does not exist.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
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



# make_filename function --------------------------------------------------

#' make_filename
#'
#' This simple function makes a filename for the data associated to the US
#' National Highway Traffic Safety Administration's Fatality Analysis
#' Reporting System, according to a specified year.
#'
#' @param year A numeric value giving the year to make the filename.
#'
#' @return This function returns the created filename with quotation marks.
#'
#' @note There will be errors if the year argument is absent or if the year is
#' referenced to a non-existing object in the environment. If there is a
#' character string in the year argument, R will introduce NAs by coercion.
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



# fars_read_years function ------------------------------------------------

#' fars_read_years
#'
#' This function selects the month and year from each record in the files
#' associated to the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System, given one or more years.
#'
#' @param years A numeric vector giving the year or years from which to select.
#'
#' @return This function returns a list with data frames of class tibble with
#' the months and years from each record of the file.
#'
#' @note There will be errors if the year is absent or the if the year is
#' referenced to a non-existing object in the environment. There are warnings
#' when the year is invalid (no existing filename with that year).
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' fars_read_years(c(2013,2014))
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


# fars_summarize_years function -------------------------------------------

#' fars_summarize_years
#'
#' This function summarizes the number of times the months appear for a
#' specified vector of years.
#'
#' @param years A numeric vector giving the year or years from which to select
#'
#' @return This function returns a data frame with the amount of times every
#' month appears per year.
#'
#' @note There will be errors if the year is absent or the if the year is
#' referenced to a non-existing object in the environment. There are warnings
#' when the year or years are invalid (no existing filename with that year).
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


# fars_map_state -------------------------------------------------------------------------

#' fars_map_state
#'
#' This function makes a plot of the fatality traffic for a specified state of
#' the United States, according to the year established by the user.
#'
#' @param state.num A numeric value for the state of the United States to
#' analyze.
#'
#' @param year A numeric value giving the year to analyze.
#'
#' @return This function returns a geographic plot of the state where there has
#' been at least one fatality traffic accident for a particular year.
#'
#' @note There will be errors if any of the arguments are absent or if there are
#' no numeric values for the arguments.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(39, 2014)
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
