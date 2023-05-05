#' extract_year()
#'
#' A function that extracts the year from dates where the date can be in such formats as:
#' - DD/MM/YYYY
#' - YYYY-MM-DD
#' - MM/YYYY
#' - YYYY-MM
#' - YYYY
#' - DD Month YYYY
#'
#' The method works by extracting any patterns of 4 numbers in a row.
#' Thus, the year must be in the format YYYY. I.e. cannot have formats such as DD/MM/YY.
#'
#' @param date_vector A vector of dates
#'
#' @return a vector of years
#'
#' @examples
#' extract_year('03/05/2023')
#' extract_year('3rd of May 2023')
#' extract_year('2023-05-03')
#'
#' @export
extract_year <- function(date_vector){
  return(as.numeric(unlist(stringr::str_extract(date_vector, '[0-9]{4}'))))
}


#' Add status_year column to Botanic garden database.
#'
#' @param data BG database
#' @param ItemStatusDate_column The name of the column containing the Taxon name.
#'
#' @return The BG database with a new column called status_year.
#' @export
add_status_year <- function(data, ItemStatusDate_column = 'ItemStatusDate'){
  ItemStatusdate = data[,match(ItemStatusDate_column, names(data))]
  status_year = extract_year(ItemStatusdate)
  return(data.frame(data,status_year = status_year))
}
