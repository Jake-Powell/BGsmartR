#' Blank Matching criterion
#'
#' @param enrich_database_extract extract of database (often records with identical taxonomic names)
#' @param message Matching message.
#'
#' @return Always returns a list of 3 with:
#' - `$row` =`-2` set to no match.
#' - `$message` A combination of the input message and `unclear, do not match'`.
#' @export
#'
no_additional_matching <- function(enrich_database_extract, message = ''){
  # No matching criteria
  matched = 1:nrow(enrich_database_extract)
  message = paste0('(', message, 'unclear, do not match',')',collapse = '')

  return(list(row = matched, message = message))
}
