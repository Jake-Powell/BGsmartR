#' sanitise_name()
#'Converts taxon names such that only the first letter of the Genus is capitalised and all remaining characters are lower case.
#'
#' @param taxon_name taxon name
#'
#' @return sanitised taxon name
#' @export
#'
#' @examples
#' sanitise_name('TRIGONELLA smyrnaea')
sanitise_name <- function(taxon_name){
  # If the starting letter means hybrid (x,+,\u00D7)
  if(grepl('^[+\u00D7]|^x ',taxon_name)){
    taxon_part = stringr::str_sub(taxon_name,3,-1)
    taxon_part = gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", taxon_part, perl = TRUE)
    taxon_name = paste0(stringr::str_sub(taxon_name,1,2), taxon_part, collapse ='')
    return(taxon_name)
  }
  # First letter capital the rest lower.
  taxon_name = gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", taxon_name, perl = TRUE)

  return(taxon_name)
}
