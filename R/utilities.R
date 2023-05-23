#' sanitise_name()
#'
#'Converts taxon names such that only the first letter of the Genus is capitalised and all remaining characters are lower case.
#'
#'Also change to correct hybrid symbol where needed.
#'
#' @param taxon_name taxon name
#'
#' @return sanitised taxon name
#' @export
#'
#' @examples
#' sanitise_name('TRIGONELLA smyrnaea')
sanitise_name <- function(taxon_name){
  ###########
  ## 1) fix x/X/h/H to \u00D7.
  ###########
  if(grepl(' [xXhH] |^[xXhH] | [xXhH]$',taxon_name)){
    taxon_name = stringr::str_replace(taxon_name,'^[xX] ','\u00D7 ')
    taxon_name = stringr::str_replace(taxon_name,' [xX]$',' \u00D7')
    taxon_name = stringr::str_replace(taxon_name,' [xX] ',' \u00D7 ')
  }

  ###########
  ## 2) Fix casing only first letter of Genus Capital rest lower
  ###########
  # If the starting letter means hybrid (x,+,\u00D7)
  if(grepl('^[+\u00D7]',taxon_name)){
    taxon_part = stringr::str_sub(taxon_name,3,-1)
    taxon_part = gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", taxon_part, perl = TRUE)
    taxon_name = paste0(stringr::str_sub(taxon_name,1,2), taxon_part, collapse ='')
  }
  else{
    # First letter capital the rest lower.
    taxon_name = gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", taxon_name, perl = TRUE)
  }

  ###########
  ## 3) fix f or var to f. and var.
  ###########
  if(grepl(' f | var | subsp | v ',taxon_name)){
    taxon_name = stringr::str_replace(taxon_name,' f ',' f\\. ')
    taxon_name = stringr::str_replace(taxon_name,' var | v ',' var\\. ')
    taxon_name = stringr::str_replace(taxon_name,' subsp ',' subsp\\. ')
    taxon_name = stringr::str_replace(taxon_name,' nothosubsp ',' nothosubsp\\. ')

  }
  ###########
  ## 3) Remove excess whitespace.
  ###########
  taxon_name = stringr::str_squish(taxon_name)

  return(taxon_name)
}


#' author_from_taxon_name_full()
#'
#' @param taxon_names taxon names
#' @param taxon_names_full taxon names with author
#'
#' @return only author
#' @export
#'
author_from_taxon_name_full <- function(taxon_names, taxon_names_full){
  #As we're using grepl need to add escape for special characters
  taxon_names = stringr::str_replace_all(taxon_names,pattern = '\\.', '\\\\.')
  taxon_names = stringr::str_replace_all(taxon_names,pattern = '\\[', '\\\\[')
  taxon_names = stringr::str_replace_all(taxon_names,pattern = '\\]', '\\\\]')
  taxon_names = stringr::str_replace_all(taxon_names,pattern = '\\+', '\\\\+')
  taxon_names = stringr::str_replace_all(taxon_names,pattern = '\\?', '\\\\?')
  taxon_names = stringr::str_replace_all(taxon_names,pattern = '\\(', '\\\\(')
  taxon_names = stringr::str_replace_all(taxon_names,pattern = '\\)', '\\\\)')

  # Convert taxon name to a grepl statement, where AA BB goes to AA|BB.
  taxon_name_words_grepl = unlist(lapply(taxon_names, function(x){
    words = stringr::str_split(x, ' ')[[1]]
    return(paste0(words,collapse='|'))
  }))

  #Loop over all taxon name full removing any word that is also in taxon name.
  authors = rep(NA,length(taxon_names_full))
  for(i in 1:length(authors)){
    if(taxon_names_full[i] != ''){
      auth_cur = stringr::str_replace_all(taxon_names_full[i],taxon_name_words_grepl[i],'')
      authors[i] = stringr::str_squish(auth_cur)
    }
    else{
      authors[i] = ''
    }

  }
  return(authors)
}
