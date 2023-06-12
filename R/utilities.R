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
    if(is.na(taxon_names_full[i])){
      authors[i] = ''
    }
    else if(taxon_names_full[i] != ''){
      auth_cur = stringr::str_replace_all(taxon_names_full[i],taxon_name_words_grepl[i],'')
      authors[i] = stringr::str_squish(auth_cur)
    }
    else{
      authors[i] = ''
    }

  }
  authors = stringi::stri_trans_general(authors, id = "Latin-ASCII") # simplify characters, i.e remove upstroph, tilde.
  return(authors)
}

#' clean_names_authors()
#'
#' @param taxon_names  taxon names
#' @param taxon_authors taxon_authors
#' @param taxon_name_full taxon names with author
#'
#' @return list of cleaned taxon names and author
#' @export
clean_names_authors <- function(taxon_names,
                                taxon_authors = NA,
                                taxon_name_full = NA){
  # A) Sanitise the taxon names.
  clean_taxon_name = unlist(lapply(taxon_names, sanitise_name))

  # B) Extract author if needed.
  # i) Both taxon_authors and taxon_name_full = NA
  if(length(taxon_authors) == 1  & length(taxon_name_full) == 1 & all(is.na(taxon_authors)) & all(is.na(taxon_name_full))){
    author = rep('',length(taxon_names))
  }
  # ii) Both taxon_authors is NA  and taxon_name_full is not
  else if(length(taxon_authors) == 1  & length(taxon_name_full) > 1 & all(is.na(taxon_authors))){
    author = author_from_taxon_name_full(taxon_names, taxon_name_full)
  }
  #iii) Taxon authors in not NA.
  else{
    author = taxon_authors
  }


  #C) Was sanitising needed.
  sanitised = rep(F,length(taxon_names))
  sanitised[taxon_names != clean_taxon_name] = T

  return(list(taxon_name = clean_taxon_name, author = author, sanitised = sanitised))
}


#' clean_names_authors_report
#'
#' @param original_report original_report
#' @param taxon_name_col taxon_name_col
#' @param taxon_name_full_col taxon_name_full_col
#' @param taxon_author_col taxon_author_col
#'
#' @return list of cleaned taxon names and author
#' @export
clean_names_authors_report <- function(original_report,
                                       taxon_name_col = 'TaxonName',
                                       taxon_name_full_col = NA,
                                       taxon_author_col = NA){
  # Get the values out of original report.
  taxon_names = original_report[,match(taxon_name_col, names(original_report))]
  if(is.na(taxon_name_full_col)){
    taxon_name_full = NA
  }else{
    taxon_name_full = original_report[,match(taxon_name_full_col,names(original_report))]
  }

  if(is.na(taxon_author_col)){
    taxon_authors = NA
  }else{
    taxon_authors = original_report[,match(taxon_author_col,names(original_report))]
  }

  return(clean_names_authors(taxon_names = taxon_names, taxon_name_full = taxon_name_full, taxon_authors = taxon_authors))
}
#' author_check()
#'
#' @param original_author original_author
#' @param proposed_author proposed_author
#'
#' @return message of author comparision
#' @export
#'
author_check <- function(original_author, proposed_author){
  if(is.na(original_author) || is.na(proposed_author)){
    return('Different')
  }
  # Check if the authors are identical.
  if(original_author == proposed_author){
    return('Identical')
  }
  # strip each author into words.
  original_words = author_words(original_author)
  proposed_words = author_words(proposed_author)
  orig_words_in_proposed = FALSE
  prop_words_in_original = FALSE
  if(length(original_words) > 0){
    orig_words_in_proposed = unlist(lapply(original_words, function(x){grepl(x,proposed_author)}))
  }
  if(length(proposed_words) > 0){
    prop_words_in_original= unlist(lapply(proposed_words, function(x){grepl(x,original_author)}))
  }
  if(any(c(orig_words_in_proposed, prop_words_in_original))){
    return('Partial')
  }

  return('Different')
}

#' author_words()
#'
#' @param author Author that wants splitting into words.
#'
#' @return author words
#' @export
#'
author_words <- function(author){
  author_wordsA = unlist(stringr::str_extract_all(author,'[A-Z]{1}[a-z-]{2,}'))
  author_wordsB = unlist(stringr::str_extract_all(author,'DC\\.|Sm\\.|Br\\.'))
  author_wordsB = stringr::str_replace_all(author_wordsB,'\\.','\\\\.')
  return(c(author_wordsA,author_wordsB))
}
