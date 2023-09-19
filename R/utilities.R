#' author_from_taxon_names_full()
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
  # authors = stringi::stri_trans_general(authors, id = "Latin-ASCII") # simplify characters, i.e remove upstroph, tilde.
  return(authors)
}




#' author_check()
#'
#' @param original_author original_author
#' @param proposed_author proposed_author
#'
#' @return message of author comparison
#' @export
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
author_words <- function(author){
  author_wordsA = unlist(stringr::str_extract_all(author,'[A-Z]{1}[a-z-]{2,}'))
  author_wordsB = unlist(stringr::str_extract_all(author,'DC\\.|Sm\\.|Br\\.'))
  author_wordsB = stringr::str_replace_all(author_wordsB,'\\.','\\\\.')
  return(c(author_wordsA,author_wordsB))
}
