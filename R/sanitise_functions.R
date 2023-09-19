#' Sanitising functions
#'
#' Functions to clean and standardise taxonomic names and authors.
#'
#' `sanitise_name()` returns the sanitised name of a single taxonomic name.
#'
#' `sanitise_authors()` returns the sanitised name of a  taxonomic authors. Where by characters are coerced to Latin-ASCII, thereby removing diacritics (e.g umlauts).
#'
#' `clean_names_authors()` sanitises multiple taxonomic names with or without the corresponding authors, by applying `sanitise_name()` and `sanitise_authors()`. As input a vector of taxonomic names is required (`taxon_names`), in addition a vector of the authors (`taxon_authors`) or joined taxonomic name and authors (`taxon_names_full`) can be provided. If neither `taxon_authors` or `taxon_names_full` are provided the author names are set to `''`. A list is returned where:
#' -  `$taxon_name` a vector of the sanitised taxonomic names,
#' -  `$author`a vector of the sanitised authors,
#' -  `$sanitised` is a logical vector of whether the taxon_name was sanitised.
#'
#' `clean_names_authors_report()` applies `clean_names_authors()` to a collection where the inputs are:
#' -  a data frame of the collection (`original_report`),
#' - column name for the taxonomic names (`taxon_name_col`, required),
#' - column name of the authors of the taxonomic names  (`taxon_author_col`, optional),
#' - column name of the combined taxonomic name and author  (`taxon_name_full_col`, optional).
#'
#' @param original_report a dataframe of a collection
#' @param taxon_name_col the name of the column in the `original_report` corresponding to taxonomic names.
#' @param taxon_name_full_col the name of the column in the `original_report` corresponding to joined taxonomic names and authors.
#' @param taxon_author_col the name of the column in the `original_report` corresponding to the authors of the taxonomic names.
#' @param taxon_name taxonomic name of a plant.
#' @param taxon_names vector of taxonomic names.
#' @param taxon_names_full vector of joined taxonomic name and author.
#' @param taxon_authors vector of taxonomic authors.
#'
#' @export
#'
#' @examples
#' sanitise_name('TRIGONELLA afghanica')
#' sanitise_name('Halimium X pauanum')
#' sanitise_name('Aruncus dioicus var acuminatus')
#' sanitise_authors('Stehlé')
#'
#' taxon_names = c('TRIGONELLA afghanica', 'Halimium X pauanum', 'Aruncus dioicus var acuminatus', 'Eupatorium magdalenae')
#' taxon_authors = c('Vassilcz', 'Font Quer', '(Douglas ex Hook.) H.Hara', 'Stehlé')
#'
#' sanitise_names_authors(taxon_names, taxon_authors)
#'
#' collection = data.frame(names = taxon_names, full = paste0(taxon_names, ' ', taxon_authors))
#' sanitise_names_authors_report(collection, taxon_name_col = 'names', taxon_name_full_col = 'full')
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

#' @rdname sanitise_name
#' @export
sanitise_authors <- function(taxon_authors){
  stringi::stri_trans_general(taxon_authors, id = "Latin-ASCII") # simplify characters, i.e remove upstroph, tilde.
}


#' @rdname sanitise_name
#' @export
sanitise_names_authors <- function(taxon_names,
                                taxon_authors = NA,
                                taxon_names_full = NA){
  # A) Sanitise the taxon names.
  clean_taxon_name = unlist(lapply(taxon_names, sanitise_name))

  # B) Extract author if needed.
  # i) Both taxon_authors and taxon_names_full = NA
  if(length(taxon_authors) == 1  & length(taxon_names_full) == 1 & all(is.na(taxon_authors)) & all(is.na(taxon_names_full))){
    author = rep('',length(taxon_names))
  }
  # ii) Both taxon_authors is NA  and taxon_names_full is not
  else if(length(taxon_authors) == 1  & length(taxon_names_full) > 1 & all(is.na(taxon_authors))){
    author = author_from_taxon_name_full(taxon_names, taxon_names_full)
    author = sanitise_authors(author)
  }
  #iii) Taxon authors in not NA.
  else{
    author = taxon_authors
    author = sanitise_authors(author)
  }


  #C) Was sanitising needed.
  sanitised = rep(F,length(taxon_names))
  sanitised[taxon_names != clean_taxon_name] = T

  return(list(taxon_name = clean_taxon_name, author = author, sanitised = sanitised))
}

#' @rdname sanitise_name
#' @export
sanitise_names_authors_report <- function(original_report,
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

  return(sanitise_names_authors(taxon_names = taxon_names, taxon_names_full = taxon_name_full, taxon_authors = taxon_authors))
}
