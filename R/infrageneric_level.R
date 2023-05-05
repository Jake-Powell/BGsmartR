#' infrageneric_level()
#'
#' This functions extracts the infrageneric level from a taxon name, in particular:
#'
#' - `0.indet`: The plant is indeterminate.
#' - `1.species`: The plant is a species.
#' - `2.subsp`: The plant is a subspecies.
#' - `3.var`: The plant is a variety.
#' - `4.f`: The plant is
#' - `5.cultivar`: The plant is a cultivar.
#' - `6.hybrid`: The plant is a hybrid.
#'
#'  `0-4` are biological characteristics and each plant can only have one of these characteristics.
#'  `5-6` are horticultural characteristics and plants can have none, one or both of these.
#'
#'
#' @param taxonName taxon name
#'
#' @return The infrageneric levels of the plant
#' @export
#'
#' @examples
#'  \donttest{
#' # 0.indet
#' infrageneric_level("\xd7 Aridaria sp.")
#'
#' # 1.species
#' infrageneric_level("Saxegothaea conspicua")
#'
#' # 2.subsp
#' infrageneric_level("Rhododendron charitopes subsp. tsangpoense")
#'
#' # 3.var
#' infrageneric_level("Rhododendron saluenense var. prostratum")
#'
#' # 4.f
#' infrageneric_level("Aquilegia flabellata f. alba")
#'}
infrageneric_level <- function(taxonName){

  if(taxonName ==''){return(NA)}
  # A) Split the taxon name into individual words.
  no_words = stringr::str_count(stringr::str_squish(taxonName),' ') +1
  if(is.na(no_words)){return(NA)}

  # B) groups stores the infrageneric_levels that are found to describe taxon name
  groups = NULL

  # C) Go through each infrageneric level and add to groups if the correct pattern matches.

  # 0) "0.Indet"
  if(no_words == 1 | grepl(" sp\\.|indet\\.|unkn| cf\\.|aff\\.|spec\\.",taxonName)){
    groups = c(groups, '0.indet')
  }

  # 1) "1.species".
  if(no_words == 2){
    groups = c(groups, '1.species')
  }

  # 2) "2.subsp"
  if (grepl('subsp\\.', taxonName)){
    groups = c(groups, '2.subsp')
  }

  # 3) "3.var"
  if (grepl('var\\.', taxonName)){
    groups = c(groups, '3.var')
  }

  # 4) "4.f"
  if (grepl(' f\\.', taxonName)){
    groups = c(groups, '4.f')
  }

  # 5) "5.cultivar"
  if (grepl("cv\\.|'.*?'|CV|cv$|\\[.*?\\]|hort\\.", taxonName)){
    groups = c(groups, '5.cultivar')
  }

  # 6) "6.hybrid"
  if (grepl('hybrid$|Hybrid|HYBRID|gx\\.| gx | gx|\u00D7', taxonName)){
    groups = c(groups, '6.hybrid')
  }

  # 6) "6.hybrid" (this checkes the hex code rather than unicode for 'x' not sure if this will be needed once encoding issues are sorted)
  if (stringr::str_detect(taxonName,'\xd7')){
    groups = c(groups, '6.hybrid')
  }

  groups = unique(groups)
  if(is.null(groups)){
    return(NA)}

  # D) If we have multiple biological characteristics we need to remove excess ones
  # Such as choosing the biological characteristic with the smallest scope.
  # or removing species if we also have cultivar (i.e taxonname length  = 2 but one of the words will be cv. etc)
  if(length(groups) >1){
    # if we have 4.f remove 3.var and 2.subs
    if('4.f' %in% groups){
      groups = groups[!grepl('2|3',groups)]
    }
    # if we have 3.var remove 2.subs
    if('3.var' %in% groups){
      groups = groups[!grepl('2',groups)]
    }
    # if we have 0.indet then remove biological characteristics (i.e "Astragalus sp.", "Mammillaria sp. f. cristata" )
    if('0.indet' %in% groups){
      groups = groups[!grepl('1|2|3|4',groups)]
    }
    # If we have cultivar and species then this is a fake species i.e length = 2 but contains cv or 'XX' so remove species.
    if('5.cultivar' %in% groups){
      groups = groups[!grepl('1',groups)]
    }
    # If we have hybrid and species then this is a fake species i.e length = 2 but contains gx so remove species.
    if('6.hybrid' %in% groups){
      groups = groups[!grepl('1',groups)]
    }
    # If we find [HYBRID] this is not a cultivar so remove.
    if(grepl('\\[HYBRID\\]', taxonName)){
      groups = groups[!grepl('5',groups)]
    }
    # If we have indet and one hort category return only indet.
    if('0.indet' %in% groups & length(groups) ==2){
      groups = groups[!grepl('5|6',groups)]
    }
  }

  return(paste0(groups,collapse = ', '))
}



#' Add infrageneric_level column to Botanic garden database.
#'
#' @param data BG database
#' @param TaxonNameColumn The name of the column containing the (original) Taxon name.
#' @param POWO_TaxonNameColumn The name of the column containing the POWO (WCVP) Taxon name.
#' @param progress_bar Logical flag, if TRUE show progress bar, else no progress bar
#'
#' @return The BG database with a new column called infrageneric_level
#' @export
add_infrageneric_level <- function(data, TaxonNameColumn = 'TaxonName', POWO_TaxonNameColumn = NULL, progress_bar = FALSE){
  ####
  # 1) Get taxon names to use for infrageneric_level
  ####
  #A) we DON'T have POWO_TaxonNameColumn.
  if(is.null(POWO_TaxonNameColumn)){
    taxon_names = data[,match(TaxonNameColumn, names(data))]
  }
  #B) we DO have POWO_TaxonNameColumn.
  else{
    taxon_names = data[,match(POWO_TaxonNameColumn, names(data))]
    original_taxon_names = data[,match(TaxonNameColumn, names(data))]
    taxon_names[is.na(taxon_names)] = original_taxon_names[is.na(taxon_names)]
  }

  ###
  # 2) Get the infrageneric level.
  ###
  if(progress_bar){
    infrageneric_levels = unlist(pbapply::pblapply(taxon_names, infrageneric_level))
  }
  else{
    infrageneric_levels = unlist(lapply(taxon_names, infrageneric_level))
  }

  ###
  # 3) Return data with infrageneric_levels added.
  ###
  return(data.frame(data, infrageneric_level = infrageneric_levels))
}
