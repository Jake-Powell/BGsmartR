#' Get accepted plant details from POWO online
#'
#'This function queries POWO online for the accepted name of a plant and the corresponding Plants of the World Online identifier.
#'
#'If the connection to POWO doesn't work (or exist) then the function returns (NA, NA).
#'
#' @param powo_id Plants of the World Online identifier
#'
#' @return A vector of length two containing the accepted name and accepted powo_id.
#' @export
#'
#' @examples
#' get_accepted_plant('582307-1')
#'
get_accepted_plant <- function(powo_id) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully
      powo_info = taxize::pow_lookup(paste0('urn:lsid:ipni.org:names:',powo_id))
      new_accepted_name = powo_info$meta$accepted$name
      new_accepted_id = stringr::str_remove(powo_info$meta$accepted$fqId, 'urn:lsid:ipni.org:names:')
      return(c(new_accepted_name, new_accepted_id))

      # The return value of `readLines()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      # message(paste("URL does not seem to exist for powo:", powo_id))
      # message("Here's the original error message:")
      # message(cond)
      # Choose a return value in case of error
      return(c(NA,NA))
    },
    warning=function(cond) {
      message(paste("URL caused a warning for powo:", powo_id))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }



  )
}

#' Import wcvp_names.csv
#'
#' Given the path to `wcvp_names.csv` file obtained from POTW website load the relevant information into R and extract the information required to perform matching to garden databases.
#'
#' The columns extracted always include:"plant_name_id", "taxon_name", "taxon_authors", "taxon_rank", "accepted_plant_name_id" and "powo_id". Further columns can be extracted using `wanted_columns` parameter, where either columns names or indices can be used.
#'
#' In particular, we add two new columns:
#' - 'taxon_length' which contains the number of characters in a given taxon_name used when searching for typos in garden databases.
#' - 'is_autonym' contains a logical flag for whether the taxon name is an autonym. This is used to match autonyms in garden databases.
#'
#' Moreover, the code performs fixes to the data provided in `wcvp_names.csv`, namely:
#' - Checks for '[*]' and '[**]' in taxon_name and taxon_rank, which should be 'var.' and 'f.' instead.
#' - There are plants that are synonyms without an accepted form in `wcvp_names.csv`. However, in some cases the accepted form can be found on POWO online. Therefore, in this case we scrap POWO online for an accepted form and add to the loaded information from `wcvp_names.csv`.
#'
#' @param filepath path to wcvp_names.csv file.
#' @param wanted_columns specify extra columns to extract from `wcvp_names.csv`.
#' @return Desired information from wcvp_names.csv
#' @export
#'
import_wcvp_names <- function(filepath, wanted_columns = c("plant_name_id", "taxon_rank", "taxon_status", "family", "genus", "species", "lifeform_description", "climate_description", "taxon_name", "taxon_authors", "accepted_plant_name_id", "powo_id")){

  # 1) Load wcvp_names (using method as described in the download)
  print('(1/6) Loading wvcp_names.csv...')
  wcvp_names = utils::read.table(filepath, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
  columns  = match(c("plant_name_id", "taxon_name", "taxon_authors", "taxon_rank", "taxon_status", "accepted_plant_name_id", "powo_id"), names(wcvp_names))
  if(is.character(wanted_columns)){
    wanted_columns = match(wanted_columns, names(wcvp_names))
  }
  columns = unique(c(columns,wanted_columns))
  wcvp_names = wcvp_names[,columns]

  # 2) Create new column for the length of the taxonName (used if we search for typos
  print('(2/6) Adding taxon_length column...')
  taxon_length = unlist(lapply(wcvp_names$taxon_name, stringr::str_length))
  wcvp_names$taxon_length = taxon_length

  # 3) Create new column for whether the taxon name is an autonym. (used for checking for matches)
  print('(3/6) Adding is_autonym column...')
  wcvp_names = add_is_autonym(wcvp_names, progress_bar = T, TaxonName_column = 'taxon_name')

  # 3) Where possible update records that are synonyms without an accepted form to include an accepted form via powo.
  print('(4/6) Checking for accepted form issues with synonyms...')
  synonyms =wcvp_names[wcvp_names$taxon_status == 'Synonym',]
  non_accepted_synonyms = synonyms[is.na(synonyms$accepted_plant_name_id),]
  powo_ids = non_accepted_synonyms$powo_id
  print(paste0('  found ', length(powo_ids), ' synonyms without accepted form...'))
  print('  searching powo online for accepted forms...')
  pbapply::pboptions(type = "txt")
  accepted_details = pbapply::pblapply(powo_ids,get_accepted_plant)
  new_accepted_name = unlist(lapply(accepted_details, function(x){x[1]}))
  new_accepted_id = unlist(lapply(accepted_details, function(x){x[2]}))

  wcvp_names[wcvp_names$taxon_status == 'Synonym',][is.na(synonyms$accepted_plant_name_id),]$accepted_plant_name_id = new_accepted_id

  remaining_NA = sum(is.na(new_accepted_id))
  fixed_synonyms = 100 - remaining_NA/length(new_accepted_name)*100
  print(paste0('  Fixed ', round(fixed_synonyms,digits=1),'% of missing accepted forms...'))
  print(paste0('  ', remaining_NA,' remaining unexplained missing accepted forms...'))

  # 4)  Check for common hybrids, cultivars in wcvp to check we can definitely exclude these initally. Find exceptions that are included in wcvp_names.
  print('(5/6) Checking for common hybrids, cultivar, etc symbols...')

  # A)  Note current wcvp has a bug where [*] and [**] are sometimes used instead if var. and f.
  # Fix this here
  with_sqbracket = wcvp_names[stringr::str_count(wcvp_names$taxon_name, "\\[") >= 1,]
  with_sqbracket_original = with_sqbracket
  with_sqbracket$taxon_name = stringr::str_replace(with_sqbracket$taxon_name,'\\[\\*\\*\\]', 'f\\.')
  with_sqbracket$taxon_name = stringr::str_replace(with_sqbracket$taxon_name,'\\[\\*\\]', 'var\\.')
  with_sqbracket$taxon_rank = stringr::str_replace(with_sqbracket$taxon_rank,'\\[\\*\\*\\]', 'Form')
  with_sqbracket$taxon_rank = stringr::str_replace(with_sqbracket$taxon_rank,'\\[\\*\\]', 'Variety')
  wcvp_names[stringr::str_count(wcvp_names$taxon_name, "\\[") >= 1,] = with_sqbracket
  with_sqbracket2 = wcvp_names[stringr::str_count(wcvp_names$taxon_name, "\\[") >= 1,]

  # B) Contains multiple apostrophe.
  with_mult_apostrophe = wcvp_names[stringr::str_count(wcvp_names$taxon_name, "'") >= 2,]

  # C) Is indeterminant by ' sp. '.
  with_spdot = wcvp_names[stringr::str_count(wcvp_names$taxon_name, " sp\\. ") >= 1,]

  # D) Is a hybrid by ' gx '.
  with_gx = wcvp_names[stringr::str_count(wcvp_names$taxon_name, " gx ") >= 1,]

  # E) is indeterminant by starting with 'Indet '.
  with_indet = wcvp_names[stringr::str_count(wcvp_names$taxon_name, "^Indet ") >= 1,]

  # Join all exceptions into one data.frame.
  exceptions = rbind(with_sqbracket2, with_mult_apostrophe, with_spdot,with_gx, with_indet)


  # 5) wcvp names into two wcvp_single and wcvp_multiple. (to allow for matching when there is a single or multiple records in wcvp)
  print('(6/6) Splitting wcvp_names into those with multiple TaxonNames and those with only one...')
  name_freq = table(wcvp_names$taxon_name)
  wcvp_single = wcvp_names[wcvp_names$taxon_name %in% names(name_freq)[as.numeric(name_freq) == 1],]
  wcvp_multi =  wcvp_names[wcvp_names$taxon_name %in% names(name_freq)[as.numeric(name_freq) > 1],]

  # 7) Log the changes.
  #add missing accepted form.
  changes = NULL

  indices = !is.na(new_accepted_name)
  if(sum(indices)>0){
    changes = data.frame(issue = rep('synonym with missing accepted form',sum(indices)),
                         powo_id = non_accepted_synonyms$powo_id[indices],
                         taxon_name = non_accepted_synonyms$taxon_name[indices],
                         issue_entry = paste0('accepted_plant_name_id = ',non_accepted_synonyms$accepted_plant_name_id[indices]),
                         fix = paste0('accepted_plant_name_id = ',new_accepted_id[indices], ' (',new_accepted_name[indices],')'))
  }

  #add square bracket issue.
  indices = with_sqbracket$taxon_name != with_sqbracket_original$taxon_name
  if(sum(indices)>0){
    changesB = data.frame(issue = rep('error in taxon name and taxon rank with [*] or [**]',sum(indices)),
                          powo_id = with_sqbracket_original$powo_id[indices],
                          taxon_name = with_sqbracket_original$taxon_name[indices],
                          issue_entry = paste0('taxon_name = ',with_sqbracket_original$taxon_name[indices], ' and taxon_rank = ',with_sqbracket_original$taxon_rank[indices]),
                          fix = paste0('taxon_name = ',with_sqbracket$taxon_name[indices], ' and taxon_rank = ',with_sqbracket$taxon_rank[indices]))
    changes = rbind(changes, changesB)
  }


  # 8) return
  return(list(wcvp_single = wcvp_single,
              wcvp_multi = wcvp_multi,
              exceptions = exceptions,
              changes = changes))
}


#' Import wcvp_distribution.csv
#'
#' Given the path to `wcvp_distribution.csv` file obtained from POTW website load the relevent information into R.
#'
#' @param filepath path to wcvp_distribution.csv file.
#'
#' @return Desired information from wcvp_distribution.csv
#' @export
#'
import_wcvp_distribution <- function(filepath){
  # 2) Load and select columns
  wcvp_distribution = utils::read.table(filepath, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
  return(wcvp_distribution)
}

#' Find if a Taxon name is an autonym
#'
#' This function calculates whether a Taxon Name is an autonym.
#'
#' @param name  Taxon name of a plant
#' @return TRUE if the Taxon name is an autonym, otherwise FALSE.
#'
#' @examples
#' # An autonym.
#' is_autonym("Codiaeum variegatum var. variegatum")
#' # Not an autonym.
#' is_autonym("Crinum pedunculatum f. purple")
#'
#' @export
is_autonym <- function(name){
  # 1) Is the plant a hybrid, i.e contains '×' (unicode \u00D7)
  if(grepl('\u00D7',name)){
    return(FALSE)
  }

  # 2) Split word by level i.e var. f., etc.
  # And 'squish' the two resultant parts (i.e remove excess whitespace)
  split_name = stringr::str_split(name,' var\\. | subsp\\. | f\\. | ssp\\. | nothosubsp\\. ')[[1]]
  split_name = unlist(lapply(split_name, stringr::str_squish))

  # 3) If there is only one chunk return no (i.e no var., f., etc)
  if(length(split_name) < 2){return(FALSE)}


  # 4) Split each chunk into words.
  split_parts = stringr::str_split(split_name,' ')

  # 5) Loop over each pair of chunks and check for autonym.
  for(i in 1:(length(split_name)-1)){
    # If the length of the second part is more than one word then cultivars (i.e will most likely contain '' or []).
    if(length(split_parts[[i+1]]) > 1){ return(FALSE)}

    #Get the word either side of the split (var./f./subsp.)
    pre_word = split_parts[[i]][length(split_parts[[i]])]
    post_word = split_parts[[i+1]][1]

    # If the words don't math return false.
    if(pre_word != post_word){
      return(FALSE)
    }

  }
  # Each chunk matches so return true.
  return(TRUE)

}

#' Add is_autonym column to Botanic garden database.
#'
#' @param data BG database
#' @param progress_bar Logical flag, if TRUE show progress bar, else no progress bar
#' @param TaxonName_column The name of the column containing the Taxon name.
#'
#' @return The BG database with a new column called is_autonym which flags where the Taxon name is an autonym
#' @export
add_is_autonym <- function(data, progress_bar = FALSE, TaxonName_column = 'TaxonName'){
  if(progress_bar){
    autonyms = unlist(pbapply::pblapply(data[,TaxonName_column], is_autonym))
  }
  else{
    unlist(lapply(data[,TaxonName_column], is_autonym))
  }
  data_new = data.frame(data, is_autonym = autonyms)
  return(data_new)
}








#' Title
#'
#' @param dataset data frame containing botanical garden reports
#'
#' @return data frame with taxonomic standardisation included.
#' @export
#'
#' @examples
taxonomic_standardisation <- function(dataset){
  # original_report <- read.delim(file = '/Users/jakepowell/Cambridge/OriginalBGSmartR/0_good_reports/Cooktown_original_report.csv', header = T, sep = ",")
  # original_report <- original_report |> dplyr::filter(ItemType == "Planting")
  # original_report <- original_report |> dplyr::filter(!ItemStatusType == "Unknown")
  # original_report <- original_report |> dplyr::filter(!ItemStatusType == "Procedure")
  # dataset = original_report
  #
  # # 1) Add is_autonym to original report. Reformat some columns.
  # autonyms = unlist(lapply(original_report$TaxonName, is_autonym))


}
