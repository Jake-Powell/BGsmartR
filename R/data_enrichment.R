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
add_is_autonym <- function(data, TaxonName_column = 'TaxonName', progress_bar = FALSE){
  if(progress_bar){
    autonyms = unlist(pbapply::pblapply(data[,TaxonName_column], is_autonym))
  }
  else{
    autonyms = unlist(lapply(data[,TaxonName_column], is_autonym))
  }
  data_new = data.frame(data, is_autonym = autonyms)
  return(data_new)
}

#' known_not_in_wcvp()
#'
#' A function that finds the taxon names which are known not to be in POWO. In particular this includes:
#' - includes ` sp.`
#' - includes ` gx `
#' - includes `'XX'` for some text XX. This is common notation for XX.
#' - includes `[`
#' - Begins with `Indet`. This is an indetminant and therefore won't be in POWO.
#'
#' @param taxon_names A vector of taxon names
#'
#' @return A vector of the indices in taxon_names that can be removed.
#' @export
known_not_in_wcvp <- function(taxon_names){
  return(which(grepl(" sp.| gx |'.*?'|\\[|^Indet",taxon_names)))
}


#' match_single_wcvp()
#'
#' Given the taxon names you want to match (`taxon_names`), the cleaned version of plants of the world online ( `wcvp` imported through the function `import_wcvp_names()`) and the indices of `wcvp`  records that you want to math to (`wcvp_search_index`). As this function is designed to match to unique taxon names the user should restrict the search indices to those with unique taxon names or a subset of those names.
#'
#' The matched index corresponds to the full `wcvp` data set. If not match is found then the function returns `NA`.
#'
#' Moreover, the function creates a message for each taxon name detailing when a match is found and the powo identifier and taxon name of the match in `wcvp`.
#'
#' @param taxon_names a vector of taxon names
#' @param wcvp cleaned POWO database
#' @param wcvp_search_index Indices of `wcvp` that we want to search for a match
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`
#' `$message` a message detailing the match.
#'
#' @export
#'
match_single_wcvp <- function(taxon_names, wcvp, wcvp_search_index){

  # A) Setup message.
  message = rep('', length(taxon_names))

  # B) Perform the matching.
  match_to_single = match(taxon_names, wcvp$wcvp_names$taxon_name[wcvp_search_index])

  # C) Find the indices of the match for both taxon names and wcvp.
  orep_index_match = (1:length(taxon_names))[!is.na(match_to_single)]
  wcvp_index_match = wcvp_search_index[match_to_single[!is.na(match_to_single)]]

  # D) Update message.
  message[orep_index_match] = paste0(message[orep_index_match], ' -> (matches POWO record with single entry) -> (', wcvp$wcvp_names$powo_id[wcvp_index_match],
                                     ', ', wcvp$wcvp_names$taxon_name[wcvp_index_match],
                                     ')')


  return(list(match = wcvp_search_index[match_to_single], message = message))
}

#' get_match_from_multiple()
#'
#' Given a taxon name and its full version we find the records in POWO that share the same taxon name. We then choose the record to match to by:
#'
#' - Checking if the full taxon name contains the author described within POWO database. If we have a single match this is chosen.
#' - If not, check for POWO records that are accepted. If a single record is accepted then this match is chosen.
#' - If not, check for records that have lifeform descriptions. If we have a single record this is chosen.
#' - Else, we cannot find a match and no record is chosen.
#'
#' @param taxon_name_and_full the pair of taxon name and taxon name full.
#' @param wcvp_mult POWO database restricted to records that do not have a unique taxon name.
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`
#' `$message` a message detailing the match.
#' @export
get_match_from_multiple <- function(taxon_name_and_full, wcvp_mult){
  # 1) Split taxon name and taxon full
  taxon_name_current = taxon_name_and_full[1]
  taxon_full_current = taxon_name_and_full[2]

  # 2) Get the corresponding records in wcvp_mult.
  # As we use grepl to match authors add escape characters (\\).
  POWO_cur = wcvp_mult[wcvp_mult$taxon_name == taxon_name_current,]
  POWO_cur$taxon_authors = stringr::str_replace_all(POWO_cur$taxon_authors,'\\.', '\\\\.')
  POWO_cur$taxon_authors = stringr::str_replace_all(POWO_cur$taxon_authors,'\\(', '\\\\(')
  POWO_cur$taxon_authors = stringr::str_replace_all(POWO_cur$taxon_authors,'\\)', '\\\\)')

  ###
  # 3) Get the match by: match author, accepted.
  ###
  flag = T
  #A) By author.
  author_match = unlist(lapply(POWO_cur$taxon_authors, function(x){grepl(x,taxon_full_current)}))
  if(any(author_match)){
    matched = POWO_cur$plant_name_id[author_match][1]
    matched = match(matched,wcvp_mult$plant_name_id)
    message = '(Multiple POWO records, match by author)'
    flag = F
  }

  # B) By accepted.
  if(flag){
    accepted_names =  POWO_cur$taxon_status == "Accepted"
    if(any(accepted_names)){
      matched = POWO_cur$plant_name_id[accepted_names]
      matched = match(matched,wcvp_mult$plant_name_id)
      message = '(Multiple POWO records, match to accepted name)'
      flag = F
    }
  }


  # C) use record that has lifeform description.
  if(flag){
    has_lifeform_description =  POWO_cur$lifeform_description != ""
    if(any(has_lifeform_description)){
      matched = POWO_cur$plant_name_id[has_lifeform_description]
      matched = match(matched,wcvp_mult$plant_name_id)
      message = '(Multiple POWO records, match to record with lifeform_description)'
      flag = F
    }
  }

  # D) Do not match. (set to -2 for we don't know what to match to)
  if(flag){
    matched = -2
    message = '(Multiple POWO records, unclear do not match)'
  }
  return(list(match = matched, message = message))
}

#' match_mult_wcvp()
#'
#' Given the taxon names and full taxon names you want to match (`taxon_names`, `taxon_names_full`), the cleaned version of plants of the world online ( `wcvp` imported through the function `import_wcvp_names()`) and the indices of `wcvp`  records that you want to math to (`wcvp_search_index`). As this function is designed to match to records that share taxon names the user should restrict the search indices to those with non-unique taxon names or a subset of those names.
#'
#' The matching of individual taxon names is completed by `get_match_from_multiple()`.
#'
#' The matched index corresponds to the full `wcvp` data set. If a match is found but we cannot ascertain which record to match to in POWO we set the match index to `-2`. If the taxon name is not found in POWO then the function returns `NA`.
#'
#' Moreover, the function creates a message for each taxon name detailing when a match is found and the powo identifier and taxon name of the match in `wcvp`.
#'
#' @param taxon_names  a vector of taxon names
#' @param taxon_names_full a vector of full taxon names (corresponding to `taxon_names`)
#' @param wcvp cleaned POWO database
#' @param wcvp_search_index Indices of `wcvp` that we want to search for a match
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`
#' `$message` a message detailing the match.
#' @export
match_mult_wcvp <- function(taxon_names,taxon_names_full, wcvp, wcvp_search_index){
  # A) Setup.
  match_to_multiple = rep(NA,length(taxon_names))
  message = rep('',length(taxon_names))
  wcvp_multiple = wcvp$wcvp_names[wcvp_search_index,]


  # 1) Find which taxon names are in the restricted wcvp.
  in_wcvp = which(taxon_names %in% wcvp_multiple$taxon_name)

  # 2) Names to find matches for. (list of pairs of taxon name and taxon full)
  to_find_match = Map(c, taxon_names[in_wcvp], taxon_names_full[in_wcvp])

  # 3) Find the match.
  match_info = pbapply::pblapply(to_find_match, function(x){get_match_from_multiple(x,wcvp_multiple)})
  match_info_match = as.numeric(unlist(lapply(match_info,function(x){x[[1]]})))
  match_info_mess = unlist(lapply(match_info,function(x){x[[2]]}))

  # 4) update match_to_multiple and message.
  match_to_multiple[in_wcvp] =match_info_match
  #message if we agree to a match
  has_accept_match = match_info_match > 0
  message[in_wcvp][has_accept_match] = paste0(message[in_wcvp][has_accept_match], ' -> ', match_info_mess[has_accept_match], ' -> (',
                                              wcvp_multiple$powo_id[match_info_match[has_accept_match]], ', ',
                                              wcvp_multiple$taxon_name[match_info_match[has_accept_match]],
                                              ')')
  #message if we don't agree to a match
  no_accept_match = match_info_match < 0
  message[in_wcvp][no_accept_match] = paste0(message[in_wcvp][no_accept_match], ' -> ', match_info_mess[no_accept_match])

  #Return match and message
  return(list(match = match_to_multiple, message = message))
}

#' convert_to_accepted_name()
#'
#' This function is used to update the match to POWO by checking for accepted plant names.
#'
#' @param original_match a vector of indices corresponding to a match between a gardens database and POWO.
#' @param wcvp The cleaned POWO database
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`, where the match goes to the record with accepted status.
#' `$message` a message detailing the match.
#' @export
#'
convert_to_accepted_name <- function(original_match, wcvp){

  # A) Setup: Create message. Reduce to only entries that have matches.
  message = rep('', length(original_match))
  a_match_has_been_found_index = which(!(is.na(original_match) | original_match < 0))
  a_match_has_been_found  = original_match[a_match_has_been_found_index]

  # B) Check if the plant name id equals the accepted plant name id.
  is_accepted_plant = wcvp$wcvp_names$plant_name_id[a_match_has_been_found] == wcvp$wcvp_names$accepted_plant_name_id[a_match_has_been_found]

  # B) Find those where there is not a match (= FALSE) and there is an accepted name (!= NA).
  not_match_index = which(is_accepted_plant == FALSE & !is.na(is_accepted_plant))

  # C) Extract the accepted plant_id for those that do not match
  new_plant_id = wcvp$wcvp_names$accepted_plant_name_id[a_match_has_been_found[not_match_index]]

  # D) Find the index of the corresponding accepted_plant_id
  accepted_index = match(new_plant_id, wcvp$wcvp_names$plant_name_id)

  # E) Update the a_match_has_been_found. Update original match.
  original_match[a_match_has_been_found_index[not_match_index]] = accepted_index

  # F) Update message
  message[a_match_has_been_found_index[not_match_index]] = paste0(message[a_match_has_been_found_index[not_match_index]],' -> (Go to accepted name) -> (', wcvp$wcvp_names$powo_id[accepted_index],
                                                                  ', ', wcvp$wcvp_names$taxon_name[accepted_index],
                                                                  ')')
  return(list(match = original_match, message = message))
}

#' check_taxon_typo()
#'
#' This function checks for typos in the taxon name in reports and suggests corrections if they exist.
#'
#' In particualar, the function compares the given taxon with a single extra letter or changing one letter against names known to be in POWO. If a fixed word is found in POWO we return it.
#'
#' @param taxon The taxon_name
#' @param wcvp POWO database
#'
#' @return a potential fixed name.
#' @export
check_taxon_typo <- function(taxon, wcvp){
  # 1) Since no words in wcvp have '(',')' we return null.
  if(grepl('\\(|\\)',taxon)){return(NULL)}

  # 2) reduce the wcvp names to check.
  #     A) Make sure the wcvp names are either the same length or one extra character.
  length_taxon = stringr::str_length(taxon)
  wcvp_needed = wcvp[wcvp$taxon_length %in% c(length_taxon-1, length_taxon, length_taxon+1),]
  #     B) Make sure we only look at taxons which contain only one word with a change (after removing words with '.')
  words = stringr::str_split(taxon,' ')[[1]]
  words = words[!grepl('\\.', words)]
  pat = paste0(words,collapse = '|')
  wcvp_needed = wcvp_needed[grepl(pat, wcvp_needed$taxon_name ),]
  wcvp_needed = wcvp_needed$taxon_name

  # Check each letter if different.
  similar = NULL
  for(i in 1:(length_taxon-1)){
    #Check changing a single letter.
    patternA = paste0(stringr::str_sub(taxon,1,i),'[a-zA-Z]',stringr::str_sub(taxon,i+2,length_taxon))

    #Check adding a single new letter
    patternB = paste0(stringr::str_sub(taxon,1,i),'[a-zA-Z]',stringr::str_sub(taxon,i+1,length_taxon))

    #Add the newly found similar words to 'similar'
    similar = c(similar,wcvp_needed[grepl(patternA, wcvp_needed)|grepl(patternB, wcvp_needed)])
  }
  return(similar[1])
}

#' match taxon and taxon name full to POWO
#'
#' For each unique taxon name / taxon name full find the corresponding record in POWO.
#'
#' @param taxon_taxon_full data frame of two columns where the first column is the taxon_name and second column is the full taxon name (with authors).
#' @param wcvp POWO database
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`, where the match goes to the record with accepted status.
#' `$message` a message detailing the match.
#' @export
match_taxon_to_wcvp <- function(taxon_taxon_full, wcvp){

  ###
  # 1) Setup original report. (only look at unique taxon name / taxon name full and add is_autonym)
  ###
  # Extract taxon name and taxon name full used in the matching.
  taxon_name_and_full = unique(taxon_name_and_full)
  taxon_name_and_full = add_is_autonym(taxon_name_and_full)
  taxon_name = taxon_name_and_full$TaxonName

  ###
  # 2) Setup outputs.
  ###
  taxon_match = rep(NA, length(taxon_name))
  taxon_name_story = taxon_name
  index_to_find_matches = 1:length(taxon_name)
  index_complete = NULL

  ###
  # 3) Match the exceptions of the known not to be in POWO.
  ###
  # (Assume all exceptions are single records in POWO, this is the case currently)
  exception_indices = match(wcvp$exceptions$plant_name_id, wcvp$wcvp_names$plant_name_id)
  match_info = match_single_wcvp(taxon_name[index_to_find_matches], wcvp, exception_indices)
  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)
  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]

  ###
  # 4) Remove known to not be in POWO. (set taxon match to -1)
  ###
  indices = known_not_in_wcvp(taxon_name[index_to_find_matches])
  taxon_match[index_to_find_matches[indices]] = -1
  taxon_name_story[index_to_find_matches[indices]] = paste0(taxon_name_story[index_to_find_matches[indices]], ' Not in POWO (known not to be in POWO)')
  index_complete = c(index_complete, indices)
  index_to_find_matches = index_to_find_matches[!index_to_find_matches %in% index_to_find_matches[indices]]

  ###
  # 5) Match original report to all taxon names with a single entry in POWO.
  ###
  single_indices = which(wcvp$wcvp_names$single_entry == TRUE)
  match_info = match_single_wcvp(taxon_name[index_to_find_matches], wcvp, single_indices)
  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)
  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]


  ###
  # 6) Match original report to all taxon names with a multiple entry in POWO.
  ###
  mult_indices = which(wcvp$wcvp_names$single_entry == FALSE)
  match_info = match_mult_wcvp(taxon_name[index_to_find_matches],taxon_name_and_full$TaxonNameFull[index_to_find_matches],  wcvp, mult_indices)
  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)
  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]

  ###
  # 7) Try to match the autonym.
  ###
  # A) Find the index of the remaining autonyms, extract the base name. Update taxon_name_story.
  remaining_autonyms_flag = taxon_name_and_full$is_autonym[index_to_find_matches]
  autonym_indices = index_to_find_matches[remaining_autonyms_flag]
  remaining_autonyms = taxon_name[autonym_indices]
  name_to_try = unlist(lapply(remaining_autonyms, function(name){
    split_name = stringr::str_split(name,' var\\. | subsp\\. | f\\. | ssp\\. | nothosubsp\\. ')[[1]]
    split_name = unlist(lapply(split_name, stringr::str_squish))
    return(split_name[1])
  }))
  taxon_name_story[autonym_indices] = paste0(taxon_name_story[autonym_indices],
                                             ' -> (Autonym. Does not exist in POWO. Convert to base name) -> ',
                                             name_to_try)

  # B) Try to match the base name to POWO with single records.
  match_info = match_single_wcvp(name_to_try, wcvp, single_indices)
  taxon_match[autonym_indices] = match_info$match
  taxon_name_story[autonym_indices] = paste0(taxon_name_story[autonym_indices], match_info$message)
  index_complete = c(index_complete, autonym_indices[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[!index_to_find_matches %in% autonym_indices[!is.na(match_info$match)]]
  name_to_try = name_to_try[is.na(match_info$match)]
  autonym_indices = autonym_indices[is.na(match_info$match)]

  # C) Try to match the base name to POWO with multiple records.
  match_info = match_mult_wcvp(name_to_try,taxon_name_and_full$TaxonNameFull[autonym_indices],  wcvp, mult_indices)
  taxon_match[autonym_indices] = match_info$match
  taxon_name_story[autonym_indices] = paste0(taxon_name_story[autonym_indices], match_info$message)
  index_complete = c(index_complete, autonym_indices[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[!index_to_find_matches %in% autonym_indices[!is.na(match_info$match)]]

  ###
  # 8) Try to find typo and then match.
  ###
  # A) Search for typos.
  fixed_typo = unlist(pbapply::pblapply(taxon_name[index_to_find_matches], function(x){check_taxon_typo(x,wcvp$wcvp_names)}))
  typo_index = index_to_find_matches[which(!is.na(fixed_typo))]
  name_to_try = fixed_typo[which(!is.na(fixed_typo))]
  taxon_name_story[typo_index] = paste0(taxon_name_story[typo_index],
                                        ' -> (Typo) -> ',
                                        name_to_try)

  # B) Try to match the base name to POWO with single records.
  match_info = match_single_wcvp(name_to_try, wcvp, single_indices)
  taxon_match[typo_index] = match_info$match
  taxon_name_story[typo_index] = paste0(taxon_name_story[typo_index], match_info$message)
  index_complete = c(index_complete, typo_index[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[!index_to_find_matches %in% typo_index[!is.na(match_info$match)]]
  name_to_try = name_to_try[is.na(match_info$match)]
  typo_index = typo_index[is.na(match_info$match)]

  # C) Try to match the base name to POWO with multiple records.
  match_info = match_mult_wcvp(name_to_try,taxon_name_and_full$TaxonNameFull[typo_index],  wcvp, mult_indices)
  taxon_match[typo_index] = match_info$match
  taxon_name_story[typo_index] = paste0(taxon_name_story[typo_index], match_info$message)
  index_complete = c(index_complete, typo_index[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[!index_to_find_matches %in% typo_index[!is.na(match_info$match)]]

  ###
  # 9) Convert to accepted name where possible.
  ###
  match_info = convert_to_accepted_name(taxon_match, wcvp)
  taxon_match = match_info$match
  taxon_name_story = paste0(taxon_name_story, match_info$message)

  ###
  # 10) return match (to original report) and details of the matches (for unique plants).
  ###
  # A) The remaining not found indices set to -3. With message not in POWO.
  taxon_match[index_to_find_matches] = -3
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], ' -> (Not in POWO)')

  # B) Set output
  output = data.frame(TaxonName = taxon_name_and_full$TaxonName,
                      TaxonNameFull = taxon_name_and_full$TaxonNameFull,
                      POWO_match = taxon_match,
                      details = taxon_name_story)
  return(output)
}


