# Functions used to match to POWO (wcvp_names)

#' shorten_message
#'
#' @param messages messages about matching
#'
#' @return shorted form of messages
#' @export
#'
shorten_message <- function(messages){
  match_short = rep('', length(messages))
  match_options = c("(matches POWO record with single entry)", 'EXACT',
                    "(Exact author match)", 'EXACT',
                    "all point to same accepted plant", 'EXACT',
                    "Partial author", 'PARTIAL',
                    "choose via taxon_status", "TAXON_STATUS",
                    "multiple best taxon status, do not match", "UNCLEAR",
                    "no accepted or synonym", "UNCLEAR",
                    "(Not in POWO <known not to be in POWO>)", 'NOT_IN',
                    "Remove autonym", 'AUTONYM',
                    "(Typo)", 'TYPO',
                    "(Not in POWO)", 'NO_MATCH',
                    "(Go to accepted name)", 'ACCEPTED',
                    "(Sanitise)", 'SANITISE',
                    "Infrageneric level update", 'INFRA',
                    "Hybrid fix", 'HYBRID'
  )
  match_options = stringr::str_replace_all(match_options, '\\(', '\\\\(')
  match_options = stringr::str_replace_all(match_options, '\\)', '\\\\)')
  match_options = matrix(match_options, byrow = T, ncol=2)
  for(i in 1:nrow(match_options)){
    indices = grepl(match_options[i,1], messages)
    match_short[indices] = paste0(match_short[indices],', ', match_options[i,2])
  }
  match_short = stringr::str_remove(match_short, '^, ')

  #If we have multiple of EXACT, PARTIAL, TAXON_STATUS only keep the worst level of matching.
  match_short = stringr::str_replace_all(match_short,'EXACT, PARTIAL','PARTIAL')
  match_short = stringr::str_replace_all(match_short,'PARTIAL, TAXON_STATUS','TAXON_STATUS')
  match_short = stringr::str_replace_all(match_short,'EXACT, UNCLEAR','UNCLEAR')

  return(match_short)
}


#' known_not_in_wcvp()
#'
#' A function that finds the taxon names which are known not to be in POWO. In particular this includes:
#' - Includes ` sp.`.
#' - Includes ` gx `.
#' - Includes ` gx`.
#' - Includes `'XX'` for some text XX. This is common notation for XX.
#' - Includes `[`.
#' - Begins with `Indet`. This is an indetminant and therefore won't be in POWO.
#' - Ends in `indet`. This is an indetminant and therefore won't be in POWO.
#' - Includes `CV`.
#' - Ends in `cv`.
#' - Includes `cv.`.
#' - Includes `Group`.
#' - Includes `unkn`.
#' - Ends in `hybrid`.
#' - Includes `Hybrid `.
#' - Includes `Unknown`.
#'
#' Note that some of these known not in POWO do have exceptions and this is captured when reading in the data via `input_wcvp_names()`.
#'
#' @param taxon_names A vector of taxon names
#'
#' @return A vector of the indices in taxon_names that can be removed.
#' @export
known_not_in_wcvp <- function(taxon_names){
  #Setup output
  out_match = rep(NA,length(taxon_names))
  out_message = rep('',length(taxon_names))

  #Find indices of those known not to be in wcvp.
  not_in_wcvp = which(grepl(" sp\\.| gx |'.*?'|\\[|^Indet| gx|indet$|CV|cv$|cv\\.|Group|unkn|hybrid$|Hybrid |Unknown",taxon_names))

  out_match[not_in_wcvp] = -1
  out_message[not_in_wcvp] = ' -> (Not in POWO <known not to be in POWO>)'

  return(list(match = out_match, message = out_message))
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

#' match_taxon_status
#'
#' @param author_match vector of true of false whether we have a match
#' @param corres_POWO  the corresponding entries in POWO (rows must equal length author match)
#' @param current_message the current message
#'
#' @return list of length 3
#' @export
#'
match_taxon_status <- function(author_match, corres_POWO, current_message = ''){
  author_match[is.na(author_match)] = FALSE # NA matches go to FALSE. This occurs when POWO author = NA.

  match_flag = FALSE
  if(sum(author_match) == 0){
    matched = NA
    message = ''
    match_flag = FALSE
  }
  # Exactly one match return it.
  else if(sum(author_match) == 1){
    matched = corres_POWO$plant_name_id[author_match]
    message = '(single match)'
    match_flag = TRUE
  }
  else if(sum(author_match) > 1){ # > 1 match
    message = ' > 1 match, '
    corres_POWO = corres_POWO[author_match,]
    accepted_plant_id = corres_POWO$accepted_plant_name_id

    # Check if all exact matches point to the same accepted name
    if(identical(accepted_plant_id, rep(accepted_plant_id[1], length(accepted_plant_id)))){
      # match to accepted if one exists if not the first plant that matches.
      taxon_accepted = corres_POWO$taxon_status == 'Accepted'
      if(any(taxon_accepted)){
        matched = corres_POWO$plant_name_id[taxon_accepted][1]
        message = paste0('(', message, 'all point to same accepted plant',')',collapse = '')
      }
      else{
        matched = corres_POWO$plant_name_id[1]
        message = paste0('(', message, 'all point to same accepted plant',')',collapse = '')

      }
      match_flag = TRUE
    }

    # Check for differences in taxon_status.
    if(!match_flag){
      taxon_status = corres_POWO$taxon_status
      taxon_status_match = match(taxon_status , c('Accepted', 'Synonym'))
      if(all(is.na(taxon_status_match))){
        matched = -2
        message = paste0('(', message, 'no accepted or synonym',')',collapse = '')
        match_flag = TRUE
      }
      else{
        chosen_record = which(taxon_status_match == min(taxon_status_match, na.rm = T))
        if(length(chosen_record) == 1){
          matched = corres_POWO$plant_name_id[chosen_record]
          message = paste0('(', message, 'choose via taxon_status',')',collapse = '')
          match_flag = TRUE
        }
        else if(length(chosen_record) >1){
          matched = -2
          message = paste0('(', message, 'multiple best taxon status, do not match',')',collapse = '')
          match_flag = TRUE
        }
      }

    }
  }
  else{
    matched = NA
    message = ''
    match_flag = FALSE
  }

  message = paste0(current_message,message, collapse =' ')
  return(list(plant_name_id = matched, message = message, match_flag = match_flag))
}

#' get_match_from_multiple()
#'
#' Given a taxon name and its full version we find the records in POWO that share the same taxon name. We then choose the record to match to by:
#'
#'
#' @param taxon_name_and_author the pair of taxon name and taxon name full.
#' @param wcvp_mult POWO database restricted to records that do not have a unique taxon name.
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`
#' `$message` a message detailing the match.
#' @export
get_match_from_multiple <- function(taxon_name_and_author, wcvp_mult){
  # 1) Split taxon name and taxon full
  taxon_name_current = taxon_name_and_author[1]
  taxon_author_current = taxon_name_and_author[2]


  try_author_match = TRUE # flag for whether we have author information
  flag = TRUE # flag for whether we need to do checks.
  #If the author is NA
  if(is.na(taxon_author_current)){
    try_author_match = FALSE
  }
  else{
    if(taxon_author_current == ''){
      try_author_match = FALSE

    }
  }



  # 2) Get the corresponding records in wcvp_mult.
  POWO_cur = wcvp_mult[wcvp_mult$taxon_name == taxon_name_current,]

  ###
  # 3) Get the match by author (if one exists)
  ###
  if(try_author_match){
    # A) By author (exact).
    exact_match = taxon_author_current == POWO_cur$taxon_authors_simp
    match_cur = match_taxon_status(exact_match, POWO_cur, current_message = '(Exact author match) -> ')

    # B) By partial author.
    if(!match_cur$match_flag){

      # Get the words for the original and POWO authors.
      POWO_author_words = POWO_cur$author_parts
      original_author_words = author_words(taxon_author_current)

      #Find number of words in POWO authors found in the original author
      no_powo_author_in_original = unlist(lapply(POWO_author_words,function(x){
        words = stringr::str_split(x,', ')[[1]]
        words = words[words != '']
        contain_words = unlist(lapply(words, function(x){grepl(x,taxon_author_current)}))
        return(sum(contain_words))
      }))

      #Find number of words in original author found in the POWO authors
      no_original_author_in_powo = rowSums(data.frame(lapply(original_author_words, function(x){grepl(x,POWO_cur$taxon_authors_simp)})))

      # Combine above and find the maximum shared words.
      total_match_word_count = rowSums(cbind(no_powo_author_in_original,no_original_author_in_powo))
      max_words_found =  max(total_match_word_count, na.rm=T)

      # If maximum shared words > 0 then try and find a match with those with the maximum number of shared words.
      if(max_words_found > 0){
        match_author_words = total_match_word_count == max_words_found
        match_cur = match_taxon_status(match_author_words, POWO_cur,  current_message = '(Partial author) -> ')
      }
    }
  }
  ###
  # 4) Try to match when we do not have the author or the author doesn't match any in POWO.
  ###
  if(!try_author_match){
    match_cur = match_taxon_status(rep(TRUE, nrow(POWO_cur)), POWO_cur, current_message = '(No authors) -> ')
  }
  else{
    if(!match_cur$match_flag){
      match_cur = match_taxon_status(rep(TRUE, nrow(POWO_cur)), POWO_cur, current_message = '(No authors match) -> ')

    }
  }

  if(match_cur$match_flag == TRUE){
    match_cur$message = paste0('(Multiple records in POWO) ->', match_cur$message, collapse = ' ')
  }
  return(list(plant_name_id = match_cur$plant_name_id, message = match_cur$message))
}

#' match_mult_wcvp()
#'
#' Given the taxon names and taxon authors you want to match (`taxon_names`, `taxon_authors`), the cleaned version of plants of the world online ( `wcvp` imported through the function `import_wcvp_names()`) and the indices of `wcvp`  records that you want to math to (`wcvp_search_index`). As this function is designed to match to records that share taxon names the user should restrict the search indices to those with non-unique taxon names or a subset of those names.
#'
#' The matching of individual taxon names is completed by `get_match_from_multiple()`.
#'
#' The matched index corresponds to the full `wcvp` data set. If a match is found but we cannot ascertain which record to match to in POWO we set the match index to `-2`. If the taxon name is not found in POWO then the function returns `NA`.
#'
#' Moreover, the function creates a message for each taxon name detailing when a match is found and the powo identifier and taxon name of the match in `wcvp`.
#'
#' @param taxon_names  a vector of taxon names
#' @param taxon_authors a vector of full taxon names (corresponding to `taxon_names`)
#' @param wcvp cleaned POWO database
#' @param wcvp_search_index Indices of `wcvp` that we want to search for a match
#' @param show_progress Flag for whether we show progress bar.
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`
#' `$message` a message detailing the match.
#' @export
match_mult_wcvp <- function(taxon_names,taxon_authors, wcvp, wcvp_search_index, show_progress = TRUE){
  # A) Setup.
  match_to_multiple = rep(NA,length(taxon_names))
  message = rep('',length(taxon_names))
  wcvp_multiple = wcvp$wcvp_names[wcvp_search_index,]


  # 1) Find which taxon names are in the restricted wcvp.
  in_wcvp = which(taxon_names %in% wcvp_multiple$taxon_name)

  # 2) Names to find matches for. (list of pairs of taxon name and taxon full)
  to_find_match = Map(c, taxon_names[in_wcvp], taxon_authors[in_wcvp])

  # 3) Find the match.
  if(show_progress){
    match_info = pbapply::pblapply(to_find_match, function(x){
      get_match_from_multiple(x,wcvp_multiple)})
  }
  else{
    match_info = lapply(to_find_match, function(x){
      get_match_from_multiple(x,wcvp_multiple)})
  }

  match_info_plant_name_id = as.numeric(unlist(lapply(match_info,function(x){x[[1]]})))
  match_info_mess = unlist(lapply(match_info,function(x){x[[2]]}))

  # 4) update match_to_multiple and message.
  match_info_match =  match(match_info_plant_name_id, wcvp$wcvp_names$plant_name_id)
  match_info_match[is.na(match_info_match)] = -2
  match_to_multiple[in_wcvp] = match_info_match

  #message if we agree to a match
  has_accept_match = match_info_match > 0
  message[in_wcvp][has_accept_match] = paste0(message[in_wcvp][has_accept_match], ' -> ', match_info_mess[has_accept_match], ' -> (',
                                              wcvp$wcvp_names$powo_id[match_info_match[has_accept_match]], ', ',
                                              wcvp$wcvp_names$taxon_name[match_info_match[has_accept_match]],
                                              ')')
  #message if we don't agree to a match
  no_accept_match = match_info_match < 0
  message[in_wcvp][no_accept_match] = paste0(message[in_wcvp][no_accept_match], ' -> ', match_info_mess[no_accept_match])

  #Return match and message
  return(list(match = match_to_multiple, message = message))
}

#' match_rm_autonym()
#'
#' @param taxon_names taxon names
#' @param taxon_authors taxon names with author
#' @param wcvp enrich information
#' @param single_indices wcvp indices with a unique taxon name
#' @param mult_indices wcvp indices with non-unique taxon name
#'
#' @return a list with match and message
#' @export
match_rm_autonym <- function(taxon_names, taxon_authors, wcvp,
                             single_indices = NA,
                             mult_indices = NA){

  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In match_autonym(), taxon names contain NA.')
  }
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }
  ########################
  # Setup + find autonym taxon names + create wcvp indices + create names to try
  ########################
  out_match = rep(NA, length(taxon_names))
  out_message = rep('',length(taxon_names))

  taxon_names_and_autonym = add_is_autonym(data.frame(TaxonName = taxon_names))
  autonym_indices =  which(taxon_names_and_autonym$is_autonym)

  if(length(autonym_indices) == 0){
    return(list(match = out_match, message = out_message))
  }

  if(is.na(single_indices[1])){
    single_indices = which(wcvp$wcvp_names$single_entry == TRUE)
  }
  if(is.na(mult_indices[1])){
    mult_indices = which(wcvp$wcvp_names$single_entry == FALSE)
  }

  name_to_try = unlist(lapply(taxon_names[autonym_indices], function(name){
    split_name = stringr::str_split(name,' var\\. | subsp\\. | f\\. | ssp\\. | nothosubsp\\. ')[[1]]
    split_name = unlist(lapply(split_name, stringr::str_squish))
    return(split_name[1])
  }))

  ########################
  # Match to single.
  ########################
  match_info = match_single_wcvp(name_to_try, wcvp, single_indices)
  out_match[autonym_indices] = match_info$match
  out_message[autonym_indices] = paste0(out_message[autonym_indices], match_info$message)
  index_complete = autonym_indices[!is.na(match_info$match)]
  index_to_find_matches = which(is.na(match_info$match))

  ########################
  # Match to multiple.
  ########################
  if(length(index_to_find_matches) > 0){
    auto_index = autonym_indices[index_to_find_matches]
    match_info = match_mult_wcvp(name_to_try[index_to_find_matches], taxon_authors[auto_index],  wcvp, mult_indices)
    out_match[auto_index] = match_info$match
    out_message[auto_index] = paste0(out_message[auto_index], match_info$message)
  }

  ########################
  # Update match message to include the removed autonym.
  ########################
  with_match = out_message[autonym_indices] != ''
  out_message[autonym_indices[with_match]] = paste0(" -> (Remove autonym) -> ", name_to_try[with_match],  out_message[autonym_indices[with_match]])


  return(list(match = out_match, message = out_message))
}

#' match_error()
#'
#' @param new_taxon_names  new_taxon_names
#' @param match_details match_details
#' @param original_author original_author
#' @param wcvp wcvp
#' @param current_message current_message
#'
#' @return list of message and match
#' @export
#'
match_error <- function(new_taxon_names, match_details, original_author, wcvp, current_message =''){
  # A) Index of the matches we found.
  found_match_index = which(!is.na(match_details$match))
  if(length(found_match_index) == 0){
    return(c(NA,''))
  }

  match_detail = list(match = match_details$match[found_match_index],
                      message = match_details$message[found_match_index],
                      taxon_name = rep(new_taxon_names,2)[found_match_index])


  # If we get a single match return it.
  if(length(match_detail$match)==1){
    match = match_detail$match
    message = paste0(current_message, match_detail$taxon_name, match_detail$message)
    return(c(match,message))
  }

  # Multiple matches currently don't know which is best.
  if(length(found_match_index)>1){
    # Does a match have better authors?
    match_authors = wcvp$wcvp_names$taxon_authors_simp[match_detail$match]
    author_compare = unlist(lapply(match_authors,function(x){author_check(original_author,x)}))
    best_match = match(author_compare,c('Exact','Partial', 'Different'))
    best_matches = which(best_match == min(best_match))

    # update match_detail to the best authors.
    match_detail = list(match = match_detail$match[best_matches],
                        message = match_detail$message[best_matches],
                        taxon_name =  match_detail$taxon_name[best_matches])

    # If we get a single match return it.
    if(length(match_detail$match)==1){
      match_detail$message = paste0('(Multiple attempted fixed taxon names match) -> (Choose record by author matching) -> ')
      match = match_detail$match
      message = paste0(current_message, match_detail$taxon_name, match_detail$message)
      return(c(match,message))
    }


    # Try and use taxon status or all point to same accepted plant.
    taxon_match = match_taxon_status(author_match = rep(T,length(match_detail$match)),
                                     corres_POWO = wcvp$wcvp_names[match_detail$match,],
                                     current_message = '(Multiple attempted fixed taxon names match) -> (Cannot choose record by author match) -> ')
    #Note that the match provided by match_taxon_status is the plant_name_id and not the row number in wcvp. So we need to convert.
    if(!is.na(taxon_match$match)){
      match_index = match(taxon_match$match, wcvp$wcvp_names$plant_name_id)
    }
    match = match_index
    message = paste0(current_message, taxon_match$message)

    return(c(match,message))
  }

}


#' add_splitter()
#'
#' @param taxon_names taxon names
#' @param taxon_authors taxon names with author
#' @param wcvp enrich information
#'
#' @return a list with match and message
#' @export
#'
add_splitter <- function(taxon_names, taxon_authors, wcvp){
  # We know from exploring POWO that var/f/subsp only occurs after the genus species. (with the potential addition of 'x' or '+' for hybrids)
  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In add_splitter(), taxon names contain NA.')
  }
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }
  ########################
  # Setup + find words of length 3 and 4 + words with a splitter.
  ########################
  splitters = c('subsp.', 'var.', 'f.', 'nothosubsp.')
  splitters_grepl = ' subsp\\. | var\\. | f\\. | nothosubsp\\. '
  out_match = rep(NA, length(taxon_names))
  out_message = rep('',length(taxon_names))
  no_words = stringr::str_count(taxon_names, ' ')+1

  #Get the index of words with length 3 without a hybrid sign.
  index_words_3 = which(no_words == 3 & !grepl('\u00D7|\\+',taxon_names))
  #Get the index of words of length 4 with a hybrid sign only before or after the first word.
  index_words_4 = which(no_words == 4 & grepl('\u00D7|\\+',taxon_names))
  hybrid_position = unlist(lapply(stringr::str_split(taxon_names[index_words_4], ' '),function(x){which(grepl('\u00D7|\\+',x))}))
  index_words_4 = index_words_4[hybrid_position %in% c(1,2)]

  index_words_with_splitter = which(grepl(splitters_grepl, taxon_names))
  wcvp_index_splitters = which(grepl(splitters_grepl, wcvp$wcvp_names$taxon_name))
  wcvp_index_splitters_mult = wcvp_index_splitters[wcvp$wcvp_names$single_entry[wcvp_index_splitters] == F]
  wcvp_index_splitters_single = wcvp_index_splitters[wcvp$wcvp_names$single_entry[wcvp_index_splitters] == T]

  ########################
  # Try matching splitter for words of length three.
  ########################
  if(length(index_words_3) > 0){
    cli::cli_alert_info("Trying add splitter (taxon names has length 3) for {length(index_words_3)} name{?s}")

    # Get the indices of wcvp we want to search (i.e must contain splitter)

    words_3 = stringr::str_split(taxon_names[index_words_3], ' ')
    to_try_words = lapply(words_3, function(x){
      just_splitter = paste(x[1], x[2], splitters, x[3])
      hybrid_and_splitter =  paste(x[1], '\u00D7', x[2], splitters, x[3])
      return(c(just_splitter,hybrid_and_splitter))
    })
    taxon_full_3 = as.list(taxon_authors[index_words_3])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_3, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_splitters_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],8), wcvp, wcvp_index_splitters_mult, show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(new_taxon_names = x[[1]],
                  match_details = match_details,
                  original_author = x[[2]],
                  wcvp = wcvp,
                  current_message = ' -> (Infrageneric level update) -> ')
     return(chosen_record)
    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_3] = as.numeric(match_info[,1])
    out_message[index_words_3] = match_info[,2]


  }

  ########################
  # Try matching splitter for words of length four. (with x or +)
  ########################
  if(length(index_words_4) > 0){
    cli::cli_alert_info("Trying add splitter (with hybrid taxon names) for {length(index_words_4)} name{?s}")

    #get wcvp indices with only + or x.
    wcvp_index_splitters_mult_h = wcvp_index_splitters_mult[grepl('\u00D7|\\+',wcvp$wcvp_names$taxon_name[wcvp_index_splitters_mult])]
    wcvp_index_splitters_single_h = wcvp_index_splitters_single[grepl('\u00D7|\\+',wcvp$wcvp_names$taxon_name[wcvp_index_splitters_single])]

    # Make sure the hybrid only occurs before or after the first word

    words_4 = stringr::str_split(taxon_names[index_words_4], ' ')
    to_try_words = lapply(words_4, function(x){
      just_splitter = paste(x[1], x[2], x[3], splitters, x[4])
      return(just_splitter)
    })
    taxon_full_4 = as.list(taxon_authors[index_words_4])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_4, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_splitters_single_h)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],4), wcvp, wcvp_index_splitters_mult_h, show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(new_taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  wcvp = wcvp,
                                  current_message = ' -> (Infrageneric level update) -> ')
      return(chosen_record)

    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_4] = as.numeric(match_info[,1])
    out_message[index_words_4] = match_info[,2]

  }

  ########################
  # Try matching by changing the splitter.
  ########################
  if(length(index_words_with_splitter)> 0){
    cli::cli_alert_info("Trying change splitter for {length(index_words_with_splitter)} name{?s}")

    # Get the words with the splitter changed.
    to_try_words = lapply(taxon_names[index_words_with_splitter], function(x){
      A=stringr::str_replace(x,pattern = splitters_grepl, ' var\\. ')
      B=stringr::str_replace(x,pattern = splitters_grepl, ' f\\. ')
      C=stringr::str_replace(x,pattern = splitters_grepl, ' subsp\\. ')
      D=stringr::str_replace(x,pattern = splitters_grepl, ' nothosubsp\\. ')
      options = c(A,B,C,D)
      options = options[-match(x, options)]
      return(options)
    })
    #Get the corresponding taxon_authors (don't change here as we only check for author.)
    taxon_full_splitter = as.list(taxon_authors[index_words_with_splitter])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_splitter, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_splitters_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],3), wcvp, wcvp_index_splitters_mult, show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(new_taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  wcvp = wcvp,
                                  current_message = ' -> (Infrageneric level update) -> ')
      return(chosen_record)

    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_with_splitter] = as.numeric(match_info[,1])
    out_message[index_words_with_splitter] = match_info[,2]

  }


  return(list(match = out_match, message = out_message))
}

#' match_hybrid_issue
#'
#' @param taxon_names taxon names
#' @param taxon_authors taxon names with author
#' @param wcvp enrich information
#'
#' @return a list with match and message
#' @export
match_hybrid_issue <- function(taxon_names, taxon_authors, wcvp){
  # We know from exploring POWO that var/f/subsp only occurs after the genus species. (with the potential addition of 'x' or '+' for hybrids)

  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In match_hybrid_issue(), taxon names contain NA.')
  }
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }
  ########################
  # Setup + find words of length 3 and 4 + words with a splitter.
  ########################
  out_match = rep(NA, length(taxon_names))
  out_message = rep('',length(taxon_names))
  no_words = stringr::str_count(taxon_names, ' ')+1
  hybrids = c('\u00D7', '+')
  # Indices of the different cases.
  # Get words that have a single hybrid (x,+) in them (this means only one x or one +)
  index_words_with_hybrid = which(grepl('\u00D7|\\+',taxon_names))
  words_with_hybrid = taxon_names[which(grepl('\u00D7|\\+',taxon_names))]
  no_hybrids = stringr::str_count(words_with_hybrid, pattern = '\u00D7|\\+')
  index_words_with_hybrid = index_words_with_hybrid[no_hybrids == 1]
  # Indices of single words
  index_words_1 = which(no_words == 1)
  #Indices of taxo names with 2/3/4 words without a hybrid.
  index_words_2_3_4 = which(no_words %in% c(2,3,4), !grepl('\u00D7|\\+',taxon_names))

  # Get the indices of wcvp with and without hybrids for single entry and multiple entry.
  wcvp_index_hybrid = which(grepl('\u00D7|\\+', wcvp$wcvp_names$taxon_name))
  wcvp_index_hybrid_mult = wcvp_index_hybrid[wcvp$wcvp_names$single_entry[wcvp_index_hybrid] == F]
  wcvp_index_hybrid_single = wcvp_index_hybrid[wcvp$wcvp_names$single_entry[wcvp_index_hybrid] == T]

  wcvp_index_mult = which(wcvp$wcvp_names$single_entry == F)
  wcvp_index_single = which(wcvp$wcvp_names$single_entry == T)

  ########################
  # Case 1: single word try hybrid at start.
  ########################
  if(length(index_words_1) > 0){
    cli::cli_alert_info("Trying hybrid fix for single words {length(index_words_1)} name{?s}")

    # Get the indices of wcvp we want to search (i.e must contain splitter)

    words_1 = taxon_names[index_words_1]
    to_try_words = lapply(words_1, function(x){
      return(c(paste('+',x,collapse =' '), paste('\u00D7', x, collapse = ' ')))
    })
    taxon_full_1 = as.list(taxon_authors[index_words_1])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_1, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_hybrid_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],2), wcvp, wcvp_index_hybrid_mult, show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

       chosen_record = match_error(new_taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  wcvp = wcvp,
                                  current_message = ' -> (Hybrid fix) -> ')
      return(chosen_record)
    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_1] = as.numeric(match_info[,1])
    out_message[index_words_1] = match_info[,2]


  }

  ########################
  # Case 2: 2/3/4 words try hybrid at start or after first word.
  ########################
  if(length(index_words_2_3_4) > 0){
    cli::cli_alert_info("Trying fixing hybrid for taxon names with 2/3/4 words {length(index_words_2_3_4)} name{?s}")

    # Get the indices of wcvp we want to search (i.e must contain splitter)
    words_2_3_4 = stringr::str_split(taxon_names[index_words_2_3_4], ' ', n=2)
    to_try_words = lapply(words_2_3_4, function(x){
      before_first_word = paste(hybrids, x[1],x[2])
      after_first_word = paste(x[1], hybrids, x[2])
      return(c(before_first_word,after_first_word))
    })
    taxon_full_2_3_4 = as.list(taxon_authors[index_words_2_3_4])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_2_3_4, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_hybrid_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],4), wcvp, wcvp_index_hybrid_mult, show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

       chosen_record = match_error(new_taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  wcvp = wcvp,
                                  current_message = ' -> (Hybrid fix) -> ')
      return(chosen_record)
    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_2_3_4] = as.numeric(match_info[,1])
    out_message[index_words_2_3_4] = match_info[,2]


  }

  ########################
  # Case 3: Change/remove hybrid
  ########################
  if(length(index_words_with_hybrid) > 0){
    cli::cli_alert_info("Trying changing/removing hybrid for taxon names {length(index_words_with_hybrid)} name{?s}")

    # Get the words with the hybrid changed or removed.
    to_try_words = lapply(taxon_names[index_words_with_hybrid], function(x){
      A=stringr::str_replace(x,pattern = '\u00D7|\\+', '')
      B=stringr::str_replace(x,pattern = '\u00D7|\\+', '\\+')
      C=stringr::str_replace(x,pattern = '\u00D7|\\+', '\u00D7')
      options = c(A,B,C)
      options = stringr::str_squish(options)
      options = options[-match(x, options)]
      return(options)
    })

    taxon_full_with_hybrid = as.list(taxon_authors[index_words_with_hybrid])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_with_hybrid, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp , wcvp_index_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],3), wcvp, wcvp_index_mult, show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

        chosen_record = match_error(new_taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  wcvp = wcvp,
                                  current_message = ' -> (Hybrid fix) -> ')
      return(chosen_record)
    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_with_hybrid] = as.numeric(match_info[,1])
    out_message[index_words_with_hybrid] = match_info[,2]


  }


  return(list(match = out_match, message = out_message))
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
#' This function checks for typos in the taxon name and returns (the first) record found in wcvp with a single change.
#'
#' In particular, the function compares the given taxon with a single extra letter, changing one letter or removing one letter against names known to be in POWO.
#'
#' @param taxon The taxon_name
#' @param wcvp POWO database
#' @param typo_df A data frame with two columns of typos and their fixes.
#' @param fast A flag for whether we want to search only the typo_df (TRUE) or also search directly (FALSE)
#'
#' @return a potential fixed name, or NA if no correction is found.
#' @export
check_taxon_typo <- function(taxon, wcvp = NA, typo_df = BGSmartR::typo_list, fast = T){
  ########################
  # 1) Return NA for non-words and special characters
  ########################
  if(is.null(taxon))(return(NA))
  if(is.na(taxon)){return(NA)}
  if(taxon ==''){return(NA)}
  if(grepl('\\(|\\)|\\*|\\?|\\$|\\^',taxon)){return(NA)} # Since no words in wcvp have '(',')', '?' or '*', '$', '^' we return NA.

  ########################
  # 2) Check if the typo is in typo list.
  ########################
  match_to_typo = match(taxon,typo_df[,1])
  if(!is.na(match_to_typo)){
    return(typo_df[match_to_typo,2])
  }
  #If we only want to check the typo list return NA for non-matches at this stage.
  if(fast){
    return(NA)
  }

  ########################
  # 3) reduce the wcvp names to check. And split into three vectors for same length, one less and one more.
  ########################
  #     A) Make sure the wcvp names are either the same length or one extra character.
  length_taxon = stringr::str_length(taxon)
  wcvp_needed = wcvp[wcvp$taxon_length %in% c(length_taxon-1, length_taxon, length_taxon+1),]
  #     B) Make sure we only look at taxons which contain only one word with a change (after removing words with '.' or '+')
  words = stringr::str_split(taxon,' ')[[1]]
  words = words[!grepl('\\.|\\+|\u00D7', words)]
  pat = paste0(words,collapse = '|')

  wcvp_needed = wcvp_needed[grepl(pat, wcvp_needed$taxon_name ),]
  wcvp_needed_minus_1 = wcvp_needed$taxon_name[wcvp_needed$taxon_length == (length_taxon-1)]
  wcvp_needed_same = wcvp_needed$taxon_name[wcvp_needed$taxon_length == (length_taxon)]
  wcvp_needed_plus_1 = wcvp_needed$taxon_name[wcvp_needed$taxon_length == (length_taxon+1)]

  ########################
  # 4) Find common typos in taxon names
  ########################
  # a) final letter change.
  final_letter_change = matrix(c('i','ii',
                      'i', 'ae',
                      'a', 'um',
                      'a', 'us',
                      'a', 'is',
                      'a', 'os',
                      'a', 'er',
                      'er', 'erus',
                      'ae', 'eae',
                      'e', 'is',
                      'ii', 'us',
                      'is','e',
                      'us', 'is',
                      'ense','iense',
                      'oides', 'ioides',
                      'orum', 'iorum'),byrow = T, ncol = 2)
  final_letter_change = rbind(final_letter_change, final_letter_change[,2:1])
  # Function that loops over all final letter changes and returns if typo is found.
  for(i in 1:nrow(final_letter_change)){
    if(stringr::str_ends(taxon,final_letter_change[i,1])){
      fixed = wcvp_needed$taxon_name[grepl(stringr::str_replace(taxon,paste0(final_letter_change[i,1],'$',collapse = ''),final_letter_change[i,2]), wcvp_needed$taxon_name)]
      if(length(fixed) >0){
        return(fixed)
      }
    }
  }

  # b) Common none simple letter swap.
  letter_change = matrix(c('i','ae'),byrow = T, ncol = 2)
  for(i in 1:nrow(letter_change)){
    locations = stringr::str_locate_all(taxon, letter_change)
    for(j in 1:length(locations)){
      current = locations[[j]]
      if(nrow(current) > 0){
        for(k in 1:nrow(current)){
          # original letter in middle.
          if(current[k,1] == 1){
            new_name = paste0(letter_change[i,3-j],
                              stringr::str_sub(taxon,current[k,2]+1,-1))
          }
          #original letter at end.
          else if(current[k,2] == length(taxon)){
            new_name = paste0(stringr::str_sub(taxon,1,current[k,1]-1),
                              letter_change[i,3-j])
          }
          #original letter in middle
          else{
            new_name = paste0(stringr::str_sub(taxon,1,current[k,1]-1),
                              letter_change[i,3-j],
                              stringr::str_sub(taxon,current[k,2]+1,-1))
          }

          fixed = wcvp_needed$taxon_name[grepl(new_name, wcvp_needed$taxon_name)]
          if(length(fixed) >0){
            return(fixed)
          }
        }
      }
    }
  }

  ########################
  # 4) More general search of one letter differences.
  ########################
  for(i in (length_taxon-1):1){
    #Check changing a single letter.
    patternA = paste0(stringr::str_sub(taxon,1,i),'[a-zA-Z]',stringr::str_sub(taxon,i+2,length_taxon))
    fixed_typoA = wcvp_needed_same[grepl(patternA, wcvp_needed_same)]
    if(length(fixed_typoA) >0){
      return(fixed_typoA[1])
    }

    #Check adding a single new letter
    patternB = paste0(stringr::str_sub(taxon,1,i),'[a-zA-Z-]',stringr::str_sub(taxon,i+1,length_taxon))
    fixed_typoB = wcvp_needed_plus_1[grepl(patternB, wcvp_needed_plus_1)]
    if(length(fixed_typoB) >0){
      return(fixed_typoB[1])
    }
    #Check removing a single new letter
    patternC = paste0(stringr::str_sub(taxon,1,i),stringr::str_sub(taxon,i+2,length_taxon))
    fixed_typoC = wcvp_needed_minus_1[grepl(patternC, wcvp_needed_minus_1)]
    if(length(fixed_typoC) >0){
      return(fixed_typoC[1])
    }
  }

  # If no typo is found by this stage we haven't managed to find a fix and return NA.
  return(NA)
}

#' match_typos()
#'
#' @param taxon_names taxon names
#' @param taxon_authors taxon names with author
#' @param wcvp enrich information
#' @param single_indices wcvp indices with a unique taxon name
#' @param mult_indices wcvp indices with non-unique taxon name
#' @param typo_method method for finding typos
#'
#' @return a list with match and message
#' @export
match_typos <- function(taxon_names, taxon_authors, wcvp,
                        single_indices = NA,
                        mult_indices = NA,
                        typo_method = 'fast'){

  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In match_typos(), taxon names contain NA.')
  }

  ########################
  # Setup + find typos + create wcvp indices
  ########################
  # setup output
  out_match = rep(NA, length(taxon_names))
  out_message = rep('',length(taxon_names))

  # Get typos
  if(typo_method == 'fast'){
    fixed_typo = unlist(pbapply::pblapply(taxon_names, function(x){check_taxon_typo(x,NA, fast = T)}))
  }
  else{
    wcvp_without_repeated = wcvp$wcvp_names[match(unique(wcvp$wcvp_names$taxon_name),
                                                  wcvp$wcvp_names$taxon_name ),]
    fixed_typo = unlist(pbapply::pblapply(taxon_names, function(x){check_taxon_typo(x,wcvp_without_repeated, fast = F)}))

  }

  # Get index of taxons found to have typos.
  typo_indices =  which(!is.na(fixed_typo))

  # If no typos return.
  if(length(typo_indices) == 0){
    return(list(match = out_match, message = out_message))
  }

  if(is.na(single_indices[1])){
    single_indices = which(wcvp$wcvp_names$single_entry == TRUE)
  }
  if(is.na(mult_indices[1])){
    mult_indices = which(wcvp$wcvp_names$single_entry == FALSE)
  }

  name_to_try = fixed_typo[typo_indices]

  ########################
  # Match to single.
  ########################
  match_info = match_single_wcvp(name_to_try, wcvp, single_indices)
  out_match[typo_indices] = match_info$match
  out_message[typo_indices] = paste0(out_message[typo_indices], match_info$message)
  index_complete = typo_indices[!is.na(match_info$match)]
  index_to_find_matches = which(is.na(match_info$match))

  ########################
  # Match to multiple.
  ########################
  if(length(index_to_find_matches) > 0){
    typo_index = typo_indices[index_to_find_matches]
    match_info = match_mult_wcvp(name_to_try[index_to_find_matches], taxon_authors[typo_index],  wcvp, mult_indices)
    out_match[typo_index] = match_info$match
    out_message[typo_index] = paste0(out_message[typo_index], match_info$message)
  }

  ########################
  # Update match message to include the removed autonym.
  ########################
  with_match = out_message[typo_indices] != ''
  out_message[typo_indices[with_match]] = paste0(" -> (Typo) -> ", name_to_try[with_match],  out_message[typo_indices[with_match]])


  return(list(match = out_match, message = out_message))
}

#' match_all_issue()
#'
#' @param taxon_names taxon_names
#' @param taxon_authors taxon_authors
#' @param do_taxon_author do_taxon_author
#' @param wcvp wcvp
#' @param single_indices single_indices
#' @param mult_indices mult_indices
#'
#' @return return
#' @export
#'
match_all_issue <- function(taxon_names,
                            taxon_authors,
                            do_taxon_author,
                            wcvp,
                            single_indices = NA,
                            mult_indices = NA){
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }
  # Try removing autonyms
  match_auto = match_rm_autonym(taxon_names, taxon_authors, wcvp,
                                single_indices = single_indices,
                                mult_indices = mult_indices)
  # Try infrageneric change
  match_splitter = add_splitter(taxon_names, taxon_authors, wcvp)

  # Try hybrid change
  match_hybrid = match_hybrid_issue(taxon_names, taxon_authors, wcvp)

  #Combine matches.
  matches = data.frame(autonym = match_auto$match, infra = match_splitter$match, hybrid = match_hybrid$match)
  messages = data.frame(autonym = match_auto$message, infra = match_splitter$message, hybrid = match_hybrid$message)

  no_method_find_match = apply(matches, 1,function(x){sum(!is.na(x))})
  combined_names = rep(NA,length(taxon_names))
  for(i in which(no_method_find_match > 1)){
    auto_indices = matches[i,][!is.na(matches[i,])]
    combined_names[i] = paste0(wcvp$wcvp_names$taxon_name[auto_indices],collapse =' OR ')
  }

  # If we don't have authors we decide which is the best order.
  if(!do_taxon_author){
    # Order to keep 1) Infra 2) Hybrid  3) Autonym
    match_best = matches$autonym
    match_best[!is.na(matches$hybrid)] = matches$hybrid[!is.na(matches$hybrid)]
    match_best[!is.na(matches$infra)] = matches$infra[!is.na(matches$infra)]

    message = messages$autonym
    message[!is.na(messages$hybrid)] = messages$hybrid[!is.na(messages$hybrid)]
    message[!is.na(messages$infra)] = messages$infra[!is.na(messages$infra)]

    message[no_method_find_match > 1] <- paste0(' -> (Multiple methods provide possible match solutions) -> (',
                                                combined_names[no_method_find_match > 1],
                                                ' -> (Choose best method)',
                                                message[no_method_find_match > 1]
    )
    return(list(match = match_best, message = message))
  }


  # If we have authors pick best match by author first.
  if(do_taxon_author){

    #setup match output
    match_best = rep(NA, length(taxon_names))
    message =  rep('',length(taxon_names))

    ##################
    ## Case 1) Single method gives a match
    ##################
    # If we only have a single method with a match choose that match entry and message
    single_method_index = which(no_method_find_match == 1)
    if(length(single_method_index) > 0){
      match_best[single_method_index] = as.numeric(apply(as.matrix(matches[single_method_index,]),1, function(x){x[!is.na(x)]}))
      message[single_method_index] = as.character(apply(as.matrix(messages[single_method_index,]),1, function(x){x[x != '']}))
    }


    ##################
    ## Case 2) multiple methods gives a match
    ##################
    # Find the records with multiple methods giving rise to a match
    mult_indices = which(no_method_find_match > 1)

    if(length(mult_indices) > 0){
      NAs = rep(NA, length(mult_indices))
      matches_cur = NAs
      messages_cur = NAs

      # Get the corresponding authors.
      author_auto = NAs ; author_splitter = NAs ; author_hybrid = NAs

      auto_index = which(match_auto$match[mult_indices] > 0)
      author_auto[auto_index] = wcvp$wcvp_names$taxon_authors_simp[match_auto$match[mult_indices][auto_index]]

      splitter_index = which(match_splitter$match[mult_indices] > 0)
      author_splitter[splitter_index] = wcvp$wcvp_names$taxon_authors_simp[match_splitter$match[match_splitter$match[mult_indices][splitter_index]]]

      hybrid_index = which(match_hybrid$match[mult_indices] > 0)
      author_hybrid[hybrid_index] = wcvp$wcvp_names$taxon_authors_simp[match_hybrid$match[match_hybrid$match[mult_indices][hybrid_index]]]

      authors = data.frame(original = taxon_authors[mult_indices],autonym = author_auto, infra = author_splitter, hybrid = author_hybrid)


      #Choose the best records according to author matching.
      author_choose = apply(authors,1,function(x){
        # Get the original author and matched authors
        auth_orig = as.character(x[1])
        auth_match = as.character(x[-1])
        # auth_match = auth_match[!is.na(auth_match)]

        #If the original author = NA no author match possible.
        if(is.na(auth_orig)){
          return(list(match = rep(TRUE, length(auth_match)), message = '(No author match)'))
        }

        #Try to match the authors exactly.
        exact_match = auth_match == auth_orig
        if(sum(exact_match,na.rm = T)>0){
          exact_match[is.na(exact_match)] = FALSE
          return(list(match = exact_match, message = '(Exact author match)'))
        }

        #Try to match the author's partially.
        # Get the words for the original and POWO authors.
        possible_author_words = lapply(auth_match, author_words)
        original_author_words = author_words(auth_orig)

        #Find number of words in POWO authors found in the original author
        no_powo_author_in_original = unlist(lapply(possible_author_words,function(x){
          # If no author words return zero.
          if(length(x) == 0){
            return(0)
          }
          words = stringr::str_split(x,', ')[[1]]
          words = words[words != '']
          contain_words = unlist(lapply(words, function(x){grepl(x,auth_orig)}))
          return(sum(contain_words))
        }))

        #Find number of words in original author found in the POWO authors
        if(length(original_author_words)==0){
          no_original_author_in_powo = rep(0,length(auth_match))
        }
        else{
          no_original_author_in_powo = rowSums(data.frame(lapply(original_author_words, function(x){
            grepl(x,auth_match)}
          )))
        }


        # Combine above and find the maximum shared words.
        total_match_word_count = rowSums(cbind(no_powo_author_in_original,no_original_author_in_powo))
        max_words_found =  max(total_match_word_count, na.rm=T)

        # If maximum shared words > 0 then try and find a match with those with the maximum number of shared words.
        if(max_words_found > 0){
          partial_match = total_match_word_count == max_words_found
          partial_match[is.na(partial_match)] = FALSE
          return(list(match = partial_match, message = '(Partial author match)'))
        }
        else{
          return(list(match = rep(TRUE, length(auth_match)), message = '(No author match)'))
        }

      })

      # Run match_taxon_status on each selection of best authors.
      for(i in 1:length(mult_indices)){
        corres_POWO = wcvp$wcvp_names[as.numeric(matches[mult_indices[i],!is.na(matches[mult_indices[i],])]),]
        corres_match_cur = author_choose[[i]]$match[!is.na(matches[mult_indices[i],])]
        match_cur = match_taxon_status(author_match = corres_match_cur,
                                       corres_POWO = corres_POWO,
                                       current_message = paste0('-> ',author_choose[[i]]$message))
        #Note that match_taxon_status returns the plant_name_id and not the row number of the match
        matches_cur[i] = match(match_cur$plant_name_id, wcvp$wcvp_names$plant_name_id)
        messages_cur[i] = match_cur$message
      }

      # If the match == -2 use the method in our preferred order.
      no_match = matches_cur == -2
      corres_matches = apply(matches[mult_indices,][no_match,],1,function(x){
        if(!is.na(x[2])){
          return(x[2])
        }
        if(!is.na(x[3])){
          return(x[3])
        }
        if(!is.na(x[1])){
          return(x[1])
        }
      })
      corres_messages = apply(messages[mult_indices,][no_match,],1,function(x){
        if(!is.na(x[2])){
          return(x[2])
        }
        if(!is.na(x[3])){
          return(x[3])
        }
        if(!is.na(x[1])){
          return(x[1])
        }
      })
      corres_messages = paste0('-> (Author/Taxon status can not choose record) -> (Use BGSmartR preferred)',
                               corres_messages)

      matches_cur[no_match] = as.numeric(corres_matches)
      messages_cur[no_match] = corres_messages
      messages_cur = paste0('-> (Multiple methods provide possible match solutions) -> (',
                            combined_names[mult_indices], ')',
                            messages_cur)


      match_best[mult_indices] = matches_cur
      message[mult_indices] = messages_cur
    }

    return(list(match = match_best, message = message))

  }





}

#' Match report to POWO via taxon name
#'
#' @param original_report A gardens original report
#' @param wcvp POWO database
#' @param taxon_name_col Column name in the original report for taxon name
#' @param taxon_name_full_col Column name in the original report for taxon name and author/s (combined)
#' @param taxon_author_col Column name in the original report for taxon author/s
#' @param typo_method Flag for whether we search for typos
#' @param try_add_split Flag for whether we search for missing f./var./subsp.
#' @param try_fix_hybrid Flag for whether we search for hybrid issues.
#' @param try_rm_autonym Flag for whether we try removing autonyms.
#' @param do_convert_accepted Flag for whether we convert to accepted names in POWO.
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`, where the match goes to the record with accepted status.
#' `$message` a message detailing the match.
#' @export
match_original_to_wcvp <- function(original_report, wcvp,
                                   taxon_name_col = 'TaxonName',
                                   taxon_name_full_col = NA,
                                   taxon_author_col = NA,
                                   typo_method = 'fast',
                                   try_add_split = TRUE, try_fix_hybrid = TRUE,
                                   try_rm_autonym = TRUE, do_convert_accepted=TRUE){
  if(!typo_method %in% c('full', 'fast','no')){
    stop('Invalid typo_method input!')
  }
  #Implies original_report and wcvp are already in the workspace.
  # cli::cli_h1("Matching names to POWO (WCVP)")
  no_records = nrow(original_report)
  cli::cli_alert_info("{.var {no_records}} records found.")

  ################################################
  # 1) Setup original report. (only look at unique taxon name / taxon name full and add is_autonym)
  ################################################
  # Get taxon name and taxon author out of the original report.
  cli::cli_h2("Extracting taxon names and authors from the original report")

  taxon_name = original_report[,match(taxon_name_col, names(original_report))]
  do_taxon_author = TRUE
  if(!is.na(taxon_author_col)){
    taxon_author = original_report[,match(taxon_author_col, names(original_report))]
    if(all(taxon_author == '')){
      do_taxon_author = FALSE
    }
  }
  else if(!is.na(taxon_name_full_col)){
    taxon_name_full = original_report[,match(taxon_name_full_col, names(original_report))]
    taxon_author = author_from_taxon_name_full(taxon_name, taxon_name_full)
    if(all(taxon_author == '')){
      do_taxon_author = FALSE
    }
  }
  else{
    taxon_author = rep('', length(taxon_name))
    do_taxon_author = FALSE
  }
  if(!do_taxon_author){
    #Simplify taxon_author names.
    cli::cli_h2("Taxon authors not provided.")
    cli::cli_h2("Reducing to unique taxon names")
  }
  else{
    taxon_author = stringi::stri_trans_general(taxon_author, id = "Latin-ASCII")
    cli::cli_h2("Reducing to unique taxon name and author combinations")
  }

  # Restrict to only unique taxon name and author combinations.
  taxon_name_and_author = data.frame(taxon_name = taxon_name, taxon_author = taxon_author)
  unique_taxon_name_and_author =unique(taxon_name_and_author)
  taxon_name_and_author_combined = do.call(paste, c(taxon_name_and_author, sep='-'))
  unique_taxon_name_and_author_combined = do.call(paste, c(unique_taxon_name_and_author, sep='-'))

  report_match = match(taxon_name_and_author_combined,unique_taxon_name_and_author_combined)

  taxon_name = unique_taxon_name_and_author$taxon_name
  taxon_author =  unique_taxon_name_and_author$taxon_author

  no_unique = length(taxon_name)
  cli::cli_alert_info("{.var {no_unique}} unique taxon names/ taxon name author combinations found.")


  ################################################
  # 2) Setup outputs.
  ################################################
  taxon_match_full = rep(NA,nrow(original_report))
  taxon_name_story_full = rep(NA,nrow(original_report))
  taxon_match = rep(NA, length(taxon_name))
  taxon_name_story = taxon_name
  index_to_find_matches = 1:length(taxon_name)
  index_complete = NULL

  ################################################
  # 3) Match the exceptions of the known not to be in POWO.
  ################################################
  # (Assume all exceptions are single records in POWO, this is the case currently)
  exception_indices = match(wcvp$exceptions$plant_name_id, wcvp$wcvp_names$plant_name_id)
  exception_indices = exception_indices[!is.na(exception_indices)]
  match_info = match_single_wcvp(taxon_name[index_to_find_matches], wcvp, exception_indices)
  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)
  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  cli::cli_alert_info("Found {length(index_complete)} exceptions to known not in POWO.")

  ################################################
  # 4) Remove known to not be in POWO. (set taxon match to -1)
  ################################################
  cli::cli_h2("Removing known not to be in POWO from {length(index_to_find_matches)} name{?s}")

  match_info = known_not_in_wcvp(taxon_name[index_to_find_matches])

  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  no_found = length(index_to_find_matches[!is.na(match_info$match)])
  cli::cli_alert_success("Found {no_found} known not to be in POWO")

  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]

  ################################################
  # 5) Sanitise taxon names.
  ################################################
  if(length(index_to_find_matches) > 0){
    cli::cli_h2("Sanitise taxon names")

    santise_taxon_name = unlist(pbapply::pblapply(taxon_name[index_to_find_matches],BGSmartR::sanitise_name))
    original_taxon_name = taxon_name
    taxon_name[index_to_find_matches] = santise_taxon_name

    indices_require_sanitise=which(taxon_name != original_taxon_name)
    taxon_name_story[indices_require_sanitise] = paste0(taxon_name_story[indices_require_sanitise],
                                                        ' -> (Sanitise name) -> ',
                                                        taxon_name[indices_require_sanitise])
    cli::cli_alert_success("Sanitising required for {length(indices_require_sanitise)} taxon names")
  }

  ################################################
  # 6) Match original report to all unique taxon names in POWO.
  ################################################
  if(length(index_to_find_matches) > 0){
    cli::cli_h2("Matching {length(index_to_find_matches)} name{?s} to unique taxon names")

    single_indices = which(wcvp$wcvp_names$single_entry == TRUE)
    match_info = match_single_wcvp(taxon_name[index_to_find_matches], wcvp, single_indices)

    taxon_match[index_to_find_matches] = match_info$match
    taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

    index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
    no_found = length(index_to_find_matches[!is.na(match_info$match)])
    cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

    index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  }

  ################################################
  # 7) Match original report to all taxon names with a multiple entry in POWO.
  ################################################
  if(length(index_to_find_matches) > 0){
    cli::cli_h2("Matching {length(index_to_find_matches)} name{?s} to non-unique taxon names")

    mult_indices = which(wcvp$wcvp_names$single_entry == FALSE)
    match_info = match_mult_wcvp(taxon_name[index_to_find_matches],taxon_author[index_to_find_matches],  wcvp, mult_indices)

    taxon_match[index_to_find_matches] = match_info$match
    taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

    index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
    no_found = length(index_to_find_matches[!is.na(match_info$match)])
    cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

    index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  }

  ################################################
  # 11) If the matched author is different after exact matching try to find a similar taxon name.
  ################################################
  if(do_taxon_author){
    # Find the records which have disagreeing authors.
    with_match = index_complete[which(!grepl('Not in POWO',taxon_name_story[index_complete]))]

    # Do we have at least one match.
    if(length(with_match) > 0){
      original_authors = taxon_author[with_match]
      matched_authors = rep(NA,length(original_authors))
      match_bigger_0 = taxon_match[with_match] > 0
      matched_authors[match_bigger_0] = wcvp$wcvp_names$taxon_authors_simp[taxon_match[with_match][match_bigger_0]]

      author_checked = rep(NA, length(original_authors))
      for(i in 1:length(author_checked)){
        author_checked[i] = author_check(original_authors[i],  matched_authors[i])
      }

      # Get the indices of those matched to a single record with author checked = 'Different'.
      diff_index = with_match[which(author_checked == 'Different')]

      if(length(diff_index) > 0){
        # Get matches trying to fix taxon name.
        match_info = match_all_issue(taxon_names = taxon_name[diff_index],
                                     taxon_authors = taxon_author[diff_index],
                                     wcvp = wcvp,
                                     single_indices = single_indices,
                                     mult_indices = mult_indices,
                                     do_taxon_author = do_taxon_author)

        proposed_authors = rep(NA, length(taxon_author[diff_index]))
        current_match = match_info$match
        current_match[is.na(current_match)] = -3
        proposed_authors[current_match > 0] = wcvp$wcvp_name$taxon_authors_simp[current_match[current_match >0]]

        # Check if proposed author is same/similar to original.
        compare_author_new = rep(NA, length(proposed_authors))
        for(i in 1:length(diff_index)){
          compare_author_new[i] = author_check(taxon_author[diff_index[i]],  proposed_authors[i])
        }
        compare_author_new[current_match < 0] = 'No Match'

        improved_author_index = which(compare_author_new %in% c('Identical', 'Partial'))
        if(length(improved_author_index)>0){
          taxon_match[diff_index[improved_author_index]] = match_info$match[improved_author_index]
          taxon_name_story[diff_index[improved_author_index]] =
            paste0(taxon_name_story[diff_index[improved_author_index]],
                   ' -> (Author differ) -> (Try fixing taxon name)',
                   match_info$message[improved_author_index])
        }
      }
    }

  }


  ################################################
  # 8) Try fixing taxon name issues.
  ################################################
  if(length(index_to_find_matches) > 0){
  cli::cli_h2("Testing and matching taxon name issues for {length(index_to_find_matches)} name{?s}")

  match_info = match_all_issue(taxon_names = taxon_name[index_to_find_matches],
                                taxon_authors = taxon_author[index_to_find_matches],
                                wcvp = wcvp,
                                single_indices = single_indices,
                                mult_indices = mult_indices,
                               do_taxon_author = do_taxon_author)

    taxon_match[index_to_find_matches] = match_info$match
    taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

    index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
    no_found = length(index_to_find_matches[!is.na(match_info$match)])
    cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

    index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  }

  ################################################
  # 9) Try to find typo and then match.
  ################################################
  if(typo_method %in%  c('fast','full') & length(index_to_find_matches) > 0){
    cli::cli_h2("Testing and matching typos for {length(index_to_find_matches)} name{?s}")

    match_info = match_typos(taxon_names = taxon_name[index_to_find_matches],
                             taxon_authors = taxon_author[index_to_find_matches],
                             wcvp = wcvp,
                             single_indices = single_indices,
                             mult_indices = mult_indices,
                             typo_method = typo_method)

    taxon_match[index_to_find_matches] = match_info$match
    taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

    index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
    no_found = length(index_to_find_matches[!is.na(match_info$match)])
    cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")


    index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  }

  ################################################
  # 10) Check if the authors match from the name match (before going to accepted name)
  ################################################
  # Get the proposed authors.
  proposed_authors = rep(NA, length(taxon_author))
  current_match = taxon_match
  current_match[is.na(current_match)] = -3
  proposed_authors[current_match > 0] = wcvp$wcvp_name$taxon_authors_simp[current_match[current_match >0]]
  matched_name = rep(NA, length(taxon_author))
  matched_name[current_match > 0] = wcvp$wcvp_name$taxon_name[current_match[current_match >0]]

  # Check if proposed author is same/similar to original.
  author_checked = rep(NA, length(proposed_authors))
  for(i in 1:length(author_checked)){
    author_checked[i] = author_check(taxon_author[i],  proposed_authors[i])
  }
  author_checked[current_match < 0] = 'No Match'

  ################################################
  # 11) Convert to accepted name where possible.
  ################################################
  if(do_convert_accepted){
    cli::cli_h2("Converting to accepted name..")

    match_info = convert_to_accepted_name(taxon_match, wcvp)

    taxon_match = match_info$match
    taxon_name_story = paste0(taxon_name_story, match_info$message)

    updated = sum(match_info$message != '')
    cli::cli_alert_success("Updated to accepted name for {updated} of {no_unique} names")

  }

  ################################################
  # 12) Set remaining taxon_match to -3 and add story.
  ################################################
  taxon_match[index_to_find_matches] = -3
  author_checked[index_to_find_matches] = 'No Match'
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], ' -> (Not in POWO)')

  ################################################
  # 13) Create a shortened version on the match details
  ################################################
  match_short = shorten_message(taxon_name_story)

  ################################################
  # 15) return match (to original report) and details of the matches (for unique plants).
  ################################################
  cli::cli_h2("Matching Complete")
  taxon_match_full = taxon_match[report_match]
  taxon_name_story_full = taxon_name_story[report_match]
  match_short_full = match_short[report_match]
  matched_name_full = matched_name[report_match]
  proposed_authors_full = proposed_authors[report_match]
  author_checked_full = author_checked[report_match]
  original_authors_full = taxon_author[report_match]
  return(list(match = taxon_match_full,
              details = taxon_name_story_full,
              details_short = match_short_full,
              match_taxon_name = matched_name_full,
              original_authors = original_authors_full,
              match_authors = proposed_authors_full,
              author_check = author_checked_full))
}
