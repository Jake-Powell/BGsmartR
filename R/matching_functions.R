# Functions used to match to POWO (wcvp_names)

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
  return(which(grepl(" sp\\.| gx |'.*?'|\\[|^Indet| gx|indet$|CV|cv$|cv\\.|Group|unkn|hybrid$|Hybrid |Unknown",taxon_names)))
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
  try_author_match = TRUE # flag for whether we have author information
  flag = TRUE # flag for whether we need to do checks.

  Authors = stringr::str_sub(taxon_full_current, start = stringr::str_length(taxon_name_current)+1, end = -1)
  Authors = stringr::str_squish(Authors)
  Authors_grepl = Authors
  Authors_grepl = stringr::str_replace_all(Authors_grepl,'\\.', '\\\\.')
  Authors_grepl = stringr::str_replace_all(Authors_grepl,'\\(', '\\\\(')
  Authors_grepl = stringr::str_replace_all(Authors_grepl,'\\)', '\\\\)')
  Authors_grepl = stringr::str_replace_all(Authors_grepl,'\\[', '\\\\[')
  Authors_grepl = stringr::str_replace_all(Authors_grepl,'\\]', '\\\\]')
  if(Authors == ''){
    try_author_match = FALSE
  }

  # 2) Get the corresponding records in wcvp_mult.
  # As we use grepl to match authors add escape characters (\\).
  POWO_cur = wcvp_mult[wcvp_mult$taxon_name == taxon_name_current,]
  taxon_author_grepl = POWO_cur$taxon_authors
  taxon_author_grepl = stringr::str_replace_all(taxon_author_grepl,'\\.', '\\\\.')
  taxon_author_grepl = stringr::str_replace_all(taxon_author_grepl,'\\(', '\\\\(')
  taxon_author_grepl = stringr::str_replace_all(taxon_author_grepl,'\\)', '\\\\)')


  ###
  # 3) Get the match by author (if one exists)
  ###
  if(try_author_match){
    # A) By author (exact).
    exact_match = Authors == POWO_cur$taxon_authors
    if(sum(exact_match) == 1){ # exactly 1 match
      # Is the single match accepted (or reference to accepted plant)
      # if(!is.na(POWO_cur$accepted_plant_name_id[exact_match])){
      matched = POWO_cur$plant_name_id[exact_match]
      message = '(Multiple POWO records, match by exact author)'
      flag = F
      # }
    }
    if(sum(exact_match) > 1){ # > 1 match
      POWO_cur = POWO_cur[exact_match,]
      accepted_plant_id = POWO_cur$accepted_plant_name_id

      # Check if all exact matches point to the same accepted name
      if(identical(accepted_plant_id, rep(accepted_plant_id[1], length(accepted_plant_id)))){
        # match to accepted if one exists if not the first plant that matches.
        taxon_accepted = POWO_cur$taxon_status == 'Accepted'
        if(any(taxon_accepted)){
          matched = POWO_cur$plant_name_id[taxon_accepted][1]
          message = '(Multiple POWO records, match by exact author)'
        }
        else{
          matched = POWO_cur$plant_name_id[1]
          message = '(Multiple POWO records, match by exact author)'

        }
        flag = F
      }

      # Check for differences in taxon_status.
      if(flag){
        taxon_status = POWO_cur$taxon_status
        taxon_status_match = match(taxon_status , c('Accepted', 'Synonym'))
        chosen_record = which.min(taxon_status_match)
        if(length(chosen_record)>0){
          matched = POWO_cur$plant_name_id[chosen_record]
          message = '(Multiple POWO records, multiple exact author, choose via taxon_status)'
          flag = F
        }
      }

      # If we can't chose by taxon_status (i.e no Accepted or Synonym) set to no match
      if(flag){
        matched = -2
        message = '(Multiple POWO records, multiple exact author, no accepted or synonym taxon status, unclear so no match)'
      }

      flag = F
    }


    # B) By author (partial: powo contained in taxon)
    if(flag){
      author_match = unlist(lapply(taxon_author_grepl, function(x){grepl(x,Authors)}))
      if(sum(author_match)==1){
        # if(!is.na(POWO_cur$accepted_plant_name_id[author_match])){
        matched = POWO_cur$plant_name_id[author_match]
        message = '(Multiple POWO records, match by partial author <powo author containing in taxon author>)'
        flag = F
        # }

      }
      if(sum(author_match) > 1){ # > 1 match
        POWO_cur = POWO_cur[author_match,]
        accepted_plant_id = POWO_cur$accepted_plant_name_id

        # Check if all exact matches point to the same accepted name
        if(identical(accepted_plant_id, rep(accepted_plant_id[1], length(accepted_plant_id)))){
          # match to accepted if one exists if not the first plant that matches.
          taxon_accepted = POWO_cur$taxon_status == 'Accepted'
          if(any(taxon_accepted)){
            matched = POWO_cur$plant_name_id[taxon_accepted][1]
            message = '(Multiple POWO records, match by partial author <powo author containing in taxon author>)'
          }
          else{
            matched = POWO_cur$plant_name_id[1]
            message = '(Multiple POWO records, match by partial author <powo author containing in taxon author>)'
          }
          flag = F
        }

        # Check for differences in taxon_status.
        if(flag){
          taxon_status = POWO_cur$taxon_status
          taxon_status_match = match(taxon_status , c('Accepted', 'Synonym'))
          chosen_record = which.min(taxon_status_match)
          if(length(chosen_record)>0){
            matched = POWO_cur$plant_name_id[chosen_record]
            message = '(Multiple POWO records, multiple partial author <powo author containing in taxon author>, choose via taxon_status)'
            flag = F
          }
        }

        # If we can't chose by taxon_status (i.e no Accepted or Synonym) set to no match
        if(flag){
          matched = -2
          message = '(Multiple POWO records, multiple partial author <powo author containing in taxon author>, no accepted or synonym taxon status, unclear so no match)'
        }


        flag = F

      }
    }

    # C) By author (partial taxon contained in powo)
    if(flag){
      author_match = grepl(Authors_grepl, POWO_cur$taxon_authors)
      if(sum(author_match)==1){
        # if(!is.na(POWO_cur$accepted_plant_name_id[author_match])){
        matched = POWO_cur$plant_name_id[author_match]
        message = '(Multiple POWO records, match by partial author <taxon author containing in powo author>)'
        flag = F
        # }

      }
      if(sum(author_match) > 1){ # > 1 match
        POWO_cur = POWO_cur[author_match,]
        accepted_plant_id = POWO_cur$accepted_plant_name_id

        # Check if all exact matches point to the same accepted name
        if(identical(accepted_plant_id, rep(accepted_plant_id[1], length(accepted_plant_id)))){
          # match to accepted if one exists if not the first plant that matches.
          taxon_accepted = POWO_cur$taxon_status == 'Accepted'
          if(any(taxon_accepted)){
            matched = POWO_cur$plant_name_id[taxon_accepted][1]
            message = '(Multiple POWO records, match by partial author <taxon author containing in powo author>)'
          }
          else{
            matched = POWO_cur$plant_name_id[1]
            message = '(Multiple POWO records, match by partial author <taxon author containing in powo author>)'
          }
        }

        # Check for differences in taxon_status.
        if(flag){
          taxon_status = POWO_cur$taxon_status
          taxon_status_match = match(taxon_status , c('Accepted', 'Synonym'))
          chosen_record = which.min(taxon_status_match)
          if(length(chosen_record)>0){
            matched = POWO_cur$plant_name_id[chosen_record]
            message = '(Multiple POWO records, multiple partial author  <taxon author containing in powo author>, choose via taxon_status)'
            flag = F
          }
        }

        # If we can't chose by taxon_status (i.e no Accepted or Synonym) set to no match
        if(flag){
          matched = -2
          message = '(Multiple POWO records, multiple partial author  <taxon author containing in powo author>, no accepted or synonym taxon status, unclear so no match)'
        }

        flag = F

      }
    }

    # D) By author (partial powo words contained in taxon)
    if(flag){
      author_words = POWO_cur$author_parts
      match_author_words = unlist(lapply(author_words,function(x){
        words = stringr::str_split(x,', ')[[1]]
        contain_words = unlist(lapply(words, function(x){grepl(x,Authors)}))
        return(sum(contain_words))

      }))

      # Older version without author_parts in wcvp.
      # match_author_words = unlist(lapply(taxon_author_grepl, function(x){
      #   words = stringr::str_split(x, ' ')[[1]]
      #   words = words[stringr::str_length(words)>2]
      #   contain_words = unlist(lapply(words, function(x){grepl(x,Authors)}))
      #   return(sum(contain_words))
      # }))

      if(length(match_author_words[match_author_words>0]) == 1){
        chosen_record = which.max(match_author_words)
        matched = POWO_cur$plant_name_id[chosen_record]
        message = '(Multiple POWO records, match by partial author  <taxon author contains words found in powo author>)'
        flag = F
      }
      else if(length(match_author_words[match_author_words>0]) > 1){

        #One record has the most words
        if(sum(match_author_words == max(match_author_words))==1){
          chosen_record = which.max(match_author_words)
          matched = POWO_cur$plant_name_id[chosen_record]
          message = '(Multiple POWO records, multiple partial author  <taxon author contains words found in powo author>, choose one with most words)'
          flag = F

        }
        else{
          #Use taxon status for those with the most words.
          record_with_most_words = which(match_author_words == max(match_author_words))

          taxon_status = POWO_cur$taxon_status[record_with_most_words]
          taxon_status_match = match(taxon_status , c('Accepted', 'Synonym'))
          chosen_record = which.min(taxon_status_match)
          if(length(chosen_record)>0){
            matched = POWO_cur$plant_name_id[record_with_most_words[chosen_record]]
            message = '(Multiple POWO records, multiple partial author  <taxon author contains words found in powo author>, multiple records with max number of partial word match, choose via taxon_status)'
            flag = F
          }
        }
      }

    }
  }

  ###
  # 4) Try to match when we do not have the author or the author doesn't match any in POWO.
  ###

  # A) Check if the accepted_plant_id is identical across all POWO records.
  if(flag){
    accepted_plant_id = POWO_cur$accepted_plant_name_id

    if(identical(accepted_plant_id, rep(accepted_plant_id[1], length(accepted_plant_id)))){
      # match to accepted if one exists if not the first plant that matches.
      taxon_accepted = POWO_cur$taxon_status == 'Accepted'
      if(any(taxon_accepted)){
        matched = POWO_cur$plant_name_id[taxon_accepted][1]
        message = '(Multiple POWO records, no author/no matched author, all lead to same accepted plant name)'
      }
      else{
        matched = POWO_cur$plant_name_id[1]
        message = '(Multiple POWO records, no author/no matched author, all lead to same accepted plant name)'

      }
      flag = F
    }

  }

  # B) Use taxon_status to choose POWO record.
  if(flag){
    taxon_status = POWO_cur$taxon_status
    taxon_status_match = match(taxon_status , c('Accepted', 'Synonym'))
    chosen_record = which.min(taxon_status_match)
    if(length(chosen_record)>0){
      matched = POWO_cur$plant_name_id[chosen_record]
      message = '(Multiple POWO records, no author/no matched author, choose via taxon_status)'
      flag = F
    }


  }

  # If we can't chose by taxon_status (i.e no Accepted or Synonym) set to no match
  if(flag){
    matched = -2
    message = '(Multiple POWO records, no author/no matched author, no accepted or synonym taxon status, unclear so no match)'
  }

  return(list(plant_name_id = matched, message = message))
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
  match_info = pbapply::pblapply(to_find_match, function(x){
    get_match_from_multiple(x,wcvp_multiple)})
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
#' @param taxon_names_full taxon names with author
#' @param wcvp enrich information
#' @param single_indices wcvp indices with a unique taxon name
#' @param mult_indices wcvp indices with non-unique taxon name
#'
#' @return a list with match and message
#' @export
match_rm_autonym <- function(taxon_names, taxon_names_full, wcvp,
                             single_indices = NA,
                             mult_indices = NA){

  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In match_autonym(), taxon names contain NA.')
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
    match_info = match_mult_wcvp(name_to_try[index_to_find_matches], taxon_names_full[auto_index],  wcvp, mult_indices)
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

#' add_splitter()
#'
#' @param taxon_names taxon names
#' @param taxon_names_full taxon names with author
#' @param wcvp enrich information
#'
#' @return a list with match and message
#' @export
#'
add_splitter <- function(taxon_names, taxon_names_full, wcvp){
  # We know from exploring POWO that var/f/subsp only occurs after the genus species. (with the potential addition of 'x' or '+' for hybrids)
  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In add_splitter(), taxon names contain NA.')
  }

  ########################
  # Setup + find words of length 3 and 4 + words with a splitter.
  ########################
  splitters = c('subsp.', 'var.', 'f.', 'nothosubsp.')
  splitters_grepl = ' subsp\\. | var\\. | f\\. | nothosubsp\\. '
  out_match = rep(NA, length(taxon_names))
  out_message = rep('',length(taxon_names))
  no_words = stringr::str_count(taxon_names, ' ')+1
  index_words_3 = which(no_words == 3 & !grepl('\u00D7|\\+',taxon_names))
  index_words_4 = which(no_words == 4 & grepl('\u00D7|\\+',taxon_names))
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
    taxon_full_3 = as.list(taxon_names_full[index_words_3])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_3, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_splitters_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],8), wcvp, wcvp_index_splitters_mult)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      #Index of the matches we found.
      found_match_index = which(!is.na(match_details$match))
      mes_index = found_match_index%%length(x[[1]])
      mes_index[mes_index == 0] = length(x[[1]])

      # If we get a single match return it.
      if(length(found_match_index)==1){
        match = match_details$match[found_match_index]
        message = paste0(' -> (Add splitter) -> ', x[[1]][mes_index], match_details$message[found_match_index])
        return(c(match,message))
      }
      # Multiple matches unclear which is best so don't give a match.
      if(length(found_match_index)>1){
        match = -5
        message = paste0(' -> (Add splitter, multiple matches, unclear which to match) -> (', paste0(x[[1]][mes_index],collapse = ' OR '), ')')
        return(c(match,message))
      }

      #Else we have no match
      return(c(NA,''))

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


    words_4 = stringr::str_split(taxon_names[index_words_4], ' ')
    to_try_words = lapply(words_4, function(x){
      just_splitter = paste(x[1], x[2], x[3], splitters, x[4])
      return(just_splitter)
    })
    taxon_full_4 = as.list(taxon_names_full[index_words_4])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_4, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_splitters_single_h)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],4), wcvp, wcvp_index_splitters_mult_h)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      #Index of the matches we found.
      found_match_index = which(!is.na(match_details$match))
      mes_index = found_match_index%%length(x[[1]])
      mes_index[mes_index == 0] = length(x[[1]])

      # If we get a single match return it.
      if(length(found_match_index)==1){
        match = match_details$match[found_match_index]
        message = paste0(' -> (Add splitter) -> ', x[[1]][mes_index], match_details$message[found_match_index])
        return(c(match,message))
      }
      # Multiple matches unclear which is best so don't give a match.
      if(length(found_match_index)>1){
        match = -5
        message = paste0(' -> (Add splitter, multiple matches, unclear which to match) -> (', paste0(x[[1]][mes_index],collapse = ' OR '), ')')
        return(c(match,message))
      }

      #Else we have no match
      return(c(NA,''))

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
    #Get the corresponding taxon_names_full (don't change here as we only check for author.)
    taxon_full_splitter = as.list(taxon_names_full[index_words_with_splitter])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_splitter, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_splitters_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],3), wcvp, wcvp_index_splitters_mult)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      #Index of the matches we found.
      found_match_index = which(!is.na(match_details$match))
      mes_index = found_match_index%%length(x[[1]])
      mes_index[mes_index == 0] = length(x[[1]])

      # If we get a single match return it.
      if(length(found_match_index)==1){
        match = match_details$match[found_match_index]
        message = paste0(' -> (Change splitter) -> ', x[[1]][mes_index], match_details$message[found_match_index])
        return(c(match,message))
      }
      # Multiple matches unclear which is best so don't give a match.
      if(length(found_match_index)>1){
        match = -5
        message = paste0(' -> (Change splitter, multiple matches, unclear which to match) -> (', paste0(x[[1]][mes_index],collapse = ' OR '), ')')
        return(c(match,message))
      }

      #Else we have no match
      return(c(NA,''))

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
#' @param taxon_names_full taxon names with author
#' @param wcvp enrich information
#'
#' @return a list with match and message
#' @export
match_hybrid_issue <- function(taxon_names, taxon_names_full, wcvp){
  # We know from exploring POWO that var/f/subsp only occurs after the genus species. (with the potential addition of 'x' or '+' for hybrids)

  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In match_hybrid_issue(), taxon names contain NA.')
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
    taxon_full_1 = as.list(taxon_names_full[index_words_1])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_1, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_hybrid_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],2), wcvp, wcvp_index_hybrid_mult)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      #Index of the matches we found.
      found_match_index = which(!is.na(match_details$match))
      mes_index = found_match_index%%length(x[[1]])
      mes_index[mes_index == 0] = length(x[[1]])

      # If we get a single match return it.
      if(length(found_match_index)==1){
        match = match_details$match[found_match_index]
        message = paste0(' -> (Hybrid fix) -> ', x[[1]][mes_index], match_details$message[found_match_index])
        return(c(match,message))
      }
      # Multiple matches unclear which is best so don't give a match.
      if(length(found_match_index)>1){
        match = -5
        message = paste0(' -> (Hybrid fix, multiple matches, unclear which to match) -> (', paste0(x[[1]][mes_index],collapse = ' OR '), ')')
        return(c(match,message))
      }

      #Else we have no match
      return(c(NA,''))

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
    taxon_full_2_3_4 = as.list(taxon_names_full[index_words_2_3_4])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_2_3_4, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp, wcvp_index_hybrid_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],4), wcvp, wcvp_index_hybrid_mult)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      #Index of the matches we found.
      found_match_index = which(!is.na(match_details$match))
      mes_index = found_match_index%%length(x[[1]])
      mes_index[mes_index == 0] = length(x[[1]])

      # If we get a single match return it.
      if(length(found_match_index)==1){
        match = match_details$match[found_match_index]
        message = paste0(' -> (Hybrid fix) -> ', x[[1]][mes_index], match_details$message[found_match_index])
        return(c(match,message))
      }
      # Multiple matches unclear which is best so don't give a match.
      if(length(found_match_index)>1){
        match = -5
        message = paste0(' -> (Hybrid fix, multiple matches, unclear which to match) -> (', paste0(x[[1]][mes_index],collapse = ' OR '), ')')
        return(c(match,message))
      }

      #Else we have no match
      return(c(NA,''))

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

    taxon_full_with_hybrid = as.list(taxon_names_full[index_words_with_hybrid])

    # Combine the taxon names to try with taxon name full.
    to_try = mapply(list,to_try_words, taxon_full_with_hybrid, SIMPLIFY = FALSE)

    match_info = unlist(pbapply::pblapply(to_try, function(x){
      # x here are the potential taxon names with splitters added.

      # Match to either single or multiple.
      match_details_single = match_single_wcvp(x[[1]], wcvp , wcvp_index_single)
      match_details_mult = match_mult_wcvp(x[[1]], rep(x[[2]],3), wcvp, wcvp_index_mult)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      #Index of the matches we found.
      found_match_index = which(!is.na(match_details$match))
      mes_index = found_match_index%%length(x[[1]])
      mes_index[mes_index == 0] = length(x[[1]])

      # If we get a single match return it.
      if(length(found_match_index)==1){
        match = match_details$match[found_match_index]
        message = paste0(' -> (Hybrid fix) -> ', x[[1]][mes_index], match_details$message[found_match_index])
        return(c(match,message))
      }
      # Multiple matches unclear which is best so don't give a match.
      if(length(found_match_index)>1){
        match = -5
        message = paste0(' -> (Hybrid fix, multiple matches, unclear which to match) -> (', paste0(x[[1]][mes_index],collapse = ' OR '), ')')
        return(c(match,message))
      }

      #Else we have no match
      return(c(NA,''))

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
                      'ae', 'eae',
                      'e', 'is',
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
#' @param taxon_names_full taxon names with author
#' @param wcvp enrich information
#' @param single_indices wcvp indices with a unique taxon name
#' @param mult_indices wcvp indices with non-unique taxon name
#' @param typo_method method for finding typos
#'
#' @return a list with match and message
#' @export
match_typos <- function(taxon_names, taxon_names_full, wcvp,
                        single_indices = NA,
                        mult_indices = NA,
                        typo_method = 'fast'){

  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In match_autonym(), taxon names contain NA.')
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
    match_info = match_mult_wcvp(name_to_try[index_to_find_matches], taxon_names_full[typo_index],  wcvp, mult_indices)
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



#' Match report to POWO via taxon name
#'
#' @param original_report A gardens original report
#' @param wcvp POWO database
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
match_original_to_wcvp <- function(original_report, wcvp, typo_method = 'fast',
                                   try_add_split = TRUE, try_fix_hybrid = TRUE,
                                   try_rm_autonym = TRUE, do_convert_accepted=TRUE){
  if(!typo_method %in% c('full', 'fast','no')){
    stop('Invalid typo_method input!')
  }
  #Implies original_report and wcvp are already in the workspace.
  cli::cli_h1("Matching names to POWO (WCVP)")
  no_records = nrow(original_report)
  cli::cli_alert_info("{.var {no_records}} records found.")

  ################################################
  # 1) Setup original report. (only look at unique taxon name / taxon name full and add is_autonym)
  ################################################
  # Extract taxon name and taxon name full used in the matching.
  taxon_name_and_full = original_report[,match(c('TaxonName','TaxonNameFull'), names(original_report))]
  unique_taxon_name_and_full =unique(taxon_name_and_full)
  taxon_name_and_full_combined = do.call(paste, c(taxon_name_and_full, sep='-'))
  unique_taxon_name_and_full_combined = do.call(paste, c(unique_taxon_name_and_full, sep='-'))

  report_match = match(taxon_name_and_full_combined,unique_taxon_name_and_full_combined)
  unique_taxon_name_and_full = add_is_autonym(unique_taxon_name_and_full)
  taxon_name = unique_taxon_name_and_full$TaxonName
  taxon_name_full =  unique_taxon_name_and_full$TaxonNameFull

  no_unique = length(taxon_name)
  cli::cli_alert_info("{.var {no_unique}} unique taxon names/ taxon name full combinations found.")


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
  no_found = length(index_complete)
  cli::cli_alert_info("Found {.var {no_found}} exceptions to known not in POWO.")

  ################################################
  # 4) Remove known to not be in POWO. (set taxon match to -1)
  ################################################
  cli::cli_h2("(1/7) Removing known not to be in POWO {length(index_to_find_matches)} name{?s}")
  indices = known_not_in_wcvp(taxon_name[index_to_find_matches])
  taxon_match[index_to_find_matches[indices]] = -1
  taxon_name_story[index_to_find_matches[indices]] = paste0(taxon_name_story[index_to_find_matches[indices]], ' -> (Not in POWO <known not to be in POWO>)')
  index_complete = c(index_complete, indices)
  index_to_find_matches = index_to_find_matches[!index_to_find_matches %in% index_to_find_matches[indices]]
  cli::cli_alert_success("Found {length(indices)} known not to be in POWO")

  ################################################
  # 5) Sanitise taxon names.
  ################################################
  cli::cli_h2("(2/7) Sanitise taxon names")

  santise_taxon_name = unlist(pbapply::pblapply(taxon_name[index_to_find_matches],BGSmartR::sanitise_name))
  original_taxon_name = taxon_name
  taxon_name[index_to_find_matches] = santise_taxon_name

  indices_require_sanitise=which(taxon_name != original_taxon_name)
  taxon_name_story[indices_require_sanitise] = paste0(taxon_name_story[indices_require_sanitise],
                                             ' -> (Sanitise name) -> ',
                                             taxon_name[indices_require_sanitise])
  cli::cli_alert_success("Sanitising required for {length(indices_require_sanitise)} taxon names")

  ################################################
  # 6) Match original report to all unique taxon names in POWO.
  ################################################
  cli::cli_h2("(3/7) Matching {length(index_to_find_matches)} name{?s} to unique taxon names")

  single_indices = which(wcvp$wcvp_names$single_entry == TRUE)
  match_info = match_single_wcvp(taxon_name[index_to_find_matches], wcvp, single_indices)

  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  no_found = length(index_to_find_matches[!is.na(match_info$match)])
  cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]


  ################################################
  # 7) Match original report to all taxon names with a multiple entry in POWO.
  ################################################
  cli::cli_h2("(3/7) Matching {length(index_to_find_matches)} name{?s} to non-unique taxon names")

  mult_indices = which(wcvp$wcvp_names$single_entry == FALSE)
  match_info = match_mult_wcvp(taxon_name[index_to_find_matches],taxon_name_full[index_to_find_matches],  wcvp, mult_indices)

  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  no_found = length(index_to_find_matches[!is.na(match_info$match)])
  cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]

  ################################################
  # 8) Try to match with autonym removed.
  ################################################
  if(try_rm_autonym){
    cli::cli_h2("(4/7) Testing and matching autonynms for {length(index_to_find_matches)} name{?s}")

    match_info = match_rm_autonym(taxon_names = taxon_name[index_to_find_matches],
                                  taxon_names_full = taxon_name_full[index_to_find_matches],
                                  wcvp = wcvp,
                                  single_indices = single_indices,
                                  mult_indices = mult_indices)

    taxon_match[index_to_find_matches] = match_info$match
    taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

    index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
    no_found = length(index_to_find_matches[!is.na(match_info$match)])
    cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

    index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  }

  ################################################
  # 9) Try to match by adding hybridisation.
  ################################################
  if(try_fix_hybrid){
    cli::cli_h2("(5/7) Testing and matching hybrid issues for {length(index_to_find_matches)} name{?s}")
    match_info = match_hybrid_issue(taxon_names = taxon_name[index_to_find_matches],
                              taxon_names_full = taxon_name_full[index_to_find_matches],
                              wcvp = wcvp)
    taxon_match[index_to_find_matches] = match_info$match
    taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

    index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
    no_found = length(index_to_find_matches[!is.na(match_info$match)])
    cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

    index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  }


  ################################################
  # 8) Try adding/updating splitter.
  ################################################
  if(try_add_split){
    cli::cli_h2("(4/7) Testing and matching adding f/var/subsp for {length(index_to_find_matches)} name{?s}")
    match_info = add_splitter(taxon_names = taxon_name[index_to_find_matches],
                              taxon_names_full = taxon_name_full[index_to_find_matches],
                              wcvp = wcvp)
    taxon_match[index_to_find_matches] = match_info$match
    taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)

    index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
    no_found = length(index_to_find_matches[!is.na(match_info$match)])
    cli::cli_alert_success("Found {no_found} of {length(index_to_find_matches)} names")

    index_to_find_matches = index_to_find_matches[is.na(match_info$match)]
  }

  ################################################
  # 10) Try to find typo and then match.
  ################################################
  if(typo_method %in%  c('fast','full')){
    cli::cli_h2("Testing and matching typos for {length(index_to_find_matches)} name{?s}")

    match_info = match_typos(taxon_names = taxon_name[index_to_find_matches],
                                  taxon_names_full = taxon_name_full[index_to_find_matches],
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
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], ' -> (Not in POWO)')

  ################################################
  # 13) Create a shortened version on the match details
  ################################################
  match_short = rep('', length(taxon_name_story))
  match_options = c("(matches POWO record with single entry)", '1',
                           "(Multiple POWO records, match by exact author)", '1',
                           "(Multiple POWO records, multiple exact author, choose via taxon_status)", "3",
                           "(Multiple POWO records, multiple exact author, no accepted or synonym taxon status, unclear so no match)", "6",
                           "(Multiple POWO records, match by partial author <powo author containing in taxon author>)", "2",
                           "(Multiple POWO records, multiple partial author <powo author containing in taxon author>, choose via taxon_status)", "3",
                           "(Multiple POWO records, multiple partial author <powo author containing in taxon author>, no accepted or synonym taxon status, unclear so no match)", "6",
                           "(Multiple POWO records, match by partial author <taxon author containing in powo author>)", "2",
                           "(Multiple POWO records, multiple partial author  <taxon author containing in powo author>, choose via taxon_status)", '3',
                           "(Multiple POWO records, multiple partial author  <taxon author containing in powo author>, no accepted or synonym taxon status, unclear so no match)", '6',
                           "(Multiple POWO records, match by partial author  <taxon author contains words found in powo author>)", '2',
                           "(Multiple POWO records, multiple partial author  <taxon author contains words found in powo author>, choose one with most words)", '3',
                           "(Multiple POWO records, multiple partial author  <taxon author contains words found in powo author>, multiple records with max number of partial word match, choose via taxon_status)", '6',
                           "(Multiple POWO records, no author/no matched author, all lead to same accepted plant name)", '1',
                           "(Multiple POWO records, no author/no matched author, choose via taxon_status)", '3',
                           "(Multiple POWO records, no author/no matched author, no accepted or synonym taxon status, unclear so no match)", '6',
                           "(Not in POWO <known not to be in POWO>)", '7',
                           "(Autonym. Does not exist in POWO. Convert to base name)", '5',
                           "(Typo)", 'T',
                           "(Not in POWO)", '8',
                           "(Go to accepted name)", 'A',
                           "(Sanitise)", 'S',
                           "splitter", 'L',
                           "(Try adding hybrid)", 'H'
                    )
  match_options = stringr::str_replace_all(match_options, '\\(', '\\\\(')
  match_options = stringr::str_replace_all(match_options, '\\)', '\\\\)')
  match_options = matrix(match_options, byrow = T, ncol=2)
  for(i in 1:nrow(match_options)){
    indices = grepl(match_options[i,1], taxon_name_story)
    match_short[indices] = paste0(match_short[indices],', ', match_options[i,2])
  }
  match_short = stringr::str_remove(match_short, '^, ')

  ################################################
  # 14) return match (to original report) and details of the matches (for unique plants).
  ################################################
  cli::cli_h2("Matching Complete")
  taxon_match_full = taxon_match[report_match]
  taxon_name_story_full = taxon_name_story[report_match]
  match_short_full = match_short[report_match]
  return(list(match = taxon_match_full, details = taxon_name_story_full, details_short = match_short_full))
}
