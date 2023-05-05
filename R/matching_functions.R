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

  Authors = stringr::str_sub(taxon_full_current, start = stringr::str_length(taxon_name_current)+1, end = stringr::str_length(taxon_full_current))
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
      match_author_words = unlist(lapply(taxon_author_grepl, function(x){
        words = stringr::str_split(x, ' ')[[1]]
        words = words[stringr::str_length(words)>2]
        contain_words = unlist(lapply(words, function(x){grepl(x,Authors)}))
        return(sum(contain_words))
      }))
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
  match_info = pbapply::pblapply(to_find_match, function(x){get_match_from_multiple(x,wcvp_multiple)})
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
#'
#' @return a potential fixed name, or NA if no correction is found.
#' @export
check_taxon_typo <- function(taxon, wcvp){
  # 1) Since no words in wcvp have '(',')' we return null.
  if(grepl('\\(|\\)',taxon)){return(NA)}

  # 2) reduce the wcvp names to check. And split into three vectors for same length, one less and one more.

  #     A) Make sure the wcvp names are either the same length or one extra character.
  length_taxon = stringr::str_length(taxon)
  wcvp_needed = wcvp[wcvp$taxon_length %in% c(length_taxon-1, length_taxon, length_taxon+1),]
  #     B) Make sure we only look at taxons which contain only one word with a change (after removing words with '.')
  words = stringr::str_split(taxon,' ')[[1]]
  words = words[!grepl('\\.', words)]
  pat = paste0(words,collapse = '|')
  wcvp_needed = wcvp_needed[grepl(pat, wcvp_needed$taxon_name ),]
  wcvp_needed_minus_1 = wcvp_needed$taxon_name[wcvp_needed$taxon_length == (length_taxon-1)]
  wcvp_needed_same = wcvp_needed$taxon_name[wcvp_needed$taxon_length == (length_taxon)]
  wcvp_needed_plus_1 = wcvp_needed$taxon_name[wcvp_needed$taxon_length == (length_taxon+1)]


  for(i in (length_taxon-1):1){
    #Check changing a single letter.
    patternA = paste0(stringr::str_sub(taxon,1,i),'[a-zA-Z]',stringr::str_sub(taxon,i+2,length_taxon))
    fixed_typoA = wcvp_needed_same[grepl(patternA, wcvp_needed_same)]
    if(length(fixed_typoA) >0){
      return(fixed_typoA[1])
    }

    #Check adding a single new letter
    patternB = paste0(stringr::str_sub(taxon,1,i),'[a-zA-Z]',stringr::str_sub(taxon,i+1,length_taxon))
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
  return(NA)
}

#' match taxon and taxon name full to POWO
#'
#' For each unique taxon name / taxon name full find the corresponding record in POWO.
#'
#' @param taxon_name_and_full data frame of two columns where the first column is the taxon_name and second column is the full taxon name (with authors).
#' @param wcvp POWO database
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`, where the match goes to the record with accepted status.
#' `$message` a message detailing the match.
#' @export
match_taxon_to_wcvp <- function(taxon_name_and_full, wcvp){

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
  taxon_name_story[index_to_find_matches[indices]] = paste0(taxon_name_story[index_to_find_matches[indices]], ' -> (Not in POWO <known not to be in POWO>)')
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

#' Match report to POWO via taxon name
#'
#' @param original_report A gardens original report
#' @param wcvp POWO database
#'
#' @return A list of length two containing:
#' `$match` the index of the match from `taxon_names` to `wcvp`, where the match goes to the record with accepted status.
#' `$message` a message detailing the match.
#' @export
match_original_to_wcvp <- function(original_report, wcvp){
  #Implies original_report and wcvp are already in the workspace.

  ###
  # 1) Setup original report. (only look at unique taxon name / taxon name full and add is_autonym)
  ###
  # Extract taxon name and taxon name full used in the matching.
  taxon_name_and_full = original_report[,match(c('TaxonName','TaxonNameFull'), names(original_report))]
  unique_taxon_name_and_full =unique(taxon_name_and_full)
  taxon_name_and_full_combined = do.call(paste, c(taxon_name_and_full, sep='-'))
  unique_taxon_name_and_full_combined = do.call(paste, c(unique_taxon_name_and_full, sep='-'))

  report_match = match(taxon_name_and_full_combined,unique_taxon_name_and_full_combined)
  unique_taxon_name_and_full = add_is_autonym(unique_taxon_name_and_full)
  taxon_name = unique_taxon_name_and_full$TaxonName
  taxon_name_full =  unique_taxon_name_and_full$TaxonNameFull

  ###
  # 2) Setup outputs.
  ###
  taxon_match_full = rep(NA,nrow(original_report))
  taxon_name_story_full = rep(NA,nrow(original_report))
  taxon_match = rep(NA, length(taxon_name))
  taxon_name_story = taxon_name
  index_to_find_matches = 1:length(taxon_name)
  index_complete = NULL

  ###
  # 3) Match the exceptions of the known not to be in POWO.
  ###
  # (Assume all exceptions are single records in POWO, this is the case currently)
  exception_indices = match(wcvp$exceptions$plant_name_id, wcvp$wcvp_names$plant_name_id)
  exception_indices = exception_indices[!is.na(exception_indices)]
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
  taxon_name_story[index_to_find_matches[indices]] = paste0(taxon_name_story[index_to_find_matches[indices]], ' -> (Not in POWO <known not to be in POWO>)')
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
  match_info = match_mult_wcvp(taxon_name[index_to_find_matches],taxon_name_full[index_to_find_matches],  wcvp, mult_indices)
  taxon_match[index_to_find_matches] = match_info$match
  taxon_name_story[index_to_find_matches] = paste0(taxon_name_story[index_to_find_matches], match_info$message)
  index_complete = c(index_complete, index_to_find_matches[!is.na(match_info$match)])
  index_to_find_matches = index_to_find_matches[is.na(match_info$match)]

  ###
  # 7) Try to match the autonym.
  ###
  # A) Find the index of the remaining autonyms, extract the base name. Update taxon_name_story.
  remaining_autonyms_flag = unique_taxon_name_and_full$is_autonym[index_to_find_matches]
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
  match_info = match_mult_wcvp(name_to_try,taxon_name_full[autonym_indices],  wcvp, mult_indices)
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
  match_info = match_mult_wcvp(name_to_try,taxon_name_full[typo_index],  wcvp, mult_indices)
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
  taxon_match_full = taxon_match[report_match]
  taxon_name_story_full = taxon_name_story[report_match]
  return(list(match = taxon_match_full, details = taxon_name_story_full))
}
