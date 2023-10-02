#' Matching functions
#'
#' Functions used to match taxonomic names from a collection to exterior databases (POWO's WCVP, IUCN Redlist)
#'
#'  Below we outline the uses of each function. For further details and examples on matching functions please see the `Matching.Rmd` vignette.
#'
#'  Each of the matching functions generally return the index of the matching record in `enrich_database` and a message detailing how the match was obtained. These function can be used as building blocks to build a custom taxonomic name matching algorithm.
#'
#'  - `match_single()` matches `taxon_names` to `enrich_database` taking only the first match. `enrich_database_search_index` should be used to restrict the enrich database to only 'unique' taxonomic names (i.e taxonomic names that correspond to a single record in the enrich database). For 'non-unique' taxonomic names `match_multiple()` should be used.
#'
#' - `match_multiple()` matches `taxon_names` to `enrich_database` for entries in enrich database that have `non-unique` taxonomic names. For 'unique' taxonomic names `match_single()` should be used. For `non-unique` taxonomic names a matching criterion is applied to find the best match using taxonomic authors, and taxon status to decide the best match. For further details see  `Matching.Rmd` vignette. The matching criterion function is passed as the input parameter `matching_criterion` allowing customization. By default the function `get_match_from_multiple()` is used (which also depends on `match_taxon_status()`).
#'
#' - `match_all_issue()` attempts to fix hybridisation, change infraspecific levels or remove autonyms to find matches to an enriched database. This function depends on the functions:
#'
#'    - `match_rm_autonym()` attempts to find matches by removing autonyms.
#'
#'    - `match_splitter_issue()` attempts to find matches by adding/changing/removing infraspecific levels (var., f., etc).
#'
#'    - `match_hybrid_issue()` attempts to find matching  by adding/changing/removing hybrid markers (+ or x).
#'
#'    - `match_error()` Attempts to find the best match when fixing errors leads to multiple possible matches.
#'
#' - `match_typos()` attempts to find matches by searching for typos in the taxonomic name. This depends on the function:
#'
#'    - `check_taxon_typo()` to check a single taxonomic name for typos found either in a typo list or the enriched database.
#' - `no_match_cultivar_indet()` searches for cultivars and indeterminates and sets their match to `-1` indicating no match.
#'
#' - `shorten_message()` compresses matching message (details of how a match is found) into an easy to read format.
#'
#'
#' @param taxon_names Vector of taxonomic names.
#' @param taxon_authors A vector of full taxon names (corresponding to `taxon_names`)
#' @param enrich_database A data frame of enriching information we want to match `taxon_names` to.
#' @param enrich_database_search_index A vector of indices of `enrich_database` that are desired to be matched to.
#' @param single_indices A vector of indices of `enrich_database` that correspond to the records that have 'unique' taxonomic names.
#' @param mult_indices A vector of indices of `enrich_database` that correspond to the records that have 'non-unique' taxonomic names.
#' @param enrich_taxon_name_column The name of the column in `enrich_database` that corresponds to taxonomic names. Default value is `taxon_names`.
#' @param enrich_display_in_message_column The name of the column in `enrich_database` that contains values to show in the matching messages. Default value is `powo_id` (wcvp identifier).
#' @param match_column either `NA` or the name of the column in `enrich_database`. The default value if `NA` which means the values of the match are the indices of the matched records in the enrich database. If instead a single column of `enrich_database` is desired to be the result of the match the name of the column needs to be provided.
#' @param enrich_taxon_authors_column The name of the column in `enrich_database` that corresponds to the authors of taxonomic names. Default value is `taxon_authors_simp`.
#' @param enrich_taxon_author_words_column The name of the column in `enrich_database` that corresponds to the words contained in the authors of taxonomic names. Default value is `author_parts`.
#' @param enrich_database_taxon_names The taxon names taken from `enrich_database`.
#' enrich_database_taxon_names
#' @param enrich_plant_identifier_column The name of the column in `enrich_database` that corresponds to record identifier. Default value is `plant_name_id`.
#' @param enrich_database_mult `enrich_database` restricted to the rows that correspond to 'non-unique' taxonomic names.
#' @param do_add_split Flag (TRUE/FALSE) for whether we search for missing f./var./subsp.
#' @param do_fix_hybrid Flag (TRUE/FALSE) for whether we search for hybrid issues.
#' @param do_rm_autonym Flag (TRUE/FALSE) for whether we try removing autonyms.
#' @param typo_method Either `full` or `fast`, the method used for finding typos.
#' @param typo_df A data frame where the first column is a taxonomic name with a typo and the second column is the corrected taxonomic name. By default `BGSmartR::typo_list` is used.
#' @param author_match Logical vector (TRUE/FALSE) detailing whether we have a match or not.
#' @param corres_enrich_database  the corresponding entries in the `enrich_database` (rows must equal length `author match`).
#' @param current_message the current message detailing the matching steps already completed.
#' @param taxon_name_and_author the pair of taxonomic name and combined taxonomic name and author
#' @param match_details match_details
#' @param original_author The author of the taxonomic name wanted to be matched to the enriched information.
#' @param taxon_name A single taxonomic name.
#' @param show_progress Flag (TRUE/FALSE) for whether we show progress bar.
#' @param ... Arguments (i.e., attributes) used in the matching algorithm (passed along to nested fuctions). Examples include `enrich_taxon_authors_column`, `enrich_display_in_message_column` and `enrich_plant_identifier_column`.
#' @param matching_criterion The function used to find the best match when we have 'non-unique' taxonomic names. By default the function `BGSmartR::get_match_from_multiple()` is used.
#' @param messages messages detailing how a match is obtained.
#' @param console_message Flag (TRUE/FALSE) detailing whether to show messages in the console.
#' @param try_add_split Flag (TRUE/FALSE) for whether we search for missing f./var./subsp.
#' @param try_fix_hybrid Flag (TRUE/FALSE) for whether we search for hybrid issues.
#' @param try_rm_autonym Flag (TRUE/FALSE) for whether we try removing autonyms.
#' @param try_hybrid Flag (TRUE/FALSE) for whether hybrid fixes are attempted.
#'
match_single <- function(taxon_names, enrich_database, enrich_database_search_index,
                         enrich_taxon_name_column = 'taxon_name',
                         enrich_display_in_message_column = 'powo_id',
                         match_column = NA,...){

  # If no indices given return no match.
  if(length(enrich_database_search_index) == 0){
    return(list(match = rep(NA, length(taxon_names)), message = rep('', length(taxon_names))))
  }

  # A) setup
  enriched_taxon_names = enrich_database[[enrich_taxon_name_column]]
  enriched_display_in_message = enrich_database[,match(enrich_display_in_message_column, names(enrich_database))]
  message = rep('', length(taxon_names))

  # B) Perform the matching.
  match_to_single = match(taxon_names, enriched_taxon_names[enrich_database_search_index])

  # C) Find the indices of the match for both taxon names and enrich_database.
  orep_index_match = (1:length(taxon_names))[!is.na(match_to_single)]
  wcvp_index_match = enrich_database_search_index[match_to_single[!is.na(match_to_single)]]

  # D) Update message.
  message[orep_index_match] = paste0(message[orep_index_match], ' -> (matches record with single entry) -> (', enriched_display_in_message[wcvp_index_match],
                                     ', ', enriched_taxon_names[wcvp_index_match],
                                     ')')

  # E) Set the match. If NA return the index in enriched report otherwise select one column to return.
  if(is.na(match_column)){
    matched = enrich_database_search_index[match_to_single]
  }
  else{
    matched = enrich_database[,match(match_column, names(enrich_database))][enrich_database_search_index[match_to_single]]
  }


  return(list(match = matched, message = message))
}

#' @rdname match_single
#' @export
match_multiple <- function(taxon_names,taxon_authors, enrich_database, enrich_database_search_index,
                           enrich_taxon_name_column = 'taxon_name',
                           enrich_display_in_message_column = 'powo_id',
                           enrich_plant_identifier_column = 'plant_name_id',
                           match_column = NA,
                           ...,
                           show_progress = TRUE){

  #################################
  # If no indices given return no match.
  #################################
  if(length(enrich_database_search_index) == 0){
    return(list(match = rep(NA, length(taxon_names)), message = rep('', length(taxon_names))))
  }

  # A) Setup.
  match_to_multiple = rep(NA,length(taxon_names))
  message = rep('',length(taxon_names))
  enriched_plant_identifier = enrich_database[[enrich_plant_identifier_column]]
  enriched_display_in_message = enrich_database[[enrich_display_in_message_column]]
  enriched_taxon_names = enrich_database[[enrich_taxon_name_column]]

  wcvp_multiple = enrich_database[enrich_database_search_index,]
  wcvp_multiple_taxon_name = wcvp_multiple[[enrich_taxon_name_column]]


  # 1) Find which taxon names are in the restricted wcvp.
  in_wcvp = which(taxon_names %in% wcvp_multiple_taxon_name)

  # 2) Names to find matches for. (list of pairs of taxon name and taxon full)
  to_find_match = Map(c, taxon_names[in_wcvp], taxon_authors[in_wcvp])

  # 3) Find the match.
  if(show_progress){
    match_info = pbapply::pblapply(to_find_match, function(x){
      get_match_from_multiple(taxon_name_and_author = x,
                         enrich_database_mult = wcvp_multiple,
                         ...)
      })
  }
  else{
    match_info = lapply(to_find_match, function(x){
      get_match_from_multiple(taxon_name_and_author = x,
                         enrich_database_mult = wcvp_multiple,
                         ...)})
  }

  match_info_plant_identifier = unlist(lapply(match_info,function(x){x[[1]]}))
  match_info_mess = unlist(lapply(match_info,function(x){x[[2]]}))

  # 4) update match_to_multiple and message.
  match_info_match =  match(match_info_plant_identifier, enriched_plant_identifier)
  match_info_match[is.na(match_info_match)] = -2
  match_to_multiple[in_wcvp] = match_info_match

  #message if we agree to a match
  has_accept_match = match_info_match > 0
  message[in_wcvp][has_accept_match] = paste0(message[in_wcvp][has_accept_match], ' -> ', match_info_mess[has_accept_match], ' -> (',
                                              enriched_display_in_message[match_info_match[has_accept_match]], ', ',
                                              enriched_taxon_names[match_info_match[has_accept_match]],
                                              ')')
  #message if we don't agree to a match
  no_accept_match = match_info_match < 0
  message[in_wcvp][no_accept_match] = paste0(message[in_wcvp][no_accept_match], ' -> ', match_info_mess[no_accept_match])

  #Return match and message
  return(list(match = match_to_multiple, message = message))
}

#' @rdname match_single
#' @export
match_all_issue <- function(taxon_names,
                            taxon_authors = rep(NA,length(taxon_names)),
                            enrich_database,
                            single_indices = NA,
                            mult_indices = NA,
                            try_add_split = TRUE,
                            try_fix_hybrid = TRUE,
                            try_rm_autonym = TRUE,
                            enrich_taxon_name_column = 'taxon_name',
                            enrich_taxon_authors_column = 'taxon_authors_simp',
                            enrich_plant_identifier_column = 'plant_name_id',
                            ...){
  ##############################
  # 1) Setup
  ##############################
  ### 1.1) If there are no taxon_names return NULLs.
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }

  ### 1.2) If none of the methods are selected return no matches found.
  if(all(c(!try_add_split, !try_fix_hybrid, !try_rm_autonym))){
    return(list(match = rep(NA, length(taxon_names)), message = rep('', length(taxon_names)) ))
  }

  ### 1.3) Get the quantities needed from enrich_database
  enriched_taxon_names = enrich_database[[enrich_taxon_name_column]]
  enrich_taxon_authors = enrich_database[[enrich_taxon_authors_column]]
  enrich_plant_identifier = enrich_database[[enrich_plant_identifier_column]]

  ### 1.4) Setup whether we need author matching.
  if(all(is.na(taxon_authors))){
    do_taxon_author = FALSE
  }else{
    do_taxon_author = TRUE
  }

  ##############################
  # 2) Run the methods to find matches.
  ##############################
  ### 2.1) Try removing autonyms.
  if(try_rm_autonym){
    match_auto = match_rm_autonym(taxon_names, taxon_authors, enrich_database,
                                  single_indices = single_indices,
                                  mult_indices = mult_indices,
                                  enrich_taxon_name_column = enrich_taxon_name_column,
                                  enrich_plant_identifier_column = enrich_plant_identifier_column,
                                  ...)
  }else{
    match_auto = list(match = rep(NA, length(taxon_names)), message = rep('', length(taxon_names)) )
  }

  ### 2.2) Try changing/removing/adding infraspecific level (var., f., subsp.)
  if(try_add_split){
    match_splitter = match_splitter_issue(taxon_names, taxon_authors, enrich_database,
                                          enrich_taxon_name_column = enrich_taxon_name_column,
                                          enrich_plant_identifier_column = enrich_plant_identifier_column,
                                    ...)
  }else{
    match_splitter = list(match = rep(NA, length(taxon_names)), message = rep('', length(taxon_names)) )
  }

  ### 2.3) Try adding/changing/removing hybrid markers.
  if(try_fix_hybrid){
    match_hybrid = match_hybrid_issue(taxon_names, taxon_authors, enrich_database,
                                      enrich_taxon_name_column = enrich_taxon_name_column,
                                      enrich_plant_identifier_column = enrich_plant_identifier_column,
                                      ...)
  }else{
    match_hybrid = list(match = rep(NA, length(taxon_names)), message = rep('', length(taxon_names)) )

  }

  ### 2.4) Combine matches.
  matches = data.frame(autonym = match_auto$match, infra = match_splitter$match, hybrid = match_hybrid$match)
  messages = data.frame(autonym = match_auto$message, infra = match_splitter$message, hybrid = match_hybrid$message)

  ### 2.5) How many of the methods found a match.
  no_method_find_match = apply(matches, 1,function(x){sum(!is.na(x))})

  ### 2.6) Combine the names that found a match into a single string with ' OR ' as the separator.
  combined_names = rep(NA,length(taxon_names))
  for(i in which(no_method_find_match > 1)){
    auto_indices = matches[i,][!is.na(matches[i,])]
    combined_names[i] = paste0(enriched_taxon_names[auto_indices],collapse =' OR ')
  }

  ##############################
  # 3) Author Matching
  ##############################
  # If we don't have authors we decide which is the best order.
  if(!do_taxon_author){
    # Order to keep 1) Infra 2) Hybrid  3) Autonym
    match_best = matches$autonym
    match_best[!is.na(matches$hybrid)] = matches$hybrid[!is.na(matches$hybrid)]
    match_best[!is.na(matches$infra)] = matches$infra[!is.na(matches$infra)]

    message = messages$autonym
    message[messages$hybrid != ''] = messages$hybrid[messages$hybrid != '']
    message[messages$infra != ''] = messages$infra[messages$infra != '']

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
      author_auto[auto_index] = enrich_taxon_authors[match_auto$match[mult_indices][auto_index]]

      splitter_index = which(match_splitter$match[mult_indices] > 0)
      author_splitter[splitter_index] = enrich_taxon_authors[match_splitter$match[mult_indices][splitter_index]]

      hybrid_index = which(match_hybrid$match[mult_indices] > 0)
      author_hybrid[hybrid_index] = enrich_taxon_authors[match_hybrid$match[mult_indices][hybrid_index]]

      authors = data.frame(original = taxon_authors[mult_indices],autonym = author_auto, infra = author_splitter, hybrid = author_hybrid)


      #Choose the best records according to author matching.
      # This returns the list of match and message. $match is a logical vector of length 3 which is true is the given method (autonym, infra, hybrid) found a match by best author matching. $message details the best level of author matching.
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
        corres_enrich_database = enrich_database[as.numeric(matches[mult_indices[i],!is.na(matches[mult_indices[i],])]),]
        corres_match_cur = author_choose[[i]]$match[!is.na(matches[mult_indices[i],])]
        match_cur = match_taxon_status(author_match = corres_match_cur,
                                       corres_enrich_database = corres_enrich_database,
                                       current_message = paste0('-> ',author_choose[[i]]$message, ' -> '))
        #Note that match_taxon_status returns the plant_name_id and not the row number of the match
        matches_cur[i] = match(match_cur[[enrich_plant_identifier_column]], enrich_plant_identifier)
        messages_cur[i] = match_cur$message
      }

      # If the match == -2 use the method in our preferred order.
      no_match = is.na(matches_cur)
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

#' @rdname match_single
#' @export
match_typos <- function(taxon_names, taxon_authors, enrich_database,
                        enrich_taxon_name_column = 'taxon_name',
                        single_indices = NA,
                        mult_indices = NA,
                        typo_method = 'fast', ...){
  enriched_taxon_names = enrich_database[,match(enrich_taxon_name_column, names(enrich_database))]

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
    fixed_typo = unlist(pbapply::pblapply(taxon_names, function(x){check_taxon_typo(x,NA, typo_method = 'fast')}))
  }
  else{
    wcvp_without_repeated = enrich_database[match(unique(enriched_taxon_names),
                                                  enriched_taxon_names),]
    fixed_typo = unlist(pbapply::pblapply(taxon_names, function(x){check_taxon_typo(x,wcvp_without_repeated, typo_method = 'full')}))

  }

  # Get index of taxons found to have typos.
  typo_indices =  which(!is.na(fixed_typo))

  # If no typos return.
  if(length(typo_indices) == 0){
    return(list(match = out_match, message = out_message))
  }

  if(is.na(single_indices[1])){
    single_indices = which(enrich_database$single_entry == TRUE)
  }
  if(is.na(mult_indices[1])){
    mult_indices = which(enrich_database$single_entry == FALSE)
  }

  name_to_try = fixed_typo[typo_indices]

  ########################
  # Match to single.
  ########################
  match_info = match_single(name_to_try, enrich_database, single_indices, ...)
  out_match[typo_indices] = match_info$match
  out_message[typo_indices] = paste0(out_message[typo_indices], match_info$message)
  index_complete = typo_indices[!is.na(match_info$match)]
  index_to_find_matches = which(is.na(match_info$match))

  ########################
  # Match to multiple.
  ########################
  if(length(index_to_find_matches) > 0){
    typo_index = typo_indices[index_to_find_matches]
    match_info = match_multiple(name_to_try[index_to_find_matches], taxon_authors[typo_index],  enrich_database, mult_indices, ...)
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

#' @rdname match_single
#' @export
no_match_cultivar_indet <- function(taxon_names){
  #Setup output
  out_match = rep(NA,length(taxon_names))
  out_message = rep('',length(taxon_names))

  #Find indices of those known to be cultivars or indeterminates.
  not_in_wcvp = which(grepl(" sp\\.| gx |'.*?'|\\[|^Indet| gx|indet$|CV|cv$|cv\\.|Group|unkn|hybrid$|Hybrid |Unknown",taxon_names))

  # For cultivars and indeterminates set the match to -1 and create message.
  out_match[not_in_wcvp] = -1
  out_message[not_in_wcvp] = ' -> (Cultivar or Indeterminate <Do not attempt matching>)'

  return(list(match = out_match, message = out_message))
}

#' @rdname match_single
#' @export
match_rm_autonym <- function(taxon_names, taxon_authors, enrich_database,
                             single_indices = NA,
                             mult_indices = NA,
                             ...){

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
    single_indices = which(enrich_database$single_entry == TRUE)
  }
  if(is.na(mult_indices[1])){
    mult_indices = which(enrich_database$single_entry == FALSE)
  }

  name_to_try = unlist(lapply(taxon_names[autonym_indices], function(name){
    split_name = stringr::str_split(name,' var\\. | subsp\\. | f\\. | ssp\\. | nothosubsp\\. ')[[1]]
    split_name = unlist(lapply(split_name, stringr::str_squish))
    return(split_name[1])
  }))

  ########################
  # Match to single.
  ########################
  match_info = match_single(name_to_try, enrich_database, single_indices,...)
  out_match[autonym_indices] = match_info$match
  out_message[autonym_indices] = paste0(out_message[autonym_indices], match_info$message)
  index_complete = autonym_indices[!is.na(match_info$match)]
  index_to_find_matches = which(is.na(match_info$match))

  ########################
  # Match to multiple.
  ########################
  if(length(index_to_find_matches) > 0){
    auto_index = autonym_indices[index_to_find_matches]
    args = list(...)
    args$taxon_names = name_to_try[index_to_find_matches]
    args$taxon_authors = taxon_authors[auto_index]
    args$enrich_database = enrich_database
    args$enrich_database_search_index = mult_indices
    # print(args)
    match_info = do.call(match_multiple, args)
    # match_info = match_multiple(taxon_names = name_to_try[index_to_find_matches],
    #                             taxon_authors = taxon_authors[auto_index],
    #                             enrich_database = enrich_database,
    #                             enrich_database_search_index = mult_indices,
    #                             ...)
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

#' @rdname match_single
#' @export
match_splitter_issue <- function(taxon_names, taxon_authors, enrich_database,
                                 enrich_taxon_name_column = 'taxon_name', ...){

  enriched_taxon_names = enrich_database[,match(enrich_taxon_name_column, names(enrich_database))]
  # We know from exploring POWO that var/f/subsp only occurs after the genus species. (with the potential addition of 'x' or '+' for hybrids)
  #Check for NA in taxon_names and remove if they exist.
  NAs = which(is.na(taxon_names))
  if(length(NAs) > 1){
    warning('In match_splitter_issue(), taxon names contain NA.')
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
  wcvp_index_splitters = which(grepl(splitters_grepl, enriched_taxon_names))
  wcvp_index_splitters_mult = wcvp_index_splitters[enrich_database$single_entry[wcvp_index_splitters] == F]
  wcvp_index_splitters_single = wcvp_index_splitters[enrich_database$single_entry[wcvp_index_splitters] == T]

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
      match_details_single = match_single(taxon_names = x[[1]],
                                          enrich_database = enrich_database,
                                          enrich_database_search_index = wcvp_index_splitters_single,
                                          ...)
      match_details_mult = match_multiple(taxon_names = x[[1]],
                                          taxon_authors = rep(x[[2]],8),
                                          enrich_database = enrich_database,
                                          enrich_database_search_index = wcvp_index_splitters_mult,
                                          ...)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  enrich_database = enrich_database,
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
    wcvp_index_splitters_mult_h = wcvp_index_splitters_mult[grepl('\u00D7|\\+',enriched_taxon_names[wcvp_index_splitters_mult])]
    wcvp_index_splitters_single_h = wcvp_index_splitters_single[grepl('\u00D7|\\+',enriched_taxon_names[wcvp_index_splitters_single])]

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
      match_details_single = match_single(x[[1]], enrich_database, wcvp_index_splitters_single_h, ...)
      match_details_mult = match_multiple(x[[1]], rep(x[[2]],4), enrich_database, wcvp_index_splitters_mult_h, ...,  show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  enrich_database = enrich_database,
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
      match_details_single = match_single(x[[1]], enrich_database, wcvp_index_splitters_single, ...)
      match_details_mult = match_multiple(x[[1]], rep(x[[2]],3), enrich_database, wcvp_index_splitters_mult, ...,  show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  enrich_database = enrich_database,
                                  current_message = ' -> (Infrageneric level update) -> ')
      return(chosen_record)

    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_with_splitter] = as.numeric(match_info[,1])
    out_message[index_words_with_splitter] = match_info[,2]

  }


  return(list(match = out_match, message = out_message))
}

#' @rdname match_single
#' @export
match_hybrid_issue <- function(taxon_names, taxon_authors, enrich_database,
                               enrich_taxon_name_column = 'taxon_name', ...){

  # We know from exploring POWO that var/f/subsp only occurs after the genus species. (with the potential addition of 'x' or '+' for hybrids)
  enriched_taxon_names = enrich_database[,match(enrich_taxon_name_column, names(enrich_database))]

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
  wcvp_index_hybrid = which(grepl('\u00D7|\\+', enriched_taxon_names))
  wcvp_index_hybrid_mult = wcvp_index_hybrid[enrich_database$single_entry[wcvp_index_hybrid] == F]
  wcvp_index_hybrid_single = wcvp_index_hybrid[enrich_database$single_entry[wcvp_index_hybrid] == T]

  wcvp_index_mult = which(enrich_database$single_entry == F)
  wcvp_index_single = which(enrich_database$single_entry == T)

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

      match_details_single = match_single(x[[1]], enrich_database, wcvp_index_hybrid_single, ...)
      match_details_mult = match_multiple(x[[1]], rep(x[[2]],2), enrich_database, wcvp_index_hybrid_mult, ...,  show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  enrich_database = enrich_database,
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
      match_details_single = match_single(x[[1]], enrich_database, wcvp_index_hybrid_single, ...)
      match_details_mult = match_multiple(x[[1]], rep(x[[2]],4), enrich_database, wcvp_index_hybrid_mult, ...,  show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  enrich_database = enrich_database,
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
      match_details_single = match_single(x[[1]], enrich_database , wcvp_index_single, ...)
      match_details_mult = match_multiple(x[[1]], rep(x[[2]],3), enrich_database, wcvp_index_mult, ...,  show_progress = FALSE)
      match_details = list(match = c(match_details_single$match, match_details_mult$match),
                           message = c(match_details_single$message, match_details_mult$message))

      chosen_record = match_error(taxon_names = x[[1]],
                                  match_details = match_details,
                                  original_author = x[[2]],
                                  enrich_database = enrich_database,
                                  current_message = ' -> (Hybrid fix) -> ')
      return(chosen_record)
    }))
    match_info = data.frame(matrix(match_info, ncol =2, byrow = T))

    out_match[index_words_with_hybrid] = as.numeric(match_info[,1])
    out_message[index_words_with_hybrid] = match_info[,2]


  }


  return(list(match = out_match, message = out_message))
}

#' @rdname match_single
#' @export
match_taxon_status <- function(author_match, corres_enrich_database, current_message = '',...){
  author_match[is.na(author_match)] = FALSE # NA matches go to FALSE. This occurs when POWO author = NA.

  match_flag = FALSE
  if(sum(author_match) == 0){
    matched = NA
    message = ''
    match_flag = FALSE
  }
  # Exactly one match return it.
  else if(sum(author_match) == 1){
    matched = corres_enrich_database$plant_name_id[author_match]
    message = '(single match)'
    match_flag = TRUE
  }
  else if(sum(author_match) > 1){ # > 1 match
    message = ' > 1 match, '
    corres_enrich_database = corres_enrich_database[author_match,]
    accepted_plant_id = corres_enrich_database$accepted_plant_name_id

    # Check if all exact matches point to the same accepted name
    if(identical(accepted_plant_id, rep(accepted_plant_id[1], length(accepted_plant_id)))){
      # match to accepted if one exists if not the first plant that matches.
      taxon_accepted = corres_enrich_database$taxon_status == 'Accepted'
      if(any(taxon_accepted)){
        matched = corres_enrich_database$plant_name_id[taxon_accepted][1]
        message = paste0('(', message, 'all point to same accepted plant',')',collapse = '')
      }
      else{
        matched = corres_enrich_database$plant_name_id[1]
        message = paste0('(', message, 'all point to same accepted plant',')',collapse = '')

      }
      match_flag = TRUE
    }

    # Check for differences in taxon_status.
    if(!match_flag){
      taxon_status = corres_enrich_database$taxon_status
      taxon_status_match = match(taxon_status , c('Accepted', 'Synonym'))
      if(all(is.na(taxon_status_match))){
        matched = -2
        message = paste0('(', message, 'no accepted or synonym',')',collapse = '')
        match_flag = TRUE
      }
      else{
        chosen_record = which(taxon_status_match == min(taxon_status_match, na.rm = T))
        if(length(chosen_record) == 1){
          matched = corres_enrich_database$plant_name_id[chosen_record]
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

#' @rdname match_single
#' @export
get_match_from_multiple <- function(taxon_name_and_author, enrich_database_mult,
                                    matching_criterion = BGSmartR::additional_wcvp_matching,
                                    enrich_plant_identifier_column = 'plant_name_id',
                                    enrich_taxon_name_column = 'taxon_name',
                                    enrich_taxon_authors_column = 'taxon_authors_simp',
                                    enrich_taxon_author_words_column = 'author_parts',...){
  ##############################
  # 1) Setup
  ##############################
  ### 1.1) Separate taxon names and taxon author, get enriched taxon names.
  enriched_taxon_names = enrich_database_mult[[enrich_taxon_name_column]]
  taxon_name_current = taxon_name_and_author[1]
  taxon_author_current = taxon_name_and_author[2]
  current_message = '(Multiple records in enriched database) -> '

  ### 1.2) Create `try_author_match` flag to check whether we have author information needed for author matching.
  try_author_match = TRUE
  if(is.na(taxon_author_current) || taxon_author_current == ''){
    try_author_match = FALSE
  }

  ### 1.3) Get the corresponding records in enrich_database_mult.
  enriched_cur = enrich_database_mult[enriched_taxon_names == taxon_name_current,]
  enrich_taxon_authors_cur = enriched_cur[[enrich_taxon_authors_column]]
  enrich_taxon_authors_words_cur = enriched_cur[[enrich_taxon_author_words_column]]

  ### 1.4) If there are no corresponding records in enrich_database_mult (nrow(enriched_cur) = 0) then return no match.
  if(nrow(enriched_cur) == 0){
    list(plant_identifer = -2, message = '')
  }


  ##############################
  # 2) Author Matching (reducing enriched_cur to the best matches)
  ##############################
  if(try_author_match){
    # Get the author matches.
    matched_by_authors = match_authors(collection_author = taxon_author_current,
                  enriched_database_authors = enrich_taxon_authors_cur,
                  ...)

    # Reduce enriched_cur by author matching.
    enriched_cur = enriched_cur[matched_by_authors$wanted,]
    current_message = paste0(current_message, matched_by_authors$message, collapse =' ')

    # If enriched_cur only has one row then we have found the best match and no further matching required.
    if(nrow(enriched_cur) == 1){
      return(list(plant_identifer = enriched_cur[[enrich_plant_identifier_column]], message = current_message))
    }
  }
  else{
    current_message = paste0(current_message, '(No authors) ->', collapse =' ')
  }

  ##############################
  # 3) Criterion Matching (dependent on enrich_database)
  ##############################
  match_by_criterion = matching_criterion(enriched_cur)
  current_message = paste0(current_message, match_by_criterion$message, collapse =' ')
  # If matching_criterion finds a best match the corresponding row in enriched_cur is returned otherwise the row is set to -2, to denote no match.
  if(length(match_by_criterion$row) == 1){
    enriched_cur = enriched_cur[match_by_criterion$row,]
    return(list(plant_identifer = enriched_cur[[enrich_plant_identifier_column]], message = current_message))
  }

  return(list(plant_identifer = -2, message = current_message))

}



#' @rdname match_single
#' @export
match_error <- function(taxon_names, match_details, original_author, enrich_database, current_message ='',...){
  # A) Index of the matches we found.
  found_match_index = which(!is.na(match_details$match))
  if(length(found_match_index) == 0){
    return(c(NA,''))
  }

  match_detail = list(match = match_details$match[found_match_index],
                      message = match_details$message[found_match_index],
                      taxon_name = rep(taxon_names,2)[found_match_index])


  # If we get a single match return it.
  if(length(match_detail$match)==1){
    match = match_detail$match
    message = paste0(current_message, match_detail$taxon_name, match_detail$message)
    return(c(match,message))
  }

  # Multiple matches currently don't know which is best.
  if(length(found_match_index)>1){
    # Does a match have better authors?
    match_authors = enrich_database$taxon_authors_simp[match_detail$match]
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
                                     corres_enrich_database = enrich_database[match_detail$match,],
                                     current_message = '(Multiple attempted fixed taxon names match) -> (Cannot choose record by author match) -> ')
    #Note that the match provided by match_taxon_status is the plant_name_id and not the row number in wcvp. So we need to convert.
    if(!is.na(taxon_match$match)){
      match_index = match(taxon_match$match, enrich_database$plant_name_id)
    }
    match = match_index
    message = paste0(current_message, taxon_match$message)

    return(c(match,message))
  }

}




#' @rdname match_single
#' @export
check_taxon_typo <- function(taxon_name, enrich_database = NA,
                             enrich_taxon_name_column = 'taxon_name',
                             typo_df = BGSmartR::typo_list,
                             typo_method = 'fast',...){
  ########################
  # 1) Return NA for non-words and special characters
  ########################
  if(is.null(taxon_name))(return(NA))
  if(is.na(taxon_name)){return(NA)}
  if(taxon_name ==''){return(NA)}
  if(grepl('\\(|\\)|\\*|\\?|\\$|\\^',taxon_name)){return(NA)} # Since no words in wcvp have '(',')', '?' or '*', '$', '^' we return NA.

  ########################
  # 2) Check if the typo is in typo list.
  ########################
  match_to_typo = match(taxon_name,typo_df[,1])
  if(!is.na(match_to_typo)){
    return(typo_df[match_to_typo,2])
  }
  #If we only want to check the typo list return NA for non-matches at this stage.
  if(typo_method == 'fast'){
    return(NA)
  }

  ########################
  # 3) reduce the wcvp names to check. And split into three vectors for same length, one less and one more.
  ########################
  enriched_taxon_names = enrich_database[,match(enrich_taxon_name_column, names(enrich_database))]
  #     A) Make sure the wcvp names are either the same length or one extra character.
  length_taxon = stringr::str_length(taxon_name)
  wcvp_needed = enrich_database[enrich_database$taxon_length %in% c(length_taxon-1, length_taxon, length_taxon+1),]
  #     B) Make sure we only look at taxons which contain only one word with a change (after removing words with '.' or '+')
  words = stringr::str_split(taxon_name,' ')[[1]]
  words = words[!grepl('\\.|\\+|\u00D7', words)]
  pat = paste0(words,collapse = '|')

  wcvp_needed = wcvp_needed[grepl(pat, enriched_taxon_names),]
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
    if(stringr::str_ends(taxon_name,final_letter_change[i,1])){
      fixed = wcvp_needed$taxon_name[wcvp_needed$taxon_name == stringr::str_replace(taxon_name,paste0(final_letter_change[i,1],'$',collapse = ''),final_letter_change[i,2])]
      if(length(fixed) >0){
        return(fixed[1])
      }
    }
  }

  # b) Common none simple letter swap.
  letter_change = matrix(c('i','ae'),byrow = T, ncol = 2)
  for(i in 1:nrow(letter_change)){
    locations = stringr::str_locate_all(taxon_name, letter_change)
    for(j in 1:length(locations)){
      current = locations[[j]]
      if(nrow(current) > 0){
        for(k in 1:nrow(current)){
          # original letter in middle.
          if(current[k,1] == 1){
            new_name = paste0(letter_change[i,3-j],
                              stringr::str_sub(taxon_name,current[k,2]+1,-1))
          }
          #original letter at end.
          else if(current[k,2] == length(taxon_name)){
            new_name = paste0(stringr::str_sub(taxon_name,1,current[k,1]-1),
                              letter_change[i,3-j])
          }
          #original letter in middle
          else{
            new_name = paste0(stringr::str_sub(taxon_name,1,current[k,1]-1),
                              letter_change[i,3-j],
                              stringr::str_sub(taxon_name,current[k,2]+1,-1))
          }

          fixed = wcvp_needed$taxon_name[wcvp_needed$taxon_name == new_name]
          if(length(fixed) >0){
            return(fixed[1])
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
    patternA = paste0(stringr::str_sub(taxon_name,1,i),'[a-zA-Z]',stringr::str_sub(taxon_name,i+2,length_taxon))
    patternA = stringr::str_replace_all(patternA,'\\.','\\\\.')
    fixed_typoA = wcvp_needed_same[grepl(patternA, wcvp_needed_same)]
    if(length(fixed_typoA) >0){
      return(fixed_typoA[1])
    }

    #Check adding a single new letter
    patternB = paste0(stringr::str_sub(taxon_name,1,i),'[a-zA-Z-]',stringr::str_sub(taxon_name,i+1,length_taxon))
    patternB = stringr::str_replace_all(patternB,'\\.','\\\\.')
    fixed_typoB = wcvp_needed_plus_1[grepl(patternB, wcvp_needed_plus_1)]
    if(length(fixed_typoB) >0){
      return(fixed_typoB[1])
    }
    #Check removing a single new letter
    patternC = paste0(stringr::str_sub(taxon_name,1,i),stringr::str_sub(taxon_name,i+2,length_taxon))
    patternC = stringr::str_replace_all(patternC,'\\.','\\\\.')
    fixed_typoC = wcvp_needed_minus_1[grepl(patternC, wcvp_needed_minus_1)]
    if(length(fixed_typoC) >0){
      return(fixed_typoC[1])
    }
  }

  # If no typo is found by this stage we haven't managed to find a fix and return NA.
  return(NA)
}


#' @rdname match_single
#' @export
shorten_message <- function(messages){
  match_short = rep('', length(messages))
  match_options = c("(matches POWO record with single entry)", 'EXACT',
                    "(Exact author match)", 'EXACT',
                    "all point to same accepted plant", 'EXACT',
                    "Partial author", 'PARTIAL',
                    "choose via taxon_status", "TAXON_STATUS",
                    "multiple best taxon status, do not match", "UNCLEAR",
                    "no accepted or synonym", "UNCLEAR",
                    "(Cultivar or Indeterminate <Do not attempt matching>)", 'CULT/INDET',
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

#' @rdname match_single
#' @export
match_all_issue_new <- function(taxon_names,
                                taxon_authors = rep(NA,length(taxon_names)),
                                enrich_database,
                                matching_criterion = BGSmartR::additional_wcvp_matching,
                                do_add_split = TRUE,
                                do_fix_hybrid = TRUE,
                                do_rm_autonym = TRUE,
                                enrich_taxon_name_column = 'taxon_name',
                                enrich_taxon_authors_column = 'taxon_authors_simp',
                                enrich_plant_identifier_column = 'plant_name_id',
                                ...){

  ##############################
  # 1) Setup
  ##############################
  ### 1.1) If there are no taxon_names return NULLs.
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }

  ### 1.2) If none of the methods are selected return no matches found.
  if(all(c(!do_add_split, !do_fix_hybrid, !do_rm_autonym))){
    return(list(match = rep(NA, length(taxon_names)), message = rep('', length(taxon_names)) ))
  }

  ### 1.3) Get the quantities needed from enrich_database
  enriched_taxon_names = enrich_database[[enrich_taxon_name_column]]

  ### 1.4) Setup whether we need author matching.
  if(all(is.na(taxon_authors))){
    do_taxon_author = FALSE
  }else{
    do_taxon_author = TRUE
  }

  ##############################
  # 2) Find potential fixed taxonomic names.
  ##############################
  ### 2.1) Try removing autonyms.
  if(do_rm_autonym){
    fix_auto = try_rm_autonym(taxon_names = taxon_names,
                              enrich_database_taxon_names = enrich_database[[enrich_taxon_name_column]],
                              ...)
  }else{
    fix_auto = rep('',length(taxon_names))
  }

  ### 2.2) Try changing/removing/adding infraspecific level (var., f., subsp.)
  if(do_add_split){
    fix_splitter = try_fix_infraspecific_level(taxon_names = taxon_names,
                                               enrich_database_taxon_names = enrich_database[[enrich_taxon_name_column]],
                                               ...)
  }else{
    fix_splitter =  rep('',length(taxon_names))
  }

  ### 2.3) Try adding/changing/removing hybrid markers.
  if(do_fix_hybrid){
    fix_hybrid = try_fix_hybrid(taxon_names = taxon_names,
                                enrich_database_taxon_names = enrich_database[[enrich_taxon_name_column]],
                                ...)
  }else{
    fix_hybrid =  rep('',length(taxon_names))

  }

  ### 2.4) Combine fixed names.
  names_to_try = paste0(fix_auto, ' OR ',fix_splitter, ' OR ',fix_hybrid)
  names_to_try = stringr::str_replace_all(names_to_try, pattern = ' OR  OR ', ' OR ') # clean missing values
  names_to_try = stringr::str_replace_all(names_to_try, pattern = '^ OR | OR $', '') # clean missing values


  ##############################
  # 3) Find best match out of fixed names.
  ##############################
  to_try = data.frame(taxon_names = names_to_try, authors = taxon_authors)
  ### 3.1) Loop over all names_to_try.
  counter = 0
  matches = apply(to_try, 1, function(name_author){
    counter <<- counter +1
    tax_names = name_author[1]
    tax_author = name_author[2]
    current_message = ''

    ### 3.2) if names == '' return no match.
    if(tax_names == ''){
      return(list(match = NA, message = ''))
    }
    current_message = paste0(current_message, '(Try Fixing taxomonic name) -> ',collapse =' ')
    current_message = paste0(current_message, tax_names, ' -> ',collapse =' ')


    ### 3.3) Extract possible matches from enriched database.
    split_names = stringr::str_split(tax_names, pattern = ' OR ')[[1]]
    enriched_cur = enrich_database[enriched_taxon_names %in% split_names,]

    ### 3.4) If only a single record return it.
    if(nrow(enriched_cur) == 1){
      current_message = paste0(current_message, ' (single fixed record) -> ',collapse =' ')

      return(list(plant_identifer = enriched_cur[[enrich_plant_identifier_column]], message = current_message))
    }

     enrich_taxon_authors_cur = enriched_cur[[enrich_taxon_authors_column]]
    ### 3.4) Author matching.
    if(do_taxon_author){
      # Get the author matches.
      matched_by_authors = match_authors(collection_author = tax_author,
                                         enriched_database_authors = enrich_taxon_authors_cur,
                                         ...)

      # Reduce enriched_cur by author matching.
      enriched_cur = enriched_cur[matched_by_authors$wanted,]
      current_message = paste0(current_message, matched_by_authors$message, collapse =' ')

      # If enriched_cur only has one row then we have found the best match and no further matching required.
      if(nrow(enriched_cur) == 1){
        return(list(plant_identifer = enriched_cur[[enrich_plant_identifier_column]], message = current_message))
      }
    }
    else{
      current_message = paste0(current_message, '(No authors) -> ', collapse =' ')
    }

    ### 3.5) Matching criterion. (dependent on enrich_database)
    match_by_criterion = matching_criterion(enrich_database_extract = enriched_cur, message = '')
    enriched_cur = enriched_cur[match_by_criterion$row,]
    current_message = paste0(current_message, match_by_criterion$message, collapse =' ')
    if(nrow(enriched_cur) == 1){
      return(list(plant_identifer = enriched_cur[[enrich_plant_identifier_column]], message = current_message))
    }

    ### 3.6) Match by method used.(Fix splitter > fix hybrid < remove autonym)
    remaining_taxon_names = enriched_cur[[enrich_taxon_name_column]]
    best_method = unlist(lapply(remaining_taxon_names, function(x){
      if(grepl(x, fix_splitter[counter])){
        return(1)
      }
      if(grepl(x, fix_hybrid[counter])){
        return(2)
      }
      if(grepl(x, fix_auto[counter])){
        return(3)
      }
    }))
    min_best_method = min(best_method,na.rm = T)
    enriched_cur = enriched_cur[which(best_method == min_best_method),]

    if(nrow(enriched_cur) == 1){
      message = '(Decide on Method: Remove Autonym) -> '
      if(min_best_method == 2){message = '(Decide on Method: Fix hybrid) -> '}
      if(min_best_method == 1){message = '(Decide on Method: Fix infraspecific level) -> '}
      current_message = paste0(current_message, message, collapse =' ')
      return(list(plant_identifer = enriched_cur[[enrich_plant_identifier_column]], message = current_message))
    }

    current_message = paste0(current_message, '(Cannot decide via fixing method) -> ', collapse =' ')

    # No method can find the single best record match.
    return(list(plant_identifer = -2, message = current_message))

  })

  counter = 0
  m = unlist(lapply(matches, function(x){x[[1]]}))
  mess = unlist(lapply(matches, function(x){x[[2]]}))

  ### Convert enrich_plant_identifier_column to index in enrich database.
  matched = rep(NA, length(taxon_names))
  matched =match(m, enrich_database[[enrich_plant_identifier_column]])
  matched[which(m == -2)] = -2

  return(list(match = matched, message = mess))
}

#' @rdname match_single
#' @export
try_rm_autonym <- function(taxon_names, enrich_database_taxon_names,
                           console_message = TRUE, ...
){
  ##############################
  # 1) Setup
  ##############################
  ### 1.1) Check for NA in taxon names and give warning if there are.
  if(any(is.na(taxon_names))){
    warning('In try_rm_autonym(), taxon names contain NA.')
  }

  ### 1.2) If no taxon_names provided return NULLs.
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }

  if(console_message){
    cli::cli_alert_info(text = "Trying removing autonyms from taxon names")
  }

  ##############################
  # 2) Find which taxon_names are autonyms and create vector of taxon names with autonym removed that are in enrich database.
  ##############################
  ### 2.1) As is_autonym column to the taxon_names.
  taxon_names_and_autonym = add_is_autonym(data.frame(TaxonName = taxon_names))
  autonym_indices =  which(taxon_names_and_autonym$is_autonym)

  ### 2.2) If there exist no_autonyms exist function.
  if(length(autonym_indices) == 0){
    return(rep('',length(taxon_names)))
  }

  ### 2.3) extract the base of the autonyms and save in autonym_base.
  autonym_base = rep(NA,length(taxon_names))
  autonym_base[autonym_indices] = stringr::str_extract(string = taxon_names[autonym_indices],
                                                       pattern = '^.*?(?= var\\. | subsp\\. | f\\. | ssp\\. | nothosubsp\\. )')


  ### 2.4) Reduce autonym base to NA, if the base is not found in enrich_database.
  autonym_base[!autonym_base %in% enrich_database_taxon_names] = ''

  return(autonym_base)
}

#' @rdname match_single
#' @export
try_fix_infraspecific_level <- function(taxon_names, enrich_database_taxon_names,
                                        try_hybrid = TRUE,
                                        console_message = TRUE,
                                        ...){
  ##############################
  # 1) Setup
  ##############################
  ### 1.1) Check for NA in taxon names and give warning if there are.
  if(any(is.na(taxon_names))){
    warning('In try_rm_autonym(), taxon names contain NA.')
  }

  ### 1.2) If no taxon_names provided return NULLs.
  if(length(taxon_names) == 0){
    return(list(match = NULL, message = NULL))
  }

  ### 1.3) Define the infraspecific levels.
  splitters = c('subsp.', 'var.', 'f.', 'nothosubsp.')
  splitters_grepl = ' subsp\\. | var\\. | f\\. | nothosubsp\\. '

  ### 1.4) Find how many word each taxon_name has.
  no_words = stringr::str_count(taxon_names, ' ')+1

  ### 1.5) Define output variable
  out_fixed_names = rep('',length(taxon_names))

  ### 1.6) Set lapply to pbapply::pblapply to show console progression.
  if(console_message){
    lapply = pbapply::pblapply
  }

  ### 1.7) Get reduced enriched taxon names that contain infraspecific levels.
  enrich_taxon_names_w_split = enrich_database_taxon_names[grepl(splitters_grepl,enrich_database_taxon_names)]



  ##############################
  # 2) Try adding a infraspecific level. (taxon name needs 3 words)
  ##############################
  ### 2.1) Get the indices of taxon_names with 3 words
  index_words_3 = which(no_words == 3 & !grepl('\u00D7|\\+',taxon_names))

  if(length(index_words_3)>0){
    if(console_message){
      cli::cli_alert_info("Trying adding infraspecific level to {length(index_words_3)} name{?s}")
    }
    ### 2.2) Get the taxon names to try with adding infraspecific (/hybrid) markers. whilst checking if in enrich_database.
    words_3 = stringr::str_split(taxon_names[index_words_3], ' ')
    if(try_hybrid){
      new_taxon_names = unlist(lapply(words_3, function(x){
        just_splitter = paste(x[1], x[2], splitters, x[3])
        taxon_namesA = just_splitter[just_splitter %in% enrich_taxon_names_w_split]
        hybrid_and_splitter =  paste(x[1], '\u00D7', x[2], splitters, x[3])
        taxon_namesB = hybrid_and_splitter[hybrid_and_splitter %in% enrich_taxon_names_w_split]
        new_taxon_names = c(taxon_namesA,taxon_namesB)
        return(paste0(new_taxon_names, collapse = ' OR '))
      }))

    }
    else{
      new_taxon_names = unlist(lapply(words_3, function(x){
        just_splitter = paste(x[1], x[2], splitters, x[3])
        new_taxon_names = just_splitter[just_splitter %in% enrich_taxon_names_w_split]

        return(paste0(new_taxon_names, collapse = ' OR '))
      }))

    }
    out_fixed_names[index_words_3] = paste0(out_fixed_names[index_words_3], new_taxon_names, sep = '')
  }


  ##############################
  # 3) Try adding a infraspecific level when hybrid marker exists. (taxon name needs 4 words)
  ##############################
  if(try_hybrid){
    ### 3.1) Get the indices of taxon_names with 4 words and hybrid in the best position.
    index_words_4 = which(no_words == 4 & grepl('\u00D7|\\+',taxon_names))
    hybrid_position = unlist(lapply(stringr::str_split(taxon_names[index_words_4], ' '),function(x){which(grepl('\u00D7|\\+',x))}))
    index_words_4 = index_words_4[hybrid_position %in% c(1,2)]

    if(length(index_words_4)>0){
      if(console_message){
        cli::cli_alert_info("Trying adding infraspecific level (taxon with hybrid markers) to {length(index_words_4)} name{?s}")
      }
      ### 3.2) Get the taxon names to try with adding infraspecific (/hybrid) markers. whilst checking if in enrich_database.
      words_4 = stringr::str_split(taxon_names[index_words_4], ' ')
      new_taxon_names = unlist(lapply(words_4, function(x){
        just_splitter = paste(x[1], x[2], x[3], splitters, x[4])
        new_taxon_names = just_splitter[just_splitter %in% enrich_taxon_names_w_split]

        return(paste0(new_taxon_names, collapse = ' OR '))
      }))

      out_fixed_names[index_words_4] = paste0(out_fixed_names[index_words_4], new_taxon_names, sep = '')
    }


  }

  ##############################
  # 4) Try changing an infraspecific level.
  ##############################
  ### 4.1) Get the indices of taxon_names with 3 words
  index_words_with_splitter = which(grepl(splitters_grepl, taxon_names))

  if(length(index_words_with_splitter) >0){
    if(console_message){
      cli::cli_alert_info("Trying changing infraspecific level to {length(index_words_with_splitter)} name{?s}")
    }
    new_taxon_names = unlist(lapply(taxon_names[index_words_with_splitter], function(x){
      A=stringr::str_replace(x,pattern = splitters_grepl, ' var\\. ')
      B=stringr::str_replace(x,pattern = splitters_grepl, ' f\\. ')
      C=stringr::str_replace(x,pattern = splitters_grepl, ' subsp\\. ')
      D=stringr::str_replace(x,pattern = splitters_grepl, ' nothosubsp\\. ')
      options = c(A,B,C,D)
      new_taxon_names = options[-match(x, options)]

      new_taxon_names = new_taxon_names[new_taxon_names %in% enrich_taxon_names_w_split]

      return(paste0(new_taxon_names, collapse = ' OR '))
    }))
    out_fixed_names[index_words_with_splitter] = paste0(out_fixed_names[index_words_with_splitter], new_taxon_names, sep = '')
  }

  return(out_fixed_names)
}

#' @rdname match_single
#' @export
try_fix_hybrid <- function(taxon_names, enrich_database_taxon_names,
                           try_hybrid = TRUE,
                           console_message = TRUE,
                           ...){
  ##############################
  # 1) Setup
  ##############################
  ### 1.1) Check for NA in taxon names and give warning if there are.
  if(any(is.na(taxon_names))){
    warning('In try_rm_autonym(), taxon names contain NA.')
  }

  ### 1.2) If no taxon_names provided return NULLs.
  if(length(taxon_names) == 0){
    return(NULL)
  }

  ### 1.2) If we don't want to fix hybrid things.
  if(!try_hybrid){
    return(rep('',length(taxon_names)))
  }

  ### 1.3) Set lapply to pbapply::pblapply to show console progression.
  if(console_message){
    lapply = pbapply::pblapply
  }

  ### 1.4) Define the hybrid markers
  hybrids = c('\u00D7', '+')
  hybrids_grepl = ' \u00D7 | \\+  '

  ### 1.5) Find how many word each taxon_name has.
  no_words = stringr::str_count(taxon_names, ' ')+1

  ### 1.6) Define output variable
  out_fixed_names = rep('',length(taxon_names))

  ### 1.7) Get reduced enriched taxon names that contain hybrids.
  enrich_taxon_names_w_hybrid = enrich_database_taxon_names[grepl(hybrids_grepl,enrich_database_taxon_names)]

  ##############################
  # 2) Try hybrid at start. (taxon name needs 1 word)
  ##############################
  ### 2.1) Get the indices of taxon_names with 1 word
  index_words_1 = which(no_words == 1)

  if(length(index_words_1)>0){
    if(console_message){
      cli::cli_alert_info("Trying fixing hybrid for taxon names with 1 words {length(index_words_1)} name{?s}")
    }
    ### 2.2) Get the taxon names By adding hybrid before first word.
    words_1 = taxon_names[index_words_1]
    new_taxon_names = unlist(lapply(words_1, function(x){
      new_taxon_names = c(paste('+',x,collapse =' '), paste('\u00D7', x, collapse = ' '))
      new_taxon_names = new_taxon_names[new_taxon_names %in% enrich_taxon_names_w_hybrid]

      return(paste0(new_taxon_names, collapse = ' OR '))
    }))

    out_fixed_names[index_words_1] = paste0(out_fixed_names[index_words_1], new_taxon_names, sep = '')

  }

  ########################
  # 3) 2/3/4 words try hybrid at start or after first word.
  ########################
  ### 3.1) Get the indices of taxon_names with 2/3/4 words
  index_words_2_3_4 = which(no_words %in% c(2,3,4), !grepl('\u00D7|\\+',taxon_names))

  if(length(index_words_2_3_4) > 0){
    if(console_message){
      cli::cli_alert_info("Trying fixing hybrid for taxon names with 2/3/4 words {length(index_words_2_3_4)} name{?s}")
    }

    words_2_3_4 = stringr::str_split(taxon_names[index_words_2_3_4], ' ', n=2)
    new_taxon_names = unlist(lapply(words_2_3_4, function(x){
      ### 3.2) Get the taxon names to try with adding infraspecific (/hybrid) markers. whilst checking if in enrich_database.
      before_first_word = paste(hybrids, x[1],x[2])
      after_first_word = paste(x[1], hybrids, x[2])
      new_taxon_names = c(before_first_word,after_first_word)
      new_taxon_names = new_taxon_names[new_taxon_names %in% enrich_taxon_names_w_hybrid]

      return(paste0(new_taxon_names, collapse = ' OR '))
    }))

    out_fixed_names[index_words_2_3_4] = paste0(out_fixed_names[index_words_2_3_4], new_taxon_names, sep = '')


  }

  ########################
  # 4) Change/remove hybrid
  ########################
  ### 3.1) Get the indices of taxon_names with hybrid characters.
  index_words_with_hybrid = which(grepl('\u00D7|\\+',taxon_names))

  if(length(index_words_with_hybrid) > 0){
    if(console_message){
      cli::cli_alert_info("Trying changing/removing hybrid for taxon names {length(index_words_with_hybrid)} name{?s}")
    }

    # Get the words with the hybrid changed or removed.
    new_taxon_names = unlist(lapply(taxon_names[index_words_with_hybrid], function(x){
      A=stringr::str_replace(x,pattern = '\u00D7|\\+', '')
      B=stringr::str_replace(x,pattern = '\u00D7|\\+', '\\+')
      C=stringr::str_replace(x,pattern = '\u00D7|\\+', '\u00D7')
      options = c(A,B,C)
      options = stringr::str_squish(options)
      new_taxon_names = options[-match(x, options)]
      new_taxon_names = new_taxon_names[new_taxon_names %in% enrich_database_taxon_names]

      return(paste0(new_taxon_names, collapse = ' OR '))
    }))

    out_fixed_names[index_words_with_hybrid] = paste0(out_fixed_names[index_words_with_hybrid], new_taxon_names, sep = '')

  }


  return(out_fixed_names)

}

#' Find best author matches
#'
#' @param collection_author The author from the collection wanted to be matched
#' @param enriched_database_authors Author options from the enrich_database.
#' @param partial_method Either `'most words` or `'any_words'`, defining the method used to find partial matches.
#' @param ... Arguments (i.e., attributes) used in the matching algorithm (passed along to nested fuctions).
#'
#' @return a list of length 2 with:
#'  - `$wanted` is a logical (TRUE/FALSE) vector with length `length(enriched_database_authors)` corresponding to the enriched database authors that most match the collection author.
#'  - `$message` detailing whether the author match was exact, partial or no match was found.
#' @export
#'
#' @examples
#' collection_author = 'Schult'
#' enriched_database_authors = c("(Lour.) Schult", "Borhidi & E.Martinez")
#' match_authors(collection_author, enriched_database_authors)
match_authors <- function(collection_author, enriched_database_authors, partial_method = 'most words', ...){

  ### 1) Exact matching
  exact_match = collection_author == enriched_database_authors
  exact_match[is.na(exact_match)] = FALSE # Set NA values to FALSE
  if(any(exact_match)){
    return(list(wanted = exact_match, message = '(Exact author match) -> '))
  }

  ### 2) Partial matching
  if(partial_method == 'most words'){
    # Get the words for the collections and databases authors.
    enriched_database_authors_words = lapply(enriched_database_authors, author_words)
    collection_author_words = author_words(collection_author)

    #Find number of words in database's authors found in the collection's author
    no_database_author_in_collection = unlist(lapply(enriched_database_authors_words,function(words){
      words = words[words != '']
      contain_words = unlist(lapply(words, function(x){grepl(x,collection_author)}))
      return(sum(contain_words))
    }))

    #Find number of words in collection's author found in the database's authors
    no_collection_author_in_database = rowSums(data.frame(lapply(collection_author_words, function(x){grepl(x,enriched_database_authors)})))

    # Combine above and find the maximum shared words.
    total_match_word_count = rowSums(cbind(no_database_author_in_collection,no_collection_author_in_database))
    max_words_found =  max(total_match_word_count, na.rm=T)

    # If maximum shared words > 0 return those with the maximum number of shared words.
    if(max_words_found > 0){
      match_author_words = total_match_word_count == max_words_found
      match_author_words[is.na(match_author_words)] = FALSE # Set NA values to FALSE
      return(list(wanted = match_author_words, message = '(Partial author <most words>) -> '))
    }
  }
  else if(partial_method == 'any words'){
    author_checks = unlist(lapply(enriched_database_authors,function(x){author_check(collection_author,x)}))
    if(any(author_checks == 'Partial')){
      partial_authors = author_checks == 'Partial'
      partial_authors[is.na(partial_authors)] = FALSE # Set NA values to FALSE
      return(list(wanted = partial_authors, message = '(Partial author <any words>) -> '))
    }
  }else{
    stop('Invalid partial_method input in match_authors()!')
  }


  ### 3) No matching
  return(list(wanted = rep(TRUE, length(enriched_database_authors)), message = '(No authors match) ->'))
}

