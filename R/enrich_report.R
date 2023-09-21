#' Enrich a collection's database
#'
#' This function enriches plat records in a collection using information from POWO (WCVP), IUCN redlist and BGCI.
#'
#'
#' @param collection A data frame containing a collection.
#' @param wcvp World Checklist of Vascular Plants (WCVP) database, obtained using the function [import_wcvp_names()].
#' @param redList RedList database
#' @param BGCI Requires a cleaned BGCI plant search database to obtain how many collections globally a taxon is contained (Not freely available).
#' @param taxon_name_column The name of the column in the `collection` corresponding to taxonomic names.
#' @param taxon_name_full_column The name of the column in the `collection` corresponding to joined taxonomic names and authors.
#' @param taxon_author_column The name of the column in the `collection` corresponding to the authors of the taxonomic names.
#' @param do_is_autonym Flag (TRUE/FALSE) for whether to add the column is_autonym, see [add_is_autonym()] for details.
#' @param do_status_year Flag (TRUE/FALSE) for whether to add the column status_year, see [add_status_year()] for details.
#' @param do_taxon_types Flag (TRUE/FALSE) for whether to add the column taxon_type, see [add_taxon_type()] for details.
#' @param typo_method Defines the method used to find typos in the taxonomic name. Allowed values are `"full"`,  `"fast"`, or `"no"`. These correspond to full search (see [match_typos()]), search only using [typo_list] and no typo searching.
#'
#' @return The `collection` data frame enriched with information dependent on function inputs (new columns).
#' @export
enrich_collection <- function(collection,
                          wcvp = NA,
                          redList = NA,
                          BGCI = NA,
                          taxon_name_column = 'TaxonName',
                          taxon_name_full_column = NA,
                          taxon_author_column = NA,
                          do_is_autonym = FALSE, do_status_year = FALSE, do_taxon_types = FALSE, typo_method = 'fast'){

  ############################################
  # 1) Clean/extract taxon name + taxon author
  ############################################
  cli::cli_h2("Sanitise taxon name and extract author")
  taxon_name_and_author = sanitise_names_authors_report(collection,
                             taxon_name_column = taxon_name_column,
                             taxon_name_full_column = taxon_name_full_column,
                             taxon_author_column = taxon_author_column)


  collection = data.frame(collection,
                               sanitised_taxon = taxon_name_and_author$taxon_name,
                               need_sanitise = taxon_name_and_author$sanitised,
                               extracted_author = taxon_name_and_author$author)
  enriched_report = collection


  ############################################
  # 1) Add is_autonym.
  ############################################
  if(do_is_autonym){
    cli::cli_h2("Adding is autonym")
    enriched_report = add_is_autonym(enriched_report)
  }

  ############################################
  # 2) Add status_year.
  ############################################
  if(do_status_year){
    cli::cli_h2("Adding status year")
    enriched_report = add_status_year(enriched_report)
  }


  ############################################
  # 4) Add information from POWO.
  ############################################
  if(!is.na(wcvp)[1]){
  cli::cli_h2("Adding POWO information")

  # A) find the match between original report and wcvp.
  match_info = match_original_to_wcvp(collection,
                                      wcvp,
                                      taxon_name_col = 'sanitised_taxon',
                                      taxon_name_full_col = NA,
                                      taxon_author_col = 'extracted_author',
                                      typo_method = typo_method)

  # B) Extract the info from wcvp_names.
  wcvp_wanted_columns = c("plant_name_id", "taxon_name", "taxon_authors", "taxon_rank", "taxon_status","powo_id", "family", "genus", "species", "lifeform_description", "climate_description", "geographic_area")
  wcvp_wanted_columns = wcvp_wanted_columns[wcvp_wanted_columns %in% names(wcvp$wcvp_names)]
  POWO_info = data.frame(matrix(NA, nrow = nrow(collection), ncol = length(wcvp_wanted_columns)))
  names(POWO_info) = paste0('POWO_',wcvp_wanted_columns)
  indices = which(!(is.na(match_info$match) | match_info$match < 0))
  POWO_info[indices,] = wcvp$wcvp_names[match_info$match[indices],match(wcvp_wanted_columns,names(wcvp$wcvp_names))]

  # C) Add info from wcvp_distributions.
  Geog_info = data.frame(matrix(NA, nrow = nrow(collection), ncol = length(names(wcvp$geography))))
  havePOWO_index = which(!is.na(POWO_info$POWO_plant_name_id))
  plant_id_match = match(POWO_info$POWO_plant_name_id[havePOWO_index], wcvp$geography$plant_name_id)

  Geog_info[havePOWO_index,] = wcvp$geography[plant_id_match,]
  names(Geog_info) = paste0('POWO_', names(wcvp$geography))

  # D) Add info from WCVP matched to Redlist.
  wanted_columns = c("plant_name_id", "main_common_name", "assessment_date", "category",           "criteria", "population_trend")
  red_data = wcvp$redList[,match(wanted_columns, names(wcvp$redList))]
  Red_info = data.frame(matrix(NA, nrow = nrow(collection), ncol = length(wanted_columns)))
  havePOWO_index = which(!is.na(POWO_info$POWO_plant_name_id))
  plant_id_match = match(POWO_info$POWO_plant_name_id[havePOWO_index], red_data$plant_name_id)

  Red_info[havePOWO_index,] = red_data[plant_id_match,]
  names(Red_info) = paste0('POWO_Red_',wanted_columns)

  # E) Create POWO web address.
  POWO_web_address = rep(NA, nrow(POWO_info))
  indices = !is.na(POWO_info$POWO_powo_id)
  POWO_web_address[indices] = paste0('https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:',POWO_info$POWO_powo_id[indices])


  # E) add to enriched report.
  enriched_report = data.frame(enriched_report,
                               original_authors = match_info$original_authors,
                               match_taxon_name = match_info$match_taxon_name,
                               match_authors = match_info$match_authors,
                               author_check = match_info$author_check,
                               match_detail = match_info$details,
                               match_detail_short =match_info$details_short,
                               POWO_web_address = POWO_web_address, POWO_info, Geog_info[,-1], Red_info[,-1])
  }

  ############################################
  # 4) Add taxon type
  ############################################
  # We run infrageneric_level on the taxonName from POWO unless we didn't find a match then we use the original taxon name.
  if(do_taxon_types){
    cli::cli_h2("Adding taxon types")
    enriched_report = add_taxon_type(enriched_report, POWO_taxon_name_column = 'POWO_taxon_name')
  }

  ############################################
  # 5) Add RedList information.
  ############################################
  if(!is.na(redList)[1]){
    cli::cli_h2("Adding ICUN Red list information")

    redList$taxon_status = rep('NA',nrow(redList))
    redList$accepted_plant_name_id = 1:nrow(redList)
    redList$plant_name_id = 1:nrow(redList)

    match_info = match_original_to_wcvp(collection,
                                        wcvp = list(wcvp_names = redList, exceptions = NULL, changes = NULL),
                                        taxon_name_col = 'sanitised_taxon',
                                        taxon_name_full_col = NA,
                                        taxon_author_col = 'extracted_author',
                                        typo_method = typo_method)

    redList_wanted_columns = c("scientific_name", 'authority', "main_common_name",
                               "category", "published_year", "assessment_date",
                               'criteria', 'population_trend',
                               'aoo_km2', 'eoo_km2',
                               'elevation_upper', 'elevation_lower')
    redList_wanted_columns = redList_wanted_columns[redList_wanted_columns %in% names(redList)]
    redList_info = data.frame(matrix(NA, nrow = nrow(collection), ncol = length(redList_wanted_columns)))
    names(redList_info) = paste0('redList_',redList_wanted_columns)
    indices = which(!(is.na(match_info$match) | match_info$match < 0))
    redList_info[indices,] = redList[match_info$match[indices],match(redList_wanted_columns,names(redList))]

    enriched_report = data.frame(enriched_report,
                                 redList_original_authors = match_info$original_authors,
                                 redList_match_taxon_name = match_info$match_taxon_name,
                                 redList_match_authors = match_info$match_authors,
                                 redList_author_check = match_info$author_check,
                                 redList_match_detail = match_info$details,
                                 redList_match_detail_short =match_info$details_short,
                                 redList_info)
  }

  ############################################
  # 5) Add Number of Gardens (BGCI).
  ############################################
  if(!is.na(BGCI)[1]){
    cli::cli_h2("Adding Number of gardens")

    no_gardens = match_original_to_BGCI(collection,
                                        BGCI,
                                        taxon_name_col = 'sanitised_taxon')

    enriched_report = data.frame(enriched_report,
                                 no_gardens = no_gardens$no_gardens)
  }

  #Return the enriched report.
  return(enriched_report)
}
