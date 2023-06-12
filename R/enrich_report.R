#' enrich_original()
#'
#' This function enriches an original report by adding columns:
#'
#' - `is_autonym` (Logical (TRUE/FALSE)) detailing whether the taxon name is an autonym.
#' - `status_year` (numeric) the year extracted from `ItemStatusDate.`
#' - `infrageneric_level` (character) the infrageneric level of the taxon name. Uses taxon names retrieved from POWO if available.
#' - Information from POWO.
#'
#' @param original_report A gardens original report
#' @param wcvp POWO database
#' @param redList RedList database
#' @param BGCI BGCI database
#' @param taxon_name_col Column name in the original report for taxon name
#' @param taxon_name_full_col Column name in the original report for taxon name and author/s (combined)
#' @param taxon_author_col Column name in the original report for taxon author/s
#' @param do_is_autonym Flag for whether to add the column is_autonym
#' @param do_status_year Flag for whether to add the column status_year
#' @param do_infrageneric_level Flag for whether to add the column infrageneric_level
#' @param typo_method Flag for whether we search for typos
#'
#' @return a list of length two:
#' `$enriched_report` the enriched report, and
#' `match_details` the details of how taxon names have being used to match to POWO.
#' @export
enrich_report <- function(original_report,
                          wcvp = NA,
                          redList = NA,
                          BGCI = NA,
                          taxon_name_col = 'TaxonName',
                          taxon_name_full_col = NA,
                          taxon_author_col = NA,
                          do_is_autonym = FALSE, do_status_year = FALSE, do_infrageneric_level = FALSE, typo_method = 'fast'){

  ############################################
  # 1) Clean/extract taxon name + taxon author
  ############################################
  cli::cli_h2("Sanitise taxon name and extract author")
  taxon_name_and_author = clean_names_authors_report(original_report,
                             taxon_name_col = taxon_name_col,
                             taxon_name_full_col = taxon_name_full_col,
                             taxon_author_col = taxon_author_col)


  original_report = data.frame(original_report,
                               sanitised_taxon = taxon_name_and_author$taxon_name,
                               need_sanitise = taxon_name_and_author$sanitised,
                               extracted_author = taxon_name_and_author$author)
  enriched_report = original_report


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
  match_info = match_original_to_wcvp(original_report,
                                      wcvp,
                                      taxon_name_col = 'sanitised_taxon',
                                      taxon_name_full_col = NA,
                                      taxon_author_col = 'extracted_author',
                                      typo_method = typo_method)

  # B) Extract the info from wcvp.
  wcvp_wanted_columns = c("plant_name_id", "taxon_name", "taxon_authors", "taxon_rank", "taxon_status","powo_id", "family", "genus", "species", "lifeform_description", "climate_description", "geographic_area", "Dist_area_code_l3", "Dist_labels")
  wcvp_wanted_columns = wcvp_wanted_columns[wcvp_wanted_columns %in% names(wcvp$wcvp_names)]
  POWO_info = data.frame(matrix(NA, nrow = nrow(original_report), ncol = length(wcvp_wanted_columns)))
  names(POWO_info) = paste0('POWO_',wcvp_wanted_columns)
  indices = which(!(is.na(match_info$match) | match_info$match < 0))
  POWO_info[indices,] = wcvp$wcvp_names[match_info$match[indices],match(wcvp_wanted_columns,names(wcvp$wcvp_names))]

  # D) Create POWO web address.
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
                               POWO_web_address = POWO_web_address, POWO_info)
  }

  ############################################
  # 4) Add infrageneric_level.
  ############################################
  # We run infrageneric_level on the taxonName from POWO unless we didn't find a match then we use the original taxon name.
  if(do_infrageneric_level){
    cli::cli_h2("Adding infrageneric level")
    enriched_report = add_infrageneric_level(enriched_report, POWO_TaxonNameColumn = 'POWO_taxon_name')
  }

  ############################################
  # 5) Add RedList information.
  ############################################
  if(!is.na(redList)[1]){
    cli::cli_h2("Adding ICUN Red list information")

    redList$taxon_status = rep('NA',nrow(redList))
    redList$accepted_plant_name_id = 1:nrow(redList)
    redList$plant_name_id = 1:nrow(redList)

    match_info = match_original_to_wcvp(original_report,
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
    redList_info = data.frame(matrix(NA, nrow = nrow(original_report), ncol = length(redList_wanted_columns)))
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

    no_gardens = match_original_to_BGCI(original_report,
                                        BGCI,
                                        taxon_name_col = 'sanitised_taxon')

    enriched_report = data.frame(enriched_report,
                                 no_gardens = no_gardens$no_gardens)
  }

  #Return the enriched report.
  return(enriched_report)
}
