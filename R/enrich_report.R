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
#' @param do_is_autonym Flag for whether to add the column is_autonym
#' @param do_status_year Flag for whether to add the column status_year
#' @param do_infrageneric_level Flag for whether to add the column infrageneric_level

#'
#' @return a list of length two:
#' `$enriched_report` the enriched report, and
#' `match_details` the details of how taxon names have being used to match to POWO.
#' @export
enrich_report <- function(original_report, wcvp, do_is_autonym = FALSE, do_status_year = FALSE, do_infrageneric_level = FALSE){

  enriched_report = original_report

  ###
  # 1) Add is_autonym.
  ###
  if(do_is_autonym){
    enriched_report = add_is_autonym(enriched_report)
  }

  ###
  # 2) Add status_year.
  ###
  if(do_status_year){
    enriched_report = add_status_year(enriched_report)
  }


  ###
  # 4) Add information from POWO.
  ###
  # A) find the match between original report and wcvp.
  match_info = match_original_to_wcvp(original_report, wcvp)

  # B) Extract the info from wcvp.
  wcvp_wanted_columns = c("plant_name_id", "taxon_name", "taxon_authors", "taxon_rank", "taxon_status","powo_id", "family", "genus", "species", "lifeform_description", "climate_description", "geographic_area", "Dist_area_code_l3", "Dist_labels")
  wcvp_wanted_columns = wcvp_wanted_columns[wcvp_wanted_columns %in% names(wcvp$wcvp_names)]
  POWO_info = data.frame(matrix(NA, nrow = nrow(original_report), ncol = length(wcvp_wanted_columns)))
  names(POWO_info) = paste0('POWO_',wcvp_wanted_columns)
  indices = which(!(is.na(match_info$match) | match_info$match < 0))
  POWO_info[indices,] = wcvp$wcvp_names[match_info$match[indices],match(wcvp_wanted_columns,names(wcvp$wcvp_names))]

  # C) add to enriched report.
  enriched_report = data.frame(enriched_report, POWO_info)


  ###
  # 4) Add infrageneric_level.
  ###
  # We run infrageneric_level on the taxonName from POWO unless we didn't find a match then we use the original taxon name.
  if(do_infrageneric_level){
    enriched_report = add_infrageneric_level(enriched_report, POWO_TaxonNameColumn = 'POWO_taxon_name')
  }

  #Return the enriched report and the detail of how matching to POWO was performed.
  return(list(enriched_report = enriched_report, match_details = match_info$details))
}
