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

  # C) Create a compressed version on the enriched report
  match_short = rep('',nrow(enriched_report))
  match_options = matrix(c("(matches POWO record with single entry)", '1',
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
                    "(Typo)", '6',
                    "(Not in POWO)", '8',
                    "(Go to accepted name)", 'A'),
                    ncol=2, byrow = T)
  for(i in 1:nrow(match_options)){
    indices = grepl(match_options[i,1], match_info$details)
    match_short[indices] = paste0(match_short[indices],', ', match_options[i,2])
  }
  match_short = stringr::str_remove(match_short, '^, ')


  # D) Create POWO web address.
  POWO_web_address = rep(NA, nrow(POWO_info))
  indices = !is.na(POWO_info$POWO_powo_id)
  POWO_web_address[indices] = paste0('https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:',POWO_info$POWO_powo_id[indices])


  # E) add to enriched report.
  enriched_report = data.frame(enriched_report, match_detail = match_info$details, match_detail_short =match_short,
                               POWO_web_address = POWO_web_address, POWO_info)


  ###
  # 4) Add infrageneric_level.
  ###
  # We run infrageneric_level on the taxonName from POWO unless we didn't find a match then we use the original taxon name.
  if(do_infrageneric_level){
    enriched_report = add_infrageneric_level(enriched_report, POWO_TaxonNameColumn = 'POWO_taxon_name')
  }

  #Return the enriched report.
  return(enriched_report)
}
