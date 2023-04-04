#' Import wcvp_names.csv
#'
#' Given the path to `wcvp_names.csv` file obtained from POTW website load the relevent information into R.
#'
#' @param filepath path to wcvp_names.csv file.
#'
#' @return Desired information from wcvp_names.csv
#' @export
#'
import_wcvp_names <- function(filepath){
  # 1) Define the information we want.
  wanted_columns = c("plant_name_id", "taxon_rank", "taxon_status", "family", "genus", "species", "lifeform_description", "climate_description", "taxon_name", "taxon_authors", "accepted_plant_name_id", "powo_id")

  # 2) Load and select columns
  wcvp_names = utils::read.table(filepath, sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
  wcvp_names = wcvp_names[,match(wanted_columns, names(wcvp_names))]

  return(wcvp_names)
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
#' @param TaxonName_column The name of the column containing the Taxon name.
#'
#' @return The BG database with a new column called is_autonym which flags where the Taxon name is an autonym
#' @export
add_is_autonym <- function(data, TaxonName_column = 'TaxonName'){
  autonyms = unlist(lapply(data[,TaxonName_column], is_autonym))
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
