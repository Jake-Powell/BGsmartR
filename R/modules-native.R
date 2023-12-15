#' Create a report of native taxa in a LC
#'
#'This function can be used to create a report of the native plants in a living collection (LC). This includes exploring:
#' - How many native species are in the LC?
#' - What proportion of the LC at Kew is native and how has this varied over time?
#' - Among the native plants in a LC, how many of them are endemic or redlisted?
#' - Provenance of native plants in the LC
#' - Threatened, native plants
#' - Turnover of native plants
#' - Survival of native plants in the LC
#'
#'
#' @param enriched_report The enriched (via BGSmartR) report of a LC.
#' @param coordinates The coordinates of the collection (long, lat).
#' @param wgsrpd3 World Geographical Scheme for Recording Plant Distributions. Containing polygons of each level 3 area. Can be obtained from `rWCVP` pacakge.
#' @param wcvp World Checklist of Vascular Plants (WCVP) database, obtained using the function [import_wcvp_names()].
#' @param collection The name of the LC
#' @param native string where 'Naturally occurring only' or 'Introduced only' reduces the locations used to determine native plants.
#' @param extinct  Flag (TRUE/FALSE) for whether to include extinct geographic locations.
#' @param doubtful_locations  Flag (TRUE/FALSE) for whether to include doubtful locations.
#' @param min_year The minimum year for analyses over time.
#' @param export_data  Flag (TRUE/FALSE) for whether to create a `.rda` and `.xlsx` files containing the data used to create the figures in the plot.
#' @param output_file The file path of the native report.
#' @param table_font_size Font size for tables.
#' @param ggtheme gg plot theme to be applied to all ggplots, see `ggthemes` package for pre-set themes.
#' @param separate_figure_folder  Flag (TRUE/FALSE) for whether the figures should also be outputted in seperate files.
#' @param scale_colour_continuous,scale_colour_discrete,scale_colour_binned,scale_fill_continuous,scale_fill_discrete,scale_fill_binned  scales for ggplot. Default is to use viridis.
#' @param reference_docx path to a .docx file whose style (design) the report copies.
#' @param output_dir The output directory
#'
#' @return renders the native report (word document)
#' @export
#'
create_native_static <- function(enriched_report,
                                 coordinates = NULL,
                                 wgsrpd3 = NULL,
                                 wcvp = NULL,
                                 collection = 'LC',
                                 native = 'Naturally occurring only',
                                 extinct = TRUE,
                                 doubtful_locations = FALSE,
                                 min_year = 1970,
                                 export_data = FALSE,
                                 output_file = NULL,
                                 output_dir = NULL,
                                 table_font_size = 10,
                                 ggtheme = NULL,
                                 separate_figure_folder = TRUE,
                                 scale_colour_continuous = ggplot2::scale_colour_viridis_c,
                                 scale_colour_discrete = ggplot2::scale_colour_viridis_d,
                                 scale_colour_binned = ggplot2::scale_colour_viridis_b,
                                 scale_fill_continuous = ggplot2::scale_fill_viridis_c,
                                 scale_fill_discrete = ggplot2::scale_fill_viridis_d,
                                 scale_fill_binned = ggplot2::scale_fill_viridis_b,
                                 reference_docx = NULL
){


  # 1) Setup.
  # A) Check enriched report.
  if(is.null(coordinates)){
    stop('Error! Must provide LC coordinates')
  }
  if(length(coordinates) != 2){
    stop('`coords` must have length 2.')
  }
  if(is.null(wgsrpd3)){
    stop('Error! Must provide wgsrpd3 dataset.')
  }
  if(is.null(wcvp)){
    stop('Error! Must provide BGSmartR version of wcvp dataset.')
  }
  if(!is.numeric(min_year)){
    stop('`min_year` must be numeric!')
  }


  # B) Choose output file
  if(is.null(output_file) & collection == 'LC'){
    output_file = 'native_static.docx'
  }
  else if(is.null(output_file) & collection != 'LC'){
    output_file = paste0(collection, '_native_static.docx')
  }
  # Set the output directory if not specified.
  if(is.null(output_dir)){
    output_dir = getwd()
  }


  # 2) Render basic stats_static document.
  if(!is.null(reference_docx)){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Native_static.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    coordinates = coordinates,
                                    wgsrpd3 = wgsrpd3,
                                    wcvp = wcvp,
                                    collection = collection,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    min_year = min_year,
                                    export_data = export_data,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    scale_colour_continuous = scale_colour_continuous,
                                    scale_colour_discrete = scale_colour_discrete,
                                    scale_colour_binned = scale_colour_binned,
                                    scale_fill_continuous = scale_fill_continuous,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_fill_binned = scale_fill_binned,
                                    separate_figure_folder = separate_figure_folder,
                                    output_dir = output_dir),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::word_document(reference_docx = reference_docx))
  }else{
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Native_static.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    coordinates = coordinates,
                                    wgsrpd3 = wgsrpd3,
                                    wcvp = wcvp,
                                    collection = collection,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    min_year = min_year,
                                    export_data = export_data,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    scale_colour_continuous = scale_colour_continuous,
                                    scale_colour_discrete = scale_colour_discrete,
                                    scale_colour_binned = scale_colour_binned,
                                    scale_fill_continuous = scale_fill_continuous,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_fill_binned = scale_fill_binned,
                                    separate_figure_folder = separate_figure_folder,
                                    output_dir = output_dir),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::word_document())
  }


  if(separate_figure_folder){
    unlink('native_static_cache', recursive = T)
  }
}
