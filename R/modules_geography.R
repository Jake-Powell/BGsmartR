#' Create a report of the geographic distribution of the LC (currently existing)
#'
#'This function can be used to create a report of the geographic distribution of plants in a living collection (LC). This includes exploring:
#' - Which geographic regions are represented by species in the LC?
#' - Which geographic regions are represented by Endemic species in the LC?
#' - Which geographic regions are represented by Threatened species in the LC?
#' - How many species are their from each regions in the LC?
#' - How many Endemic species are their from each regions in the LC?
#' - How many Threatened species are their from each regions in the LC?
#'
#'
#' @param enriched_report The enriched (via BGSmartR) report of a LC.
#' @param collection The name of the LC
#' @param wgsrpd3 World Geographical Scheme for Recording Plant Distributions. Containing polygons of each level 3 area. Can be obtained from `rWCVP` pacakge.

#' @param native string where 'Naturally occurring only' or 'Introduced only' reduces the locations used to determine native plants.
#' @param extinct  Flag (TRUE/FALSE) for whether to include extinct geographic locations.
#' @param doubtful_locations  Flag (TRUE/FALSE) for whether to include doubtful locations.
#' @param export_data  Flag (TRUE/FALSE) for whether to create a `.rda` and `.xlsx` files containing the data used to create the figures in the plot.
#' @param output_file The file path of the native report.
#' @param table_font_size Font size for tables.
#' @param ggtheme gg plot theme to be applied to all ggplots, see `ggthemes` package for pre-set themes.
#' @param separate_plot_folder  Flag (TRUE/FALSE) for whether the figures should also be outputted in seperate files.
#' @param scale_colour_distiller,scale_fill_distiller,scale_fill_discrete,scale_colour_discrete  scales for ggplot. Default is to use viridis for discrete and greens for distiller.
#' @param reference_docx path to a .docx file whose style (design) the report copies.
#'
#' @return renders the native report (word document)
#' @export
#'
create_geography_static <- function(enriched_report,
                                    collection = NULL,
                                    wgsrpd3 = NULL,
                                    native = 'Naturally occurring only',
                                    extinct = TRUE,
                                    doubtful_locations = FALSE,
                                    export_data = FALSE,
                                    output_file = NULL,
                                    table_font_size = 10,
                                    ggtheme = NULL,
                                    separate_plot_folder = TRUE,
                                    scale_colour_distiller = function(...) ggplot2::scale_colour_distiller(palette="Greens",...),
                                    scale_fill_distiller = function(...) ggplot2::scale_fill_distiller(palette="Greens",...),
                                    scale_fill_discrete = ggplot2::scale_fill_viridis_d,
                                    scale_colour_discrete = ggplot2::scale_colour_viridis_d,
                                    reference_docx = NULL){
  # 1) Setup.
  # A) Check enriched report.
  if(is.null(wgsrpd3)){
    stop('Error! Must provide wgsrpd3 dataset.')
  }

  # B) Choose output file
  if(is.null(output_file) & is.null(collection)){
    output_file = 'geography_static.docx'
  }
  else if(is.null(output_file) & !is.null(collection)){
    output_file = paste0(collection, '_geography_static.docx')
  }

  if(!is.null(reference_docx)){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Geography_static.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    wgsrpd3 = wgsrpd3,
                                    collection = collection,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    export_data = export_data,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    scale_colour_distiller = scale_colour_distiller,
                                    scale_fill_distiller = scale_fill_distiller,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_colour_discrete = scale_colour_discrete,
                                    separate_plot_folder = separate_plot_folder),
                      output_file = output_file,
                      output_dir = getwd(),
                      output_format = rmarkdown::word_document(reference_docx = reference_docx))
  }else{
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Geography_static.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    wgsrpd3 = wgsrpd3,
                                    collection = collection,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    export_data = export_data,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    scale_colour_distiller = scale_colour_distiller,
                                    scale_fill_distiller = scale_fill_distiller,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_colour_discrete = scale_colour_discrete,
                                    separate_plot_folder = separate_plot_folder),
                      output_file = output_file,
                      output_dir = getwd(),
                      output_format = rmarkdown::word_document())
  }


}
