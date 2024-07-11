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
#' @param wcvp World Checklist of Vascular Plants (WCVP) database, obtained using the function [import_wcvp_names()].
#' @param native string where 'Naturally occurring only' or 'Introduced only' reduces the locations used to determine native plants.
#' @param extinct  Flag (TRUE/FALSE) for whether to include extinct geographic locations.
#' @param doubtful_locations  Flag (TRUE/FALSE) for whether to include doubtful locations.
#' @param export_data  Flag (TRUE/FALSE) for whether to create a `.rda` and `.xlsx` files containing the data used to create the figures in the plot.
#' @param output_file The file path of the native report.
#' @param table_font_size Font size for tables.
#' @param ggtheme gg plot theme to be applied to all ggplots, see `ggthemes` package for pre-set themes.
#' @param separate_figure_folder  Flag (TRUE/FALSE) for whether the figures should also be outputted in seperate files.
#' @param scale_colour_distiller,scale_fill_distiller  scales for ggplot. Default is to use viridis for discrete and greens for distiller.
#' @param reference_docx path to a .docx file whose style (design) the report copies.
#' @param output_dir The output directory
#' @param value_on_fig Flag (TRUE/FALSE) for whether to include values of static plots.
#' @param report_kind The find of report to create, either `static` or `interactive`.
#' @param palette String colour palette for use in the interactive report.
#' @param color_binary colours for the representation of species.
#' @param endemic_species_per_region A data frame detailing the number of accepted endemic species found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#' @param accepted_species_per_region A data frame detailing the number of accepted species (unique records in WCVP, including subspecies, etc) found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#' @param tree_species_per_region A data frame detailing the number of accepted trees (unique records in WCVP, including subspecies, etc) found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#'
#' @return renders the native (word document) or interactive report (html document)
#' @export
#'
create_geography_report <- function(enriched_report,
                                    collection = NULL,
                                    wgsrpd3 = NULL,
                                    wcvp = NULL,
                                    report_kind = 'static',
                                    native = 'Naturally occurring only',
                                    extinct = TRUE,
                                    doubtful_locations = FALSE,
                                    export_data = FALSE,
                                    output_file = NULL,
                                    output_dir = NULL,
                                    table_font_size = 10,
                                    ggtheme = NULL,
                                    separate_figure_folder = TRUE,
                                    value_on_fig = FALSE,
                                    endemic_species_per_region = NULL,
                                    accepted_species_per_region = NULL,
                                    tree_species_per_region = NULL,

                                    color_binary = c('darkgray','darkgreen'),
                                    palette = 'Greens',
                                    scale_colour_distiller = function(...) ggplot2::scale_colour_distiller(palette="Greens",...),
                                    scale_fill_distiller = function(...) ggplot2::scale_fill_distiller(palette="Greens",...),
                                    reference_docx = NULL){
  # 1) Setup.
  # A) Check enriched report.
  if(is.null(wgsrpd3)){
    stop('Error! Must provide wgsrpd3 dataset.')
  }

  # B) Choose output file
  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'geography_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_geography_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'geography_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_geography_static.docx')
    }
  }else{
    stop('Invalid report_kind input!')
  }
  # Set the output directory if not specified.
  if(is.null(output_dir)){
    output_dir = getwd()
  }


  if(report_kind == 'static'){
    if(!is.null(reference_docx)){
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Geography_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      wgsrpd3 = wgsrpd3,
                                      wcvp = wcvp,
                                      collection = collection,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      export_data = export_data,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
                                      color_binary = color_binary,
                                      palette = palette,
                                      scale_colour_distiller = scale_colour_distiller,
                                      scale_fill_distiller = scale_fill_distiller,
                                      separate_figure_folder = separate_figure_folder,
                                      output_dir = output_dir,
                                      endemic_species_per_region = endemic_species_per_region,
                                      accepted_species_per_region = accepted_species_per_region,
                                      tree_species_per_region = tree_species_per_region,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Geography_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      wgsrpd3 = wgsrpd3,
                                      wcvp = wcvp,
                                      collection = collection,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      export_data = export_data,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
                                      color_binary = color_binary,
                                      palette = palette,
                                      scale_colour_distiller = scale_colour_distiller,
                                      scale_fill_distiller = scale_fill_distiller,
                                      separate_figure_folder = separate_figure_folder,
                                      output_dir = output_dir,
                                      endemic_species_per_region = endemic_species_per_region,
                                      accepted_species_per_region = accepted_species_per_region,
                                      tree_species_per_region = tree_species_per_region,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Geography_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    wgsrpd3 = wgsrpd3,
                                    wcvp = wcvp,
                                    collection = collection,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    export_data = export_data,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    color_binary = color_binary,
                                    palette = palette,
                                    scale_colour_distiller = scale_colour_distiller,
                                    scale_fill_distiller = scale_fill_distiller,
                                    separate_figure_folder = separate_figure_folder,
                                    output_dir = output_dir,
                                    endemic_species_per_region = endemic_species_per_region,
                                    accepted_species_per_region = accepted_species_per_region,
                                    tree_species_per_region = tree_species_per_region,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }



}
