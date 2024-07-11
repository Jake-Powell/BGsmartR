# Script for running collection dynamics reports

#' create_trends_report()
#'
#'This function can be used to create the trends report of a living collection. The report produces interactive or static graphs (bars, pies) detailing the gain and loss of items/accessions/taxa/families over time whilst also considering the type of taxa (native, endemic, threatened) in the LC.
#'
#' @inheritParams create_native_report
#' @param PlantClassification plant classification information
#' @export
#'
create_taxonomic_diversity_report <- function(enriched_report,
                                   collection = NULL,
                                   wcvp = NULL,
                                   PlantClassification = BGSmartR::Diversity_classification$Classification,
                                   min_year = 1970,

                                   report_kind = 'static',
                                   output_file = NULL,
                                   output_dir = NULL,
                                   export_data = FALSE,
                                   save_excel = FALSE,
                                   separate_figure_folder = TRUE,
                                   table_font_size = 10,
                                   ggtheme = NULL,
                                   value_on_fig = FALSE,
                                   interactive_colour = viridis::viridis,
                                   scale_colour_continuous = ggplot2::scale_colour_viridis_c,
                                   scale_colour_discrete = ggplot2::scale_colour_viridis_d,
                                   scale_colour_binned = ggplot2::scale_colour_viridis_b,
                                   scale_fill_continuous = ggplot2::scale_fill_viridis_c,
                                   scale_fill_discrete = ggplot2::scale_fill_viridis_d,
                                   scale_fill_binned = ggplot2::scale_fill_viridis_b,
                                   reference_docx = NULL
){
  # 1) Setup.
  # A) Check inputs.

  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'taxonomic_diversity_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_taxonomic_diversity_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'taxonomic_diversity_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_taxonomic_diversity_static.docx')
    }
  }else{
    stop('Invalid report_kind input!')
  }

  # Set the output directory if not specified.
  if(is.null(output_dir)){
    output_dir = getwd()
  }


  # 2) Render basic stats_static document.
  if(report_kind == 'static'){
    if(is.null(reference_docx)){
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Taxonomic_diversity_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      wcvp = wcvp,
                                      PlantClassification = PlantClassification,
                                      min_year = min_year,

                                      report_kind = report_kind,
                                      output_file = output_file,
                                      output_dir = output_dir,
                                      export_data = export_data,
                                      separate_figure_folder = separate_figure_folder,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
                                      interactive_colour = interactive_colour,
                                      scale_colour_continuous = scale_colour_continuous,
                                      scale_colour_discrete =scale_colour_discrete,
                                      scale_colour_binned = scale_colour_binned,
                                      scale_fill_continuous = scale_fill_continuous,
                                      scale_fill_discrete = scale_fill_discrete,
                                      scale_fill_binned = scale_fill_binned,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Taxonomic_diversity_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      wcvp = wcvp,
                                      PlantClassification = PlantClassification,
                                      min_year = min_year,

                                      report_kind = report_kind,
                                      output_file = output_file,
                                      output_dir = output_dir,
                                      export_data = export_data,
                                      separate_figure_folder = separate_figure_folder,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
                                      interactive_colour = interactive_colour,
                                      scale_colour_continuous = scale_colour_continuous,
                                      scale_colour_discrete =scale_colour_discrete,
                                      scale_colour_binned = scale_colour_binned,
                                      scale_fill_continuous = scale_fill_continuous,
                                      scale_fill_discrete = scale_fill_discrete,
                                      scale_fill_binned = scale_fill_binned,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Taxonomic_diversity_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    wcvp = wcvp,
                                    PlantClassification = PlantClassification,
                                    min_year = min_year,

                                    report_kind = report_kind,
                                    output_file = output_file,
                                    output_dir = output_dir,
                                    export_data = export_data,
                                    separate_figure_folder = separate_figure_folder,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    interactive_colour = interactive_colour,
                                    scale_colour_continuous = scale_colour_continuous,
                                    scale_colour_discrete =scale_colour_discrete,
                                    scale_colour_binned = scale_colour_binned,
                                    scale_fill_continuous = scale_fill_continuous,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_fill_binned = scale_fill_binned,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}
