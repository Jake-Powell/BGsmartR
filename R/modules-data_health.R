
#' create_data_health_report()
#'
#'
#'@inheritParams create_native_report
#' @param PlantClassification_filepath filepath to family classification. If NULL we use the default contained within BGSmartR.
#'
#' @export
#'
create_data_health_report <- function(enriched_report,
                                   collection = NULL,
                                   wgsrpd3 = NULL,
                                   PlantClassification_filepath = NULL,
                                   report_kind = 'static',
                                   coordinates = NA,
                                   output_file = NULL,
                                   output_dir = NULL,
                                   export_data = FALSE,
                                   separate_figure_folder = TRUE,
                                   native = 'Naturally occurring only',
                                   extinct = TRUE,
                                   doubtful_locations = FALSE,
                                   table_font_size = 10,
                                   ggtheme = NULL,
                                   value_on_fig = FALSE,

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
      output_file = 'data_health_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_data_health_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'data_health_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_data_health_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Data_health_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      output_file = output_file,
                                      output_dir = output_dir,
                                      export_data = export_data,
                                      separate_figure_folder = separate_figure_folder,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Data_health_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      output_file = output_file,
                                      output_dir = output_dir,
                                      export_data = export_data,
                                      separate_figure_folder = separate_figure_folder,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
                                      scale_colour_continuous = scale_colour_continuous,
                                      scale_colour_discrete =scale_colour_discrete,
                                      scale_colour_binned = scale_colour_binned,
                                      scale_fill_continuous = scale_fill_continuous,
                                      scale_fill_discrete = scale_fill_discrete,
                                      scale_fill_binned = scale_fill_binned,
                                      value_on_fig = value_on_fig
                                      ),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Data_health_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    report_kind = report_kind,
                                    coordinates = coordinates,
                                    output_file = output_file,
                                    output_dir = output_dir,
                                    export_data = export_data,
                                    separate_figure_folder = separate_figure_folder,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    scale_colour_continuous = scale_colour_continuous,
                                    scale_colour_discrete =scale_colour_discrete,
                                    scale_colour_binned = scale_colour_binned,
                                    scale_fill_continuous = scale_fill_continuous,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_fill_binned = scale_fill_binned,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4))
  }

}
