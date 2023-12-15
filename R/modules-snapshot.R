
#' create_snapshot_report()
#'
#'This function can be used to create the snapshot report of a living collection. The report produces interactive graphs (bars, pies and geography) of the distribution of plants in the collection. This includes the native location of all plants in the collection and the proportion of endemic and threatened species in the collection.
#'
#'@inheritParams create_native_static
#' @param report_kind The find of report to create, either `static` or `interactive`.
#' @param value_on_fig Flag (TRUE/FALSE) for whether to include values of static plots.
#' @param Diversity_classification filepath to divrsity classification. If NULL we use the default contained within BGSmartR.
#'
#' @export
#'
create_snapshot_report <- function(enriched_report,
                                   collection = NULL,
                                   wgsrpd3 = NULL,
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
                                   reference_docx = NULL,
                                   Diversity_classification = NULL
                                   ){
  # 1) Setup.
  # A) Check inputs.

  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'snapshot_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_snapshot_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'snapshot_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_snapshot_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Snapshot_report.Rmd"),
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
                                      value_on_fig = value_on_fig,
                                      Diversity_classification = Diversity_classification),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Snapshot_report.Rmd"),
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
                                      value_on_fig = value_on_fig,
                                      Diversity_classification = Diversity_classification),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Snapshot_report.Rmd"),
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
                                    value_on_fig = value_on_fig,
                                    Diversity_classification = Diversity_classification),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
  }

}
