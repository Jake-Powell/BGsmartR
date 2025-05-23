
#' Create overview module
#'
#'@description
#'The most relevant plots illustrating the general quality of a collection. This include sections on:
#'  - Composition of the collection,
#'  - Geographic coverage of the collection,
#'  - Change over time of the collection,
#'  - Turnover of the collection,
#'  - Taxonomic diversity of the collection,
#'  - Survival in the collection,
#'  - Provenance in the collection and,
#'  - Important accessions.
#'
#'
#'@inheritParams create_native_report
#' @param PlantClassification_filepath filepath to family classification. If NULL we use the default contained within BGSmartR.
#' @param old_accession_year_codes Vector of numbers corresponding to values within "AccYear" that indicate that a record is old with unknown accession year. For our analyses over time we assume these records are accessioned on `earliest_allowable_record`.
#' @param earliest_allowable_record Number, corresponding to the minimum allowable year in the "AccYear" column.
#'
#' @export
#'
create_overview_report <- function(enriched_report,
                                   collection = NULL,
                                   wgsrpd3 = NULL,
                                   PlantClassification_filepath = NULL,
                                   # report_kind = 'static',
                                   coordinates = NA,
                                   # output_file = NULL,
                                   output_dir = NULL,
                                   # export_data = FALSE,
                                   # separate_figure_folder = TRUE,
                                   min_year = 1970,
                                   native = 'Naturally occurring only',
                                   extinct = TRUE,
                                   doubtful_locations = FALSE,
                                   # table_font_size = 10,
                                   # ggtheme = NULL,
                                   # value_on_fig = FALSE,
                                   earliest_allowable_record = 1700,
                                   old_accession_year_codes = NULL

                                   # scale_colour_continuous = ggplot2::scale_colour_viridis_c,
                                   # scale_colour_discrete = ggplot2::scale_colour_viridis_d,
                                   # scale_colour_binned = ggplot2::scale_colour_viridis_b,
                                   # scale_fill_continuous = ggplot2::scale_fill_viridis_c,
                                   # scale_fill_discrete = ggplot2::scale_fill_viridis_d,
                                   # scale_fill_binned = ggplot2::scale_fill_viridis_b,
                                   # reference_docx = NULL
){
  interactive_colour = viridis::viridis
  scale_colour_continuous = ggplot2::scale_colour_viridis_c
  scale_colour_discrete = ggplot2::scale_colour_viridis_d
  scale_colour_binned = ggplot2::scale_colour_viridis_b
  scale_fill_continuous = ggplot2::scale_fill_viridis_c
  scale_fill_discrete = ggplot2::scale_fill_viridis_d
  scale_fill_binned = ggplot2::scale_fill_viridis_b
  reference_docx = NULL
  table_font_size = 10
  ggtheme = NULL
  separate_figure_folder = FALSE
  value_on_fig = FALSE
  export_data = FALSE
  save_excel = FALSE
  output_file = NULL
  report_kind = 'interactive'
  color_binary = c('darkgray','darkgreen')
  palette = 'Greens'
  do_download = T
  # 1) Setup.
  # A) Check inputs.

  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'overview_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_overview_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'overview_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_overview_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Overview_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      output_file = output_file,
                                      output_dir = output_dir,
                                      export_data = export_data,
                                      separate_figure_folder = separate_figure_folder,
                                      min_year = min_year,
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
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Overview_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      output_file = output_file,
                                      output_dir = output_dir,
                                      export_data = export_data,
                                      separate_figure_folder = separate_figure_folder,
                                      min_year = min_year,
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
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      value_on_fig = value_on_fig
                        ),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Overview_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    report_kind = report_kind,
                                    coordinates = coordinates,
                                    output_file = output_file,
                                    output_dir = output_dir,
                                    export_data = export_data,
                                    separate_figure_folder = separate_figure_folder,
                                    min_year = min_year,
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
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}
