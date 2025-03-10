# Script for running collection dynamics reports

#' Create collection dynamics module
#'
#'@description
#'**Trends**: Evolution over time of the capacity and diversity of a collection. **Turnover**: Dynamics of the gained and lost plants each year.
#'
#'
#' @inheritParams create_native_report
#' @param data_type Either "Accessions" or "Items". Specifies the quantity to use when looking at the trends of provenance, native species, endemic species and threatened species.
#' @param old_accession_year_codes Vector of numbers corresponding to values within "AccYear" that indicate that a record is old with unknown accession year. For our analyses over time we assume these records are accessioned on `earliest_allowable_record`.
#' @param earliest_allowable_record Number, corresponding to the minimum allowable year in the "AccYear" column.
#' @param interactive_colour Number, corresponding to the minimum allowable year in the "AccYear" column.
#' @param do_geography_trend Flag (TRUE/FALSE), if true we export an excel file of the trends for each  TDWG geographical codes (Brummitt, 2001) expressed to that system’s third level (369 regions). Also, if `separate_figure_folder == TRUE` we create a folder within `separate_figure_folder` called geography_trends which contains a map for each year between min_year to today. These can be joined into a video using ffmpeg e.g. ffmpeg -r 5 -i geography_trend%d.jpeg geography_trend.mp4.
#' @export
#'
create_trends_report <- function(enriched_report,
                                   collection = NULL,
                                   min_year = 1970,
                                   coordinates = NULL,
                                   native = 'Naturally occurring only',
                                   extinct = TRUE,
                                   doubtful_locations = FALSE,
                                   wgsrpd3 = NULL,
                                   report_kind = 'interactive',
                                   data_type = 'Accessions',
                                   earliest_allowable_record = 1700,
                                   old_accession_year_codes = NULL,
                                   do_geography_trend = FALSE,
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
      output_file = 'trends_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_trends_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'trends_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_trends_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Trends_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
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
                                      min_year = min_year,
                                      coordinates = coordinates,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      wgsrpd3 = wgsrpd3,
                                      value_on_fig = value_on_fig,
                                      data_type = data_type,
                                      do_geography_trend =do_geography_trend,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      save_excel = save_excel),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Trends_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
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
                                      min_year = min_year,
                                      coordinates = coordinates,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      wgsrpd3 = wgsrpd3,
                                      value_on_fig = value_on_fig,
                                      data_type = data_type,
                                      do_geography_trend =do_geography_trend,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      save_excel = save_excel),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Trends_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    report_kind = report_kind,
                                    output_file = output_file,
                                    output_dir = output_dir,
                                    export_data = export_data,
                                    separate_figure_folder = separate_figure_folder,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    interactive_colour =interactive_colour,
                                    scale_colour_continuous = scale_colour_continuous,
                                    scale_colour_discrete =scale_colour_discrete,
                                    scale_colour_binned = scale_colour_binned,
                                    scale_fill_continuous = scale_fill_continuous,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_fill_binned = scale_fill_binned,
                                    min_year = min_year,
                                    coordinates = coordinates,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    wgsrpd3 = wgsrpd3,
                                    value_on_fig = value_on_fig,
                                    data_type = data_type,
                                    do_geography_trend =do_geography_trend,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    save_excel = save_excel),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' @rdname create_trends_report
#' @export
#'
create_turnover_report <- function(enriched_report,
                                 collection = NULL,
                                 min_year = 1970,

                                 coordinates = NULL,
                                 native = 'Naturally occurring only',
                                 extinct = TRUE,
                                 doubtful_locations = FALSE,
                                 wgsrpd3 = NULL,

                                 report_kind = 'interactive',
                                 data_type = 'Accessions',
                                 earliest_allowable_record = 1700,
                                 old_accession_year_codes = NULL,
                                 do_geography_trend = TRUE,


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
      output_file = 'turnover_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_turnover_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'turnover_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_turnover_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Turnover_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
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
                                      min_year = min_year,
                                      coordinates = coordinates,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      wgsrpd3 = wgsrpd3,
                                      value_on_fig = value_on_fig,
                                      data_type = data_type,
                                      do_geography_trend =do_geography_trend,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      save_excel = save_excel),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Turnover_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
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
                                      min_year = min_year,
                                      coordinates = coordinates,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      wgsrpd3 = wgsrpd3,
                                      value_on_fig = value_on_fig,
                                      data_type = data_type,
                                      do_geography_trend =do_geography_trend,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      save_excel = save_excel),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Turnover_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    report_kind = report_kind,
                                    output_file = output_file,
                                    output_dir = output_dir,
                                    export_data = export_data,
                                    separate_figure_folder = separate_figure_folder,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    interactive_colour =interactive_colour,
                                    scale_colour_continuous = scale_colour_continuous,
                                    scale_colour_discrete =scale_colour_discrete,
                                    scale_colour_binned = scale_colour_binned,
                                    scale_fill_continuous = scale_fill_continuous,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_fill_binned = scale_fill_binned,
                                    min_year = min_year,
                                    coordinates = coordinates,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    wgsrpd3 = wgsrpd3,
                                    value_on_fig = value_on_fig,
                                    data_type = data_type,
                                    do_geography_trend =do_geography_trend,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    save_excel = save_excel),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}
