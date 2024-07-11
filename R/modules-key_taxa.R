#' create_endemic_report()
#'
#'This function can be used to create the snapshot report of a living collection. The report produces interactive graphs (bars, pies and geography) of the distribution of plants in the collection. This includes the native location of all plants in the collection and the proportion of endemic and threatened species in the collection.
#'
#'@inheritParams create_native_report
#' @param endemic_species_per_region A data frame detailing the number of accepted endemic species found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#' @param palette String colour palette for use in the interactive report maps.
#' @param color_binary colours for the representation of species in maps.
#' @param recent_year The year from which to do the recent analysis from.
#'
#' @export
#'
create_endemic_report <- function(enriched_report,
                                   collection = NULL,
                                   wgsrpd3 = NULL,
                                   wcvp = NULL,
                                   endemic_species_per_region = NULL,

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
                                   recent_year = as.numeric(format(Sys.Date(), '%Y')) - 3,
                                   min_year = 1970,
                                  earliest_allowable_record = 1700,
                                  old_accession_year_codes = NULL,

                                   scale_colour_continuous = ggplot2::scale_colour_viridis_c,
                                   scale_colour_discrete = ggplot2::scale_colour_viridis_d,
                                   scale_colour_binned = ggplot2::scale_colour_viridis_b,
                                   scale_fill_continuous = ggplot2::scale_fill_viridis_c,
                                   scale_fill_discrete = ggplot2::scale_fill_viridis_d,
                                   scale_fill_binned = ggplot2::scale_fill_viridis_b,
                                   color_binary = c('darkgray','darkgreen'),
                                   palette = 'Greens',
                                   reference_docx = NULL
){
  # 1) Setup.
  # A) Check inputs.

  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'endemic_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_endemic_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'endemic_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_endemic_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Endemic_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      wcvp = wcvp,
                                      wgsrpd3 = wgsrpd3,
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
                                      color_binary = color_binary,
                                      palette = palette,
                                      value_on_fig = value_on_fig,
                                      min_year =min_year,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      recent_year = recent_year),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Endemic_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      wcvp = wcvp,
                                      wgsrpd3 = wgsrpd3,
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
                                      color_binary = color_binary,
                                      palette = palette,
                                      recent_year =recent_year,
                                      min_year = min_year,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Endemic_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    report_kind = report_kind,
                                    coordinates = coordinates,
                                    wcvp = wcvp,
                                    wgsrpd3 = wgsrpd3,
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
                                    color_binary = color_binary,
                                    palette = palette,
                                    recent_year = recent_year,
                                    min_year = min_year,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' create_threatened_report()
#'
#'This function can be used to create the snapshot report of a living collection. The report produces interactive graphs (bars, pies and geography) of the distribution of plants in the collection. This includes the native location of all plants in the collection and the proportion of endemic and threatened species in the collection.
#'
#'@inheritParams create_native_report
#' @param palette String colour palette for use in the interactive report maps.
#' @param color_binary colours for the representation of species in maps.
#' @param recent_year The year from which to do the recent analysis from.
#'
#' @export
#'
create_threatened_report <- function(enriched_report,
                                  collection = NULL,
                                  wgsrpd3 = NULL,
                                  wcvp = NULL,

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
                                  recent_year = as.numeric(format(Sys.Date(), '%Y')) - 3,
                                  min_year = 1970,
                                  earliest_allowable_record = 1700,
                                  old_accession_year_codes = NULL,

                                  scale_colour_continuous = ggplot2::scale_colour_viridis_c,
                                  scale_colour_discrete = ggplot2::scale_colour_viridis_d,
                                  scale_colour_binned = ggplot2::scale_colour_viridis_b,
                                  scale_fill_continuous = ggplot2::scale_fill_viridis_c,
                                  scale_fill_discrete = ggplot2::scale_fill_viridis_d,
                                  scale_fill_binned = ggplot2::scale_fill_viridis_b,
                                  color_binary = c('darkgray','darkgreen'),
                                  palette = 'Oranges',
                                  reference_docx = NULL
){
  # 1) Setup.
  # A) Check inputs.

  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'threatened_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_threatened_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'threatened_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_threatened_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Threatened_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      wcvp = wcvp,
                                      wgsrpd3 = wgsrpd3,
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
                                      color_binary = color_binary,
                                      palette = palette,
                                      value_on_fig = value_on_fig,
                                      min_year =min_year,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      recent_year = recent_year),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Threatened_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      wcvp = wcvp,
                                      wgsrpd3 = wgsrpd3,
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
                                      color_binary = color_binary,
                                      palette = palette,
                                      recent_year =recent_year,
                                      min_year = min_year,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Threatened_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    report_kind = report_kind,
                                    coordinates = coordinates,
                                    wcvp = wcvp,
                                    wgsrpd3 = wgsrpd3,
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
                                    color_binary = color_binary,
                                    palette = palette,
                                    recent_year = recent_year,
                                    min_year = min_year,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' create_trees_report()
#'
#'This function can be used to create the snapshot report of a living collection. The report produces interactive graphs (bars, pies and geography) of the distribution of plants in the collection. This includes the native location of all plants in the collection and the proportion of endemic and threatened species in the collection.
#'
#'@inheritParams create_native_report
#' @param palette String colour palette for use in the interactive report maps.
#' @param color_binary colours for the representation of species in maps.
#' @param recent_year The year from which to do the recent analysis from.
#'
#' @export
#'
create_trees_report <- function(enriched_report,
                                  collection = NULL,
                                  wgsrpd3 = NULL,
                                  wcvp = NULL,

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
                                  recent_year = as.numeric(format(Sys.Date(), '%Y')) - 3,
                                  min_year = 1970,
                                  earliest_allowable_record = 1700,
                                  old_accession_year_codes = NULL,

                                  scale_colour_continuous = ggplot2::scale_colour_viridis_c,
                                  scale_colour_discrete = ggplot2::scale_colour_viridis_d,
                                  scale_colour_binned = ggplot2::scale_colour_viridis_b,
                                  scale_fill_continuous = ggplot2::scale_fill_viridis_c,
                                  scale_fill_discrete = ggplot2::scale_fill_viridis_d,
                                  scale_fill_binned = ggplot2::scale_fill_viridis_b,
                                  color_binary = c('darkgray','darkgreen'),
                                  palette = 'Greens',
                                  reference_docx = NULL
){
  # 1) Setup.
  # A) Check inputs.

  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'trees_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_trees_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'trees_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_trees_static.docx')
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
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Trees_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      wcvp = wcvp,
                                      wgsrpd3 = wgsrpd3,
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
                                      color_binary = color_binary,
                                      palette = palette,
                                      value_on_fig = value_on_fig,
                                      min_year =min_year,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      recent_year = recent_year),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Trees_report.Rmd"),
                        params = list(enriched_report = enriched_report,
                                      collection = collection,
                                      report_kind = report_kind,
                                      coordinates = coordinates,
                                      wcvp = wcvp,
                                      wgsrpd3 = wgsrpd3,
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
                                      color_binary = color_binary,
                                      palette = palette,
                                      recent_year =recent_year,
                                      min_year = min_year,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
                                      value_on_fig = value_on_fig),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Trees_report.Rmd"),
                      params = list(enriched_report = enriched_report,
                                    collection = collection,
                                    report_kind = report_kind,
                                    coordinates = coordinates,
                                    wcvp = wcvp,
                                    wgsrpd3 = wgsrpd3,
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
                                    color_binary = color_binary,
                                    palette = palette,
                                    recent_year = recent_year,
                                    min_year = min_year,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}
