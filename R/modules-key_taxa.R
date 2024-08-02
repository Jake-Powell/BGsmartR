#' Create key collections module
#'
#'@description
#'Create an overview of key collections within a living collection including threatened species, endemic species, native species and trees.
#'
#'
#'
#' @param enriched_report The enriched report of a living collection (created in BGsmartR)
#' @param coordinates The coordinates of the collection (long, lat).
#' @param wgsrpd3 World Geographical Scheme for Recording Plant Distributions. Containing polygons of each level 3 area. Can be obtained from `rWCVP` package.
#' @param wcvp World Checklist of Vascular Plants (WCVP) database, obtained using the function [import_wcvp_names()].
#' @param collection The name of the living collection
#' @param native string where 'Naturally occurring only' or 'Introduced only' reduces the locations used to determine native plants.
#' @param extinct  Flag (TRUE/FALSE) for whether to include extinct geographic locations.
#' @param doubtful_locations  Flag (TRUE/FALSE) for whether to include doubtful locations.
#' @param min_year The minimum year for analyses over time.
#' @param export_data  Flag (TRUE/FALSE) for whether to create a `.rda` and `.xlsx` files containing the data used to create the figures in the plot.
#' @param save_excel  Flag (TRUE/FALSE) if export_data == TRUE, save excel allows for a 'xlsx' version of the data to be created or not. Note that for particualarly large collections, saving an excel version may cause the function to fail.
#' @param output_file The file path of the native report.
#' @param table_font_size Font size for tables.
#' @param ggtheme gg plot theme to be applied to all ggplots, see `ggthemes` package for pre-set themes.
#' @param separate_figure_folder  Flag (TRUE/FALSE) for whether the figures should also be outputted in seperate files.
#' @param scale_colour_continuous,scale_colour_discrete,scale_colour_binned,scale_fill_continuous,scale_fill_discrete,scale_fill_binned  scales for ggplot. Default is to use viridis.
#' @param reference_docx path to a .docx file whose style (design) the report copies.
#' @param output_dir The output directory
#' @param value_on_fig Flag (TRUE/FALSE) for whether to include values of static plots.
#' @param report_kind The find of report to create, either `static` or `interactive`.
#' @param interactive_colour colours for interactive report, default is viridis::viridis.
#' @param old_accession_year_codes Vector of numbers corresponding to values within "AccYear" that indicate that a record is old with unknown accession year. For our analyses over time we assume these records are accessioned on `earliest_allowable_record`.
#' @param earliest_allowable_record Number, corresponding to the minimum allowable year in the "AccYear" column.
#' @param bru_codes A vector of the BRU level 3 codes that want to be considered as the native regions in the report.
#' @param endemic_species_per_region A data frame detailing the number of accepted endemic species found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#' @param palette String colour palette for use in the interactive report maps.
#' @param color_binary colours for the representation of species in maps.
#'
#' @return html report
#' @export
#'
create_native_report <- function(enriched_report,
                                 coordinates = NULL,
                                 wgsrpd3 = NULL,
                                 wcvp = NULL,
                                 collection = NULL,
                                 report_kind = 'static',
                                 native = 'Naturally occurring only',
                                 extinct = TRUE,
                                 doubtful_locations = FALSE,
                                 min_year = 1970,
                                 export_data = FALSE,
                                 save_excel = FALSE,
                                 output_file = NULL,
                                 output_dir = NULL,
                                 table_font_size = 10,
                                 ggtheme = NULL,
                                 separate_figure_folder = TRUE,
                                 value_on_fig = FALSE,
                                 earliest_allowable_record = 1700,
                                 old_accession_year_codes = NULL,
                                 bru_codes = NA,

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


  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'native_interactive.html'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_native_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(collection)){
      output_file = 'native_static.docx'
    }
    else if(is.null(output_file) & !is.null(collection)){
      output_file = paste0(collection, '_native_static.docx')
    }
  }else{
    stop('Invalid report_kind input!')
  }


  # 2) Render basic stats_static document.
  if(report_kind == 'static'){
    if(!is.null(reference_docx)){
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Native_report.Rmd"),
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
                                      interactive_colour = interactive_colour,
                                      scale_colour_continuous = scale_colour_continuous,
                                      scale_colour_discrete = scale_colour_discrete,
                                      scale_colour_binned = scale_colour_binned,
                                      scale_fill_continuous = scale_fill_continuous,
                                      scale_fill_discrete = scale_fill_discrete,
                                      scale_fill_binned = scale_fill_binned,
                                      separate_figure_folder = separate_figure_folder,
                                      # do_geography_trend =do_geography_trend,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,                                      output_dir = output_dir,
                                      value_on_fig = value_on_fig,
                                      report_kind = report_kind,
                                      bru_codes = bru_codes,
                                      save_excel = save_excel),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }else{
      rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Native_report.Rmd"),
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
                                      interactive_colour = interactive_colour,
                                      scale_colour_continuous = scale_colour_continuous,
                                      scale_colour_discrete = scale_colour_discrete,
                                      scale_colour_binned = scale_colour_binned,
                                      scale_fill_continuous = scale_fill_continuous,
                                      scale_fill_discrete = scale_fill_discrete,
                                      scale_fill_binned = scale_fill_binned,
                                      separate_figure_folder = separate_figure_folder,
                                      # do_geography_trend =do_geography_trend,
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,                                      output_dir = output_dir,
                                      value_on_fig = value_on_fig,
                                      report_kind = report_kind,
                                      bru_codes = bru_codes,
                                      save_excel = save_excel),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Native_report.Rmd"),
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
                                    interactive_colour = interactive_colour,
                                    scale_colour_continuous = scale_colour_continuous,
                                    scale_colour_discrete = scale_colour_discrete,
                                    scale_colour_binned = scale_colour_binned,
                                    scale_fill_continuous = scale_fill_continuous,
                                    scale_fill_discrete = scale_fill_discrete,
                                    scale_fill_binned = scale_fill_binned,
                                    separate_figure_folder = separate_figure_folder,
                                    # do_geography_trend =do_geography_trend,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    output_dir = output_dir,
                                    value_on_fig = value_on_fig,
                                    report_kind = report_kind,
                                    bru_codes = bru_codes,
                                    save_excel = save_excel),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' @rdname create_native_report
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
                                      old_accession_year_codes = old_accession_year_codes),
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
                                    min_year = min_year,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' @rdname create_native_report
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
                                      old_accession_year_codes = old_accession_year_codes),
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
                                    min_year = min_year,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' @rdname create_native_report
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
                                      old_accession_year_codes = old_accession_year_codes),
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
                                    min_year = min_year,
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}
