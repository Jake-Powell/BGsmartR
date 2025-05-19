# Script for running collection dynamics reports

#' Create diversity module
#' @description
#' **Taxonomic Diversity**:Taxonomic presentation and composition in a collection. **Geography**: Geographic representation of a collection.
#'
#'
#'
#' @inheritParams create_native_report
#' @param PlantClassification plant classification information
#' @param endemic_species_per_region A data frame detailing the number of accepted endemic species found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#' @param accepted_species_per_region A data frame detailing the number of accepted species (unique records in WCVP, including subspecies, etc) found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#' @param tree_species_per_region A data frame detailing the number of accepted trees (unique records in WCVP, including subspecies, etc) found in WCVP for each region in wgsrpd3. (if running multiple reports saves having to do the calculation each time)
#' @param do_download A flag (TRUE/FALSE) for whether the download buttons are included.
#' @export
#'
create_taxonomic_diversity_report <- function(enriched_report,
                                   collection = NULL,
                                   wcvp = NULL,
                                   PlantClassification = BGSmartR::Diversity_classification$Classification,
                                   min_year = 1970,
                                   earliest_allowable_record = 1700,
                                   old_accession_year_codes = NULL,

                                   # report_kind = 'static',
                                   # output_file = NULL,
                                   output_dir = NULL
                                   # export_data = FALSE,
                                   # save_excel = FALSE,
                                   # separate_figure_folder = TRUE,
                                   # table_font_size = 10,
                                   # ggtheme = NULL,
                                   # value_on_fig = FALSE,
                                   # interactive_colour = viridis::viridis,
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
  separate_figure_folder = TRUE
  value_on_fig = FALSE
  export_data = FALSE
  save_excel = FALSE
  output_file = NULL
  report_kind = 'interactive'
  color_binary = c('darkgray','darkgreen')
  palette = 'Greens'
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
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
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
                                      earliest_allowable_record = earliest_allowable_record,
                                      old_accession_year_codes = old_accession_year_codes,
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
                                    earliest_allowable_record = earliest_allowable_record,
                                    old_accession_year_codes = old_accession_year_codes,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' @rdname create_taxonomic_diversity_report
#' @export
#'
create_geography_report <- function(enriched_report,
                                    collection = NULL,
                                    wgsrpd3 = NULL,
                                    wcvp = NULL,
                                    detailed_IUCN_redlist = NULL,
                                    # report_kind = 'static',
                                    native = 'Naturally occurring only',
                                    extinct = TRUE,
                                    doubtful_locations = FALSE,
                                    # export_data = FALSE,
                                    # output_file = NULL,
                                    output_dir = NULL,
                                    # table_font_size = 10,
                                    # ggtheme = NULL,
                                    # separate_figure_folder = TRUE,
                                    # value_on_fig = FALSE,
                                    endemic_species_per_region = NULL,
                                    accepted_species_per_region = NULL,
                                    tree_species_per_region = NULL,
                                    do_download = TRUE
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
                                      detailed_IUCN_redlist = detailed_IUCN_redlist,
                                      collection = collection,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      export_data = export_data,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
                                      separate_figure_folder = separate_figure_folder,
                                      output_dir = output_dir,
                                      endemic_species_per_region = endemic_species_per_region,
                                      accepted_species_per_region = accepted_species_per_region,
                                      tree_species_per_region = tree_species_per_region,
                                      do_download = do_download,
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
                                      detailed_IUCN_redlist = detailed_IUCN_redlist,
                                      collection = collection,
                                      native = native,
                                      extinct = extinct,
                                      doubtful_locations = doubtful_locations,
                                      export_data = export_data,
                                      table_font_size = table_font_size,
                                      ggtheme = ggtheme,
                                      separate_figure_folder = separate_figure_folder,
                                      output_dir = output_dir,
                                      endemic_species_per_region = endemic_species_per_region,
                                      accepted_species_per_region = accepted_species_per_region,
                                      tree_species_per_region = tree_species_per_region,
                                      do_download = do_download,
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
                                    detailed_IUCN_redlist = detailed_IUCN_redlist,
                                    collection = collection,
                                    native = native,
                                    extinct = extinct,
                                    doubtful_locations = doubtful_locations,
                                    export_data = export_data,
                                    table_font_size = table_font_size,
                                    ggtheme = ggtheme,
                                    separate_figure_folder = separate_figure_folder,
                                    output_dir = output_dir,
                                    endemic_species_per_region = endemic_species_per_region,
                                    accepted_species_per_region = accepted_species_per_region,
                                    tree_species_per_region = tree_species_per_region,
                                    do_download = do_download,
                                    value_on_fig = value_on_fig),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }



}
