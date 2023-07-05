#' create_key_taxa_interactive()
#'
#' @param enriched_report enriched report
#' @param min_year The first year to consider for retrospective review.
#' @param collection The name of the collection
#' @param output_file The file path of the output
#'
#' @export
#'
create_key_taxa_interactive <- function(enriched_report, min_year=NULL, collection = NULL, output_file = NULL){
  # 1) Setup.
  # A) Check enriched report.

  # B) Choose output file
  if(is.null(output_file) & is.null(collection)){
    output_file = 'key_taxa_interactive.html'
  }
  else if(is.null(output_file) & !is.null(collection)){
    output_file = paste0(collection, '_key_taxa_interactive.html')
  }



  # 2) Render basic stats_static document.
  rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Key_taxa_interactive.Rmd"),
                    params = list(enriched_report = enriched_report,
                                  min_year = min_year,
                                  collection = collection),
                    output_file = output_file,
                    output_dir = getwd())
}


#' create_key_taxa_interactive()
#'
#' @param enriched_report enriched report
#' @param collection The name of the collection
#' @param output_file The file path of the output
#'
#' @export
#'
create_key_taxa_compare_interactive <- function(enriched_report, collection = NULL, output_file = NULL){
  # 1) Setup.
  # A) Check enriched report.

  # B) Choose output file
  if(is.null(output_file) & is.null(collection)){
    output_file = 'key_taxa_compare_interactive.html'
  }
  else if(is.null(output_file) & !is.null(collection)){
    output_file = paste0(collection, '_key_taxa_compare_interactive.html')
  }



  # 2) Render basic stats_static document.
  rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Key_Taxa_interactive_compare_red.Rmd"),
                    params = list(enriched_report = enriched_report,
                                  collection = collection),
                    output_file = output_file,
                    output_dir = getwd())
}
