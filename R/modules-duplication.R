#' create_duplication_interactive()
#'
#' @param enriched_report enriched report
#' @param collection The name of the collection
#' @param output_file The file path of the output
#'
#' @export
#'
create_duplication_interactive <- function(enriched_report, collection = NULL, output_file = NULL){
  # 1) Setup.
  # A) Check enriched report.

  # B) Choose output file
  if(is.null(output_file) & is.null(collection)){
    output_file = 'duplication_interactive.html'
  }
  else if(is.null(output_file) & !is.null(collection)){
    output_file = paste0(collection, '_duplication_interactive.html')
  }



  # 2) Render basic stats_static document.
  rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Duplication_interactive.Rmd"),
                    params = list(enriched_report = enriched_report,
                                  collection = collection),
                    output_file = output_file,
                    output_dir = getwd())
}
