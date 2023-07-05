#' create_turnover_interactive()
#'
#' @param enriched_report enriched report
#' @param min_year The first year to consider for retrospective review.
#' @param collection The name of the collection
#' @param output_file The file path of the output
#'
#' @export
#'
create_turnover_interactive <- function(enriched_report, min_year=NULL, collection = NULL, output_file = NULL){
  # 1) Setup.
  # A) Check enriched report.

  # B) Choose output file
  if(is.null(output_file) & is.null(collection)){
    output_file = 'turnover_interactive.html'
  }
  else if(is.null(output_file) & !is.null(collection)){
    output_file = paste0(collection, '_turnover_interactive.html')
  }



  # 2) Render basic stats_static document.
  rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Turnover_interactive.Rmd"),
                    params = list(enriched_report = enriched_report,
                                  min_year = min_year,
                                  collection = collection),
                    output_file = output_file,
                    output_dir = getwd())
}
