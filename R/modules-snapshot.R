
#' create_snapshot_interactive()
#'
#' @param enriched_report enriched report
#' @param collection The name of the collection
#' @param output_file The file path of the output
#' @param save_widgets Logical for whether we want to create the folder 'Widgets' containing the widgets from the report.
#' @param create_excel Logical for whether we want to create an excel file containing the tables used to create the widgets in the report.
#'
#' @export
#'
create_snapshot_interactive <- function(enriched_report, collection = NULL, output_file = NULL, save_widgets = FALSE, create_excel = FALSE){
  # 1) Setup.
  # A) Check enriched report.

  # B) Choose output file
  if(is.null(output_file) & is.null(collection)){
    output_file = 'basic_stats_interactive.html'
  }
  else if(is.null(output_file) & !is.null(collection)){
    output_file = paste0(collection, '_basic_stats_interactive.html')
  }



  # 2) Render basic stats_static document.
  rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Snapshot_interactive.Rmd"),
                    params = list(enriched_report = enriched_report,
                                  collection = collection,
                                  save_widgets = save_widgets,
                                  create_excel = create_excel),
                    output_file = output_file,
                    output_dir = getwd())
}
