
#' create_snapshot_interactive()
#'
#'This function can be used to create the snapshot report of a living collection. The report produces interactive graphs (bars, pies and geography) of the distribution of plants in the collection. This includes the native location of all plants in the collection and the proportion of endemic and threatened species in the collection.
#'
#' @param enriched_report enriched report
#' @param collection The name of the collection
#' @param collection_coords The coordinates of the collection (long, lat)
#' @param output_file The file path of the output
#' @param save_widgets Logical for whether we want to create the folder 'Widgets' containing the widgets from the report.
#' @param create_excel Logical for whether we want to create an excel file containing the tables used to create the widgets in the report.
#' @param output_dir Path to the output directory.

#'
#' @export
#'
create_snapshot_interactive <- function(enriched_report, collection = NULL, collection_coords = NA, output_file = NULL, save_widgets = FALSE, create_excel = FALSE, output_dir = NULL){
  # 1) Setup.
  # A) Check enriched report.

  # B) Choose output file
  if(is.null(output_file) & is.null(collection)){
    output_file = 'snapshot_interactive.html'
  }
  else if(is.null(output_file) & !is.null(collection)){
    output_file = paste0(collection, '_snapshot_interactive.html')
  }

  if(is.null(output_dir)){
    output_dir = getwd()
  }


  # 2) Render basic stats_static document.
  rmarkdown::render(paste0(system.file(package = "BGSmartR"), "/markdown_reports/Snapshot_interactive.Rmd"),
                    params = list(enriched_report = enriched_report,
                                  collection = collection,
                                  collection_coords = collection_coords,
                                  save_widgets = save_widgets,
                                  create_excel = create_excel),
                    output_file = output_file,
                    output_dir = output_dir)
}
