#' Chart colours for reports
#'
#' @param n number of colours (allowable values = 1,2,3,4)
#'
#' @return colours
#'
interactive_colour <- function(n){
  color = c('#f46d43', "#FDAE6B", '#848484', '#e6e6e6')
  if(n==2){
    return(color[c(1,4)])
  }
  color[1:n]
}
