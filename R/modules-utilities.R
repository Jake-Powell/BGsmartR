
#' exist_at_date()
#' Function that gives the records that exist at a certain date.
#' @param date dates for which we want the existing records.
#' @param acc accessions
#' @param ItemStatusDate ItemStatusDate
#' @param ItemStatusType ItemStatusType
#'
#' @return data frame where each column corresponds to a date, where the vaules is a logical vector of whether the record exists at that date.
#' @export
exist_at_date <- function(date, acc, ItemStatusDate, ItemStatusType){

  date = as.Date(date)
  # Accession date.
  pre_date = rep(NA,length(acc)) ; pre_date_char = rep(NA,length(acc))
  pre_date[which(acc > 1650)] =as.Date(paste(acc[which(acc > 1650)], '01', '01', sep = "-"), "%Y-%m-%d")
  pre_date_char[which(acc > 1650)] = paste(acc[which(acc > 1650)], '01', '01', sep = "-")

  # use the current day, unless not existing then use the date of that entry.
  post_date = rep(as.character(Sys.Date()),length(acc))
  post_date[ItemStatusType == 'NotExisting'] = ItemStatusDate[ItemStatusType == 'NotExisting']
  # If only a year is given assume it occurs on the 1st of Jan.
  post_date[stringr::str_length(post_date) == 4] = paste( post_date[stringr::str_length(post_date) == 4], '01', '01', sep = "-")
  # If only a year and month is given assume the dat is the 1st.
  post_date[stringr::str_length(post_date) == 7] = paste( post_date[stringr::str_length(post_date) == 7], '01', sep = "-")
  dates = data.frame(pre = pre_date_char, post = post_date, pre_date = pre_date, post_date = as.Date(post_date, "%Y-%m-%d"))

  # Vector of whether the plant is existing on the date.
  out = matrix(NA, nrow = length(acc), ncol = length(date))
  for(i in 1:length(date)){
    existing_on_date = rep(FALSE, length(acc))
    existing_on_date[which(date[i] >= dates$pre_date & date[i] < dates$post_date)] = TRUE

    out[,i] = existing_on_date
  }

  out = data.frame(out)
  names(out) = date

  return(out)
}


#' add_alpha
#'
#' @param col a vector of colours
#' @param alpha transparancy wanted
#'
#' @return a vector of colours with added transparancy
#' @export
add_alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, grDevices::col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}
