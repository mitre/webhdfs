

format_return <- function(dat, return_type){
  if (return_type=="data.frame") {
    return(dat)
  } else if (return_type=="data.table") {
    data.table::setDT(dat)
    return(dat)
  } else if (return_type=="tbl") {
    return(dplyr::as.tbl(dat))
  } else
    return(as(object = dat, Class = return_type))
}


#' Convert HDFS file timestamps to POSIX
#'
#' @param x Numeric, milliseconds since January 1, 1970 UTC
#'
#' @return POSIXct formatted timestamp
#' @export
#'
#' @examples
hdfs_timestamp_to_posix <- function(x) {
  return(as.POSIXct(x/1000, tz="UTC", origin="1970-01-01", format="%s"))

}
