
#' @importFrom methods as
format_return <- function(dat, return_type){
  if (return_type=="data.frame") {
    return(dat)
  } else if (return_type=="data.table") {
    data.table::setDT(dat)
    return(dat)
  } else if (return_type=="tbl") {
    return(tibble::as_tibble(dat))
  } else
    return(methods::as(object = dat, Class = return_type))
}


#' Convert HDFS file timestamps to POSIX
#'
#' @param x Numeric, milliseconds since January 1, 1970 UTC.  Will accept a
#'   character that can be converted to numeric.
#'
#' @return POSIXct formatted timestamp
#' @export
#'
#' @examples
#' hdfs_timestamp_to_posix(1489039200390)
#'
hdfs_timestamp_to_posix <- function(x) {
  return(as.POSIXct(as.numeric(x)/1000,
                    tz="UTC", origin="1970-01-01", format="%s"))

}



#' Unlist an object to the innermost \code{data.frame}
#'
#' Some calls to the WebHDFS REST API return a single JSON object, and some
#' return an array of JSON objects.  This is a general function that will return
#' a \code{data.frame} based on the output of either type from
#' \code{\link[jsonlite]{fromJSON}}
#'
#' This is similar to simply calling \code{\link{as.data.frame}} on a
#' \code{list}, but without the nested column names that result.  For example,
#' where \code{\link{as.data.frame}} could generate a column name of
#' \code{FileStatuses.FileStatus.accessTime}, this function will result in a
#' column name of simply \code{accessTime}.
#'
#' @param x Object returned from call to \code{\link[jsonlite]{fromJSON}},
#'   typically a list
#'
#' @return \code{data.frame} containing the innermost \code{data.frame}-like
#'   object within \code{x}
#' @export
#'
#' @examples
#' # sample data.frame nested within multiple lists
#' test <- list(outer = list(inner = data.frame(x = 1, y = 1:10)))
#' test
#'
#' # this function unpacks to a simple data.frame
#' unlist_carefully(test)
#'
#' # compare that to the behavior of as.data.frame
#' as.data.frame(test)
#'
unlist_carefully <- function(x) {

  # grab the innermost object that is either not a list, or has length greater than 1
  while (inherits(x, "list") & (length(x)==1)) {
    x <- x[[1]]
  }

  return(as.data.frame(x))
}
