



#' Generic function to perform HTTP GET request of a WebHDFS operation
#'
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @param path Character containing file system path
#' @param return_type character string. See \code{\link{set_return_type}} for
#'   details and options.
#'
#' @return \code{data.frame} (or other requested type) containing output from
#'   the WebHDFS operation
#' @export
#'
#' @examples
#' hdfs_ls("/data/raw/tma/SCHEDL")
#'
hdfs_ls <- function(path, return_type=get_return_type()) {

  dat <- hdfs_get(path, "LISTSTATUS", return_type=return_type)

  dat$modificationTime <- hdfs_timestamp_to_posix(dat$modificationTime)

  return(dat)
}

