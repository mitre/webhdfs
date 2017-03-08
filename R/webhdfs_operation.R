


#' Generic function to perform HTTP GET request of a WebHDFS operation
#'
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @param path Character containing file system path
#' @param operation Character containing WebHDFS operation name
#' @param return_type character string. See \code{\link{set_return_type}} for
#'   details and options.
#'
#' @return \code{data.frame} (or other requested type) containing output from
#'   the WebHDFS operation
#' @export
#' @seealso
#'   \url{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html}
#'   for documentation of WebHDFS REST API commands that can be passed to the
#'   \code{operation} parameter
#'
#' @examples
#' hdfs_get("/data/raw/tma/SCHEDL", "LISTSTATUS")
#'
hdfs_get <- function(path, operation, return_type=get_return_type()) {

  hdfs_path <- paste0(get_webhdfs_url(), path, "?op=", toupper(operation))

  dat_list <- fromJSON(getURL(hdfs_path))
  dat <- unlist_carefully(dat_list)

  format_return(dat, return_type)
}
