


#' Generic function to perform HTTP GET request of a WebHDFS operation
#'
#' The full URL is built from the base WebHDFS URL via the
#' \code{\link{get_webhdfs_url}} function, and the supplied \code{path} and
#' \code{operation} parameters.
#'
#' If the WebHDFS command returns an exception, this function issues a warning
#' containing the exception message and returns an empty \code{data.frame}.
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
#' \url{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html}
#' for documentation of WebHDFS REST API commands that can be passed to the
#' \code{operation} parameter
#'
#' @examples
#' hdfs_get("/data/raw/tma/SCHEDL", "LISTSTATUS")
#'
hdfs_get <- function(path, operation, return_type=get_return_type()) {

  hdfs_path <- paste0(get_webhdfs_url(), path, "?op=", toupper(operation))

  dat_list <- fromJSON(getURL(hdfs_path))
  dat <- unlist_carefully(dat_list)

  # check for exception (e.g. FileNotFoundException)
  if ("exception" %in% names(dat)) {
    # issue warning with exception message
    warning(dat$message)

    # return an empty data.frame, rather than a data.frame containing the exception message
    # this is subtle, but facilitates repeated calls to this function as with wildcard expansion where some elements may not exist
    dat <- data.frame()
  }

  format_return(dat, return_type)
}
