
#' Generic functions to perform HTTP GET or PUT requests of a WebHDFS operation
#'
#' The full URL is built from the base WebHDFS URL via the
#' \code{\link{get_webhdfs_url}} function, and the supplied \code{path} and
#' \code{operation} parameters.
#'
#' If the WebHDFS command returns an exception, this function issues a warning
#' containing the exception message and returns an empty \code{data.frame}.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @param path Character containing file system path
#' @param operation Character containing WebHDFS operation name.  This can
#'   include the operation itself, along with additional parameters as
#'   necessary.  If parameters are included, they should be separated from the
#'   operation and each other with '&', such as
#'   'OPERATION&param1=value1&param2=value2'.
#' @param return_type character string. See \code{\link{set_return_type}} for
#'   details and options.
#' @param handler Function to use when processing the result of the WebHFDS
#'   operation.  If not provided, defaults to jsonlite::fromJSON, which handles
#'   output from the vast majority of GET operations.
#'
#' @return \code{data.frame} (or other requested type) containing output from
#'   the WebHDFS operation
#' @export
#' @rdname hdfs_get
#' @seealso
#' \url{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html}
#' for documentation of WebHDFS REST API commands that can be passed to the
#' \code{operation} parameter
#'
#' @examples
#' \dontrun{
#' # basic usage with operation
#' hdfs_get("/data/", "LISTSTATUS")
#'
#' # usage with operation appended to parameter
#' hdfs_put("/user/uid/newdir", "MKDIRS&permission=754")
#' }
#'
hdfs_get <- function(path, operation, user = get_user(), return_type = get_return_type(),
                     handler = NULL) {

  hdfs_path <- paste0(get_webhdfs_url(), path,
                      "?user.name=", user,
                      "&op=", operation)

  # raw content from the WebHDFS command
  dat_content <- get_url_content(hdfs_path)

  # convert content to a more usable form
  if (is.null(handler)) {
    dat_list <- fromJSON(dat_content)
  } else {
    dat_list <- do.call(handler, list(dat_content))
  }

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


#' Utility wrapper around httr::GET
#' 
#' Handle any additional configuration specified in package settings
#' 
#' @importFrom httr GET content
#'
#' @param url Character containing URL to get
#' 
#' @return Character containing HTTP response
#' @noRd
#' 
get_url_content <- function(url) {

  # check setting for SSL verification
  use_ssl_verification <- get_ssl_verify()

  if (use_ssl_verification) {
    response <- GET(url)
  } else {
    response <- GET(
      url,
      httr::config(ssl_verifypeer = 0)
    )
  }

  # return raw content from the response
  return(content(response, as = "text", encoding = "UTF-8"))
}