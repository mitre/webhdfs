
#' @importFrom jsonlite fromJSON
#' @importFrom httr PUT content
#' @param user Character username to use in WebHDFS operation.  If not provided,
#'   \code{webhdfs.user} will be used and if that has not been set, a call to
#'   \code{\link{guess_user}} will be made.
#' @export
#' @rdname hdfs_get
#'
hdfs_put <- function(path, operation, user = get_user()) {

  hdfs_path <- paste0(get_webhdfs_url(), path,
                      "?user.name=", user,
                      "&op=", operation)

  result <- fromJSON(content(PUT(hdfs_path),
                             as = "text", encoding = "UTF-8"))
  return(result)
}
