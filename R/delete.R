
#' Delete a file or directory
#'
#' Shortcut to WebHDFS DELETE operation
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr DELETE content
#' @param path Character containing file system path
#' @param recursive Boolean indicator of whether to recursively delete sub-directories.  Default FALSE.
#' @param user Character username to use in WebHDFS operation.  If not provided,
#'   \code{webhdfs.user} will be used and if that has not been set, a call to
#'   \code{\link{guess_user}} will be made.
#' @return Boolean indicator of whether the delete operation succeeded
#' @export
#'
hdfs_delete <- function(path, recursive = FALSE, user = NULL) {

  if (is.null(user)) {
    # take from user package setting
    try({user <- get_user()})

    # if user setting is not set, guess
    if (is.null(user)) {
      user <- guess_user()
    }
  }

  hdfs_path <- paste0(get_webhdfs_url(), path,
                      "?user.name=", user,
                      "&op=DELETE&recursive=", tolower(recursive))

  result <- fromJSON(content(DELETE(hdfs_path),
                             as = "text", encoding = "UTF-8"))
  return(result)
}

