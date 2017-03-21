


#' Make a directory
#'
#' Shortcut to WebHDFS MKDIRS operation
#'
#' @param path Character path to create
#' @param user Character username to use in WebHDFS operation.  If not provided,
#'   \code{webhdfs.user} will be used and if that has not been set, a call to
#'   \code{\link{guess_user}} will be made.
#' @param permission Optional octal specification of permissions for the new
#'   directory.  If none is provided, the WebHDFS default (755) is used.
#' @return Boolean indicator of whether the directory was created successfully
#' @seealso
#' @export
#'
#' @examples
hdfs_makedir <- function(path, user = NULL, permission = NULL) {

  operation <- "MKDIRS"

  if (!is.null(permission)) {
    operation <- paste0(operation, "&permission=", permission)
  }

  result <- hdfs_put(path, operation, user = user)

  return(result)
}
