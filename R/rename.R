


#' Rename a file or directory
#'
#' Shortcut to WebHDFS RENAME operation
#'
#' @param old_path Character path of existing file or directory to be renamed
#' @param new_path Character path destination file or directory
#' @param user Character username to use in WebHDFS operation.  If not provided,
#'   \code{webhdfs.user} will be used and if that has not been set, a call to
#'   \code{\link{guess_user}} will be made.
#' @return Boolean indicator of whether the rename operation completed successfully
#' @export
#'
#' @examples
#' \dontrun{
#' hdfs_rename("/data/old", "/data/new")
#' }
hdfs_rename <- function(old_path, new_path, user = get_user()) {

  operation <- "RENAME"

  result <- hdfs_put(old_path,
                     paste0(operation, "&destination=", new_path),
                     user = user)

  return(result)
}
