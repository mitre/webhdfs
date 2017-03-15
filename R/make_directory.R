


#' Make a directory
#'
#' Shortcut to WebHDFS MKDIRS operation
#'
#' @param path Character path to create
#' @param permission Optional octal specification of permissions for the new
#'   directory.  If none is provided, the WebHDFS default (755) is used.
#' @return Boolean indicator of whether the directory was created successfully
#' @seealso
#' @export
#'
#' @examples
hdfs_makedir <- function(path, permission = NULL) {

  operation <- "MKDIRS"

  if (!is.null(permission)) {
    operation <- paste0(operation, "&permission=", permission)
  }

  result <- hdfs_put(path, operation)

  return(result)
}
