

#' Get content summary of a directory
#'
#' Shortcut for WebHDFS GETCONTENTSUMMARY operation
#'
#' @param path Character containing file system path
#' @param user Character username to use in WebHDFS operation.  If not provided,
#'   \code{webhdfs.user} will be used and if that has not been set, a call to
#'   \code{\link{guess_user}} will be made.
#' @param return_type character string. See \code{\link{set_return_type}} for
#'   details and options.
#'
#' @return \code{data.frame} (or other requested type) containing output from
#'   the WebHDFS operation
#' @export
#'
#' @examples
#' \dontrun{
#' hdfs_sumary("/data/blah/")
#' }
#'
hdfs_summary <- function(path, user = get_user(), return_type=get_return_type()) {

  dat <- hdfs_get(path, "GETCONTENTSUMMARY", user = user, return_type=return_type)

  return(dat)
}
