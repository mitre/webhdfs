

#' Get content summary of a directory
#'
#' Shortcut for WebHDFS GETCONTENTSUMMARY operation
#'
#' @param path Character containing file system path
#' @param return_type character string. See \code{\link{set_return_type}} for
#'   details and options.
#'
#' @return \code{data.frame} (or other requested type) containing output from
#'   the WebHDFS operation
#' @export
#'
#' @examples
#' hdfs_sumary("/data/raw/tma/SCHEDL")
#'
hdfs_summary <- function(path, return_type=get_return_type()) {

  dat <- hdfs_get(path, "GETCONTENTSUMMARY", return_type=return_type)

  return(dat)
}
