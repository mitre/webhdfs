


#' Build an active URL for WebHDFS
#'
#' Full WebHDFS URL is built dynamically from an active namenode for the current
#' cluster.
#'
#' It is possible to define an array including primary and backup namenodes for
#' use by the \code{clusterconf} package.  This function will check the provided
#' namenodes until an active one is found.  The full WebHDFS URL is then built
#' from the active namenode, along with the cluster settings for WebHDFS port
#' number and URL suffix.  This should provide the user with consistent access
#' to WebHDFS services, regardless of which namenode is currently active.
#'
#' @return Character containing full WebHDFS URL from an active cluster namenode
#' @export
#'
#' @examples
#' \dontrun{
#' set_cluster("my prefered cluster")
#' get_webhdfs_url()
#' }
#'
get_webhdfs_url <- function() {

  # test if value has already been set
  out <- get_var("webhdfs.cluster.active.url")
  if (!is.null(out)) {
    return(out)
  }

  # build webhdfs base url from namenode, port, and suffix
  nn <- get_name_node_url()
  port <- get_webhdfs_port()
  suffix <- get_webhdfs_suffix()

  # build candidate webhdfs urls
  candidates <- paste0(nn, ":", port, "/", suffix)

  # test namenode, and use failover if necessary
  found_active <- FALSE
  i <- 0

  while ((found_active == FALSE) & (i < length(candidates))) {
    i <- i + 1
    found_active <- is_namenode_active(candidates[i])
  }

  if (found_active == TRUE) {
    set_var("webhdfs.cluster.active.url", candidates[i])

    return(candidates[i])
  } else {
    warning(paste0("No active namenodes were found within the provided candidates: ",
                   paste(candidates, collapse = ", ")))
    return("")
  }

}




#' Test whether the provided namenode is active
#'
#' If the supplied namenode is currently a standby, this will detect the
#' StandbyException from the test operation and return FALSE.  A successful test
#' operation will cause this function to return TRUE.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @param webhdfs_url Character containing full WebHDFS URL of the namenode to
#'   test, including the port number and suffix
#'
#' @return Logical indicator of whether the namenode is active
#' @export
#'
#' @examples
#' \dontrun{
#' is_namenode_active("http://nn1.server.domain:50070/webhdfs/v1")
#' }
#'
is_namenode_active <- function(webhdfs_url) {

  test_url <- paste0(webhdfs_url, "/",
                      "?user.name=", get_user(),
                      "&op=LISTSTATUS")

  test_content <- content(GET(test_url),
                          as = "text", encoding = "UTF-8")

  # protect against non-json result from a server that is not providing WebHDFS services
  if (startsWith(test_content, "<html>") &
      grepl("Error 404", test_content, ignore.case = TRUE)) {
    stop("WebHDFS is not available at the requested url: ",
         webhdfs_url)
  }

  dat <- unlist_carefully(fromJSON(test_content))

  if ("exception" %in% names(dat)) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}



