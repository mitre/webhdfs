


#' Build URL for WebHDFS
#'
#' @return
#' @export
#'
#' @examples
get_webhdfs_url <- function() {

  # webhdfs base url from namenode
  nn <- get_setting("webhdfs.cluster.nn.url", NULL, param="name_node", scope="cluster", setter=set_name_node_url)
  if (any(!grepl("^http", nn)))
    nn <- paste0("http://", nn)

  port <- get_setting("webhdfs.cluster.webhdfs.port", NULL, param="port", scope="webhdfs", setter=set_name_node_url)
  suffix <- get_setting("webhdfs.cluster.webhdfs.suffix", NULL, param="suffix", scope="webhdfs", setter=set_name_node_url)

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
    # set variable

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
#' @importFrom RCurl getURL
#' @param webhdfs_url Character containing full WebHDFS URL of the namenode to
#'   test, including the port number and suffix
#'
#' @return Logical indicator of whether the namenode is active
#' @export
#'
#' @examples
is_namenode_active <- function(webhdfs_url) {

  test_url <- paste0(webhdfs_url, "/?op=LISTSTATUS")
  dat <- unlist_carefully(fromJSON(getURL(test_url)))

  if ("exception" %in% names(dat)) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}



