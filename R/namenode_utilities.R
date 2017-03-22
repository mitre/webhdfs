


#' Build URL for WebHDFS
#'
#' @return
#' @export
#'
#' @examples
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
is_namenode_active <- function(webhdfs_url) {

  test_url <- paste0(webhdfs_url, "/?op=LISTSTATUS")
  dat <- unlist_carefully(fromJSON(content(GET(test_url),
                                           as = "text", encoding = "UTF-8")))

  if ("exception" %in% names(dat)) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}



