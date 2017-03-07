


#' Title
#'
#' @import clusterconf
#' @return
#' @export
#'
#' @examples
get_webhdfs_url <- function() {
  nn <- get_name_node_url()

  # pending change to config yaml
  # port <- get_webhdfs_port()
  port <- "50070"

  # build webhdfs url
  webhdfs_url <- paste0(nn, ".mitre.org:", port,
                        "/webhdfs/v1")

  return(webhdfs_url)
}


test_namenode <- function() {


}



