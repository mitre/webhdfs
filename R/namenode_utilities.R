


#' Build URL for WebHDFS
#'
#' @return
#' @export
#'
#' @examples
get_webhdfs_url <- function() {

  # webhdfs base url from namenode
  nn <- get_setting("webhdfs.cluster.nn.url", NULL, param="name_node", scope="cluster", setter=set_name_node_url)
  if (!grepl("^http", vm))
    vm <- paste0("http://", vm)

  # TODO: test namenode, and use failover if necessary

  # TODO: pending change to config yaml
  # port <- get_webhdfs_port()
  port <- "50070"

  # build webhdfs url
  webhdfs_url <- paste0(nn, ".mitre.org:", port,
                        "/webhdfs/v1")

  return(webhdfs_url)
}




# test_namenode <- function() {
#
#
# }



