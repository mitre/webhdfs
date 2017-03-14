#' Specify a Hadoop cluster
#'
#' The easy way to set started - skipping the call to many different \code{set_}
#' functions.
#'
#' This function will take either a cluster name or a path to a configuration file.
#' If a name is provided then a search will be made for a package containing
#' cluster configurations (named \code{clusterconf.<your cluster name>}). The search
#' starts in you installed packages (\code{.libPaths()}) and then proceeds to
#' check your configured repositories (\code{getOption("repos")}).
#'
#' If you provide a configuration file path instead then that is used in lieu of
#' searching for a package and the necessary settings are parsed out of it.
#'
#' @param name Character. Specify the name of the cluster to "point" to.
#' @param config_yaml Character. Path to a YAML file specifying cluster settings.
#' @importFrom clusterconf get_cluster_configs
#' @export
set_cluster <- function(name, config_yaml) {

  if (!missing(config_yaml))
    configs <- get_cluster_configs(yaml_path=config_yaml)
  else
    configs <- get_cluster_configs(cluster_name=name)

  # clear cache
  reset_cache()

  # configure webhdfs to use the specified cluster
  set_var("webhdfs.configs", configs)

  # set cluster name
  set_var("webhdfs.cluster", name)
}

#' @rdname set_cluster
#' @export
get_cluster <- function() {
  return(get_var("webhdfs.cluster"))
}

#' Manually specify cluster settings for namenode
#'
#' @export
#' @rdname set_name_node_url
set_name_node_url <- function(url) {
  set_var("webhdfs.cluster.nn.url", url)
}

#' @export
#' @rdname set_name_node_url
get_name_node_url <- function() {
  nn <- get_setting("webhdfs.cluster.nn.url", NULL, param="name_node", scope="cluster", setter=set_name_node_url)
  if (any(!grepl("^http", nn)))
    nn <- paste0("http://", nn)
  return(nn)
}



#' Manually specify cluster settings for WebHDFS port number
#'
#' @export
#' @rdname set_webhdfs_port
set_webhdfs_port <- function(port) {
  set_var("webhdfs.cluster.webhdfs.port", port)
}

#' @export
#' @rdname set_webhdfs_port
get_webhdfs_port <- function() {
  port <- get_setting("webhdfs.cluster.webhdfs.port", NULL, param="port", scope="webhdfs", setter=set_webhdfs_port)
  return(port)
}


#' Manually specify cluster settings for WebHDFS URL suffix
#'
#' @export
#' @rdname set_webhdfs_suffix
set_webhdfs_suffix <- function(suffix) {
  set_var("webhdfs.cluster.webhdfs.suffix", suffix)
}

#' @export
#' @rdname set_webhdfs_suffix
get_webhdfs_suffix <- function() {
  suffix <- get_setting("webhdfs.cluster.webhdfs.suffix", NULL, param="suffix", scope="webhdfs", setter=set_webhdfs_port)
  return(suffix)
}


