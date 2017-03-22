#' WebHDFS Options
#'
#' There are many different configurations that are possible. Generally however it is easiest to use
#' one of the \code{clusterconf.*} packages to manage these. To set any configurations permanently one
#' can add \code{setOption(param="value")} values in, for example, an .rProfile.
#'
#' @section options settings:
#' In this section the various parameters that are used are defined. Note that all \code{soh}
#' options will begin with \code{webhdfs.}
#' \describe{
#'   \item{webhdfs.return.type}{Specifies the flavor of \code{data.frame} returned when querying
#'         hive. Most R users are used to working with \code{data.frame} objects. However this function
#'         allows users to specify a default return type of either \code{data.table} for data.table
#'         users or \code{tbl} for dplyr users.}
#'   \item{webhdfs.user}{Some WebHDFS operations and server configurations
#'         require authentication, and others don't. For those that do it is necessary
#'         to provide a username as part of the command url. If no username is given
#'         or an incorrect one is used (i.e., one not associated with an account on
#'         the hadoop cluster) then some operations (e.g., rename or delete) will fail
#'         with exit status 1. There is a mechanism by which this package guesses at
#'         your hadoop username making the assumption that it is the lowercase version
#'         of the computer user name. This should work for most people, but not all.
#'         If you are one of the lucky ones maintaining multiple (different) usernames
#'         then you should set this parameter before trying to use this package.}
#' }
#'
#' @param return_type character string, must be one of \code{"data.frame", "data.table", "tbl"}
#' @param user character string, hadoop user name
#' @export
#' @rdname webhdfs.defaults
apply_default_configurations <- function() {

  # connection status
  for (engine in get_supported_soh_engines())
    set_var(paste0("is.", engine, ".connected"), FALSE)

  # return type
  set_default("webhdfs.return.type", "data.frame", "set_return_type")

  # username
  set_default("webhdfs.user", guess_user(), "set_user")

}

#' Setting getter
#'
#' Typcally this should not be used directly. It is relied upon by the \code{get_*}
#' functions in this package. It is exported mostly to provide transparency around
#' the available parameters.
#'
#' @param varstr Character. The name of the cached variable.
#' @param value Expected value (any type). If provided it overrides the cached value.
#' @param allow_null Logical. If \code{FALSE} a helpful error will result if the return
#'        value is \code{NULL}. Otherwise the \code{NULL} will be returned silently.
#' @param param Character. Configuration parameter name. See \code{\link[clusterconf]{get_cluster_param}}.
#' @param scope Character. Configuration scope. See \code{\link[clusterconf]{get_cluster_param}}
#' @param setter Function. The \code{set_*} function used to keep track of the parameter value. This
#'        is needed so that in cases where a parameter is asked for that has not yet been set, the
#'        configuration lookup need only happen once and then the value us cached.
#' @param ... Passed to \code{\link[clusterconf]{get_cluster_param}}.
#' @importFrom clusterconf get_cluster_param
#' @importFrom clusterconf list_available_clusters
#' @importFrom clusterconf get_cluster_package_name
#' @export
get_setting <- function(varstr, value, allow_null=FALSE, param, scope, setter, ...) {

  # return the given value if there is one
  if (!(missing(value) || is.null(value) || is.na(value) || value=="default"))
    return(value)

  # get cached value
  value <- get_var(varstr)

  # try getting the value from the config
  configs <- get_var("webhdfs.configs")
  if (is.null(value) && !is.null(configs) && !missing(param) && !missing(scope)) {
    value <- get_cluster_param(configs, param, scope, ...)
    if (!is.null(value) && !missing(setter))
      setter(value)
  }

  # friendly error message if nulls are not allowed
  if (!allow_null && is.null(value)) {
    if (is_var(varstr))
      setter_string <- paste0("the '", get_var_setter(varstr), "' function")
    else
      setter_string <- "the appropriate 'set_*' function"

    if (!is.null(get_cluster()))
      config_string <- paste0(" or you can update the ", get_cluster_package_name(get_cluster()),
                              " package if you set configurations using the 'set_cluster()' function")
    else
      config_string <- paste0(" or you can use an appropriate cluster configuration package to",
                              " get the needed settings. Available configuration package(s) (both",
                              " those installed and those remotely available at configured repositories)",
                              " include '", paste0(list_available_clusters(), collapse=", "), "'.")


    stop(paste0("The required setting '", varstr, "' is not set. As such the feature you are ",
                "attempting to use does not work. You can set the parameter manually using ",
                setter_string, config_string))
  }

  return(value)
}

# internal function to check null in getOption and return user setting
# or package-specified default as appropriate
get_default <- function(option_name, default_value) {
  value <- getOption(option_name)
  if (!is.null(value))
    return(value)
  return(default_value)
}

set_default <- function(option_name, default_value, setfun, setfunargs=list()) {
  default_value <- get_default(option_name, default_value)
  if (length(setfunargs)==0)
    setfunargs <- list(default_value)
  else if (!is.null(default_value))
    setfunargs <- c(default_value, setfunargs)
  else
    setfunargs <- c(list(default_value), setfunargs)
  do.call(setfun, args=setfunargs)
  # setfun(get_default(option_name, default_value))
}

#' @export
#' @rdname webhdfs.defaults
set_return_type <- function(return_type) {

  if (is_supported_return_type(return_type))
    set_var("webhdfs.return.type", return_type)

  # check dependent packages
  if (return_type=="data.table" && !require("data.table")) {
    stop("To use the data.table return type, the 'data.table' package must be installed")
  } else if (return_type=="tbl" && !require("dplyr"))
    stop("To use the tbl return type, the 'dplyr' package must be installed")
}

is_supported_return_type <- function(return_type, stop_if_not=TRUE) {

  permitted_types <- c("data.frame", "data.table", "tbl")
  if (return_type %in% permitted_types)
    return(TRUE)

  if (stop_if_not)
    stop(paste0("The return type options are: ", paste0(permitted_types, collapse=", ")))

  return(FALSE)
}

#' @export
#' @rdname webhdfs.defaults
get_return_type <- function() {
  return(get_setting("webhdfs.return.type", NULL))
}

#' @export
#' @rdname webhdfs.defaults
set_user <- function(user) {
  set_var("webhdfs.user", user)
}

#' @export
#' @rdname webhdfs.defaults
get_user <- function() {
  return(get_setting("webhdfs.user", NULL))
}

# Returns the username for the current logged in user in lowercase. This works for both
# windows and unix
guess_user <- function(){
  if(.Platform$OS.type=="windows")
    return(tolower(Sys.getenv("USERNAME")))
  else
    return(tolower(Sys.getenv("USER")))
}
