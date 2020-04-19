set_var <- function(varstr, val, caller, check=TRUE) {

  if (check && (missing(val) || is.null(val) || any(is.na(val)))) {
    val <- NULL
  }

  # cache value
  assign(varstr, val, pkg_globals)

  # remember where setting came from
  if (missing(caller))
    caller <- deparse(sys.call(-1)[[1]])
  assign(paste0(varstr, ".setter"), caller, pkg_globals)
}

set_connection_var <- function(varstr, val, engine) {
  set_var(varstr, val, deparse(sys.call(-1)[[1]]))
  set_var(paste0("is.", engine, ".connected"), FALSE)
}

is_var <- function(varstr) {
  return(is.null(get_var(varstr)))
}

get_var <- function(varstr) {
  res <- try(get(varstr, pkg_globals), silent=TRUE)
  if(inherits(res, "try-error")){
    return(NULL)
  } else {
    return(res)
  }
}

get_var_setter <- function(varstr) {
  return(get_var(paste0(varstr, ".setter")))
}
