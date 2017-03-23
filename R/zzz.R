.datatable.aware=TRUE

.onLoad <- function(libname, pkgname) {
  assign("pkg_globals", new.env(), envir=parent.env(environment()))
  reset_cache()
}

reset_cache <- function() {
  rm(list=ls(envir=pkg_globals), envir=pkg_globals)

  apply_default_configurations()
}

get_var <- function(varstr) {
  res <- try(get(varstr, envir=pkg_globals), silent=TRUE)
  if(inherits(res, "try-error")){
    return(NULL)
  } else {
    return(res)
  }
}
