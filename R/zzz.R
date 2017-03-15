.datatable.aware=TRUE

.onLoad <- function(libname, pkgname) {
  assign("pkg_globals", new.env(), envir=parent.env(environment()))
  reset_cache()

  set_default("webhdfs.user", guess_user(), "set_user")
}

reset_cache <- function() {
  rm(list=ls(envir=pkg_globals), envir=pkg_globals)
}

get_var <- function(varstr) {
  res <- try(get(varstr, envir=pkg_globals), silent=TRUE)
  if(inherits(res, "try-error")){
    return(NULL)
  } else {
    return(res)
  }
}
