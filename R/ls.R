

#' List a directory
#'
#' Shortcut for WebHDFS LISTSTATUS operation
#'
#' @param path Character containing file system path
#' @param resursive Logical indicator of whether to resursively list individual
#'   files within sub-directories.  Default FALSE.
#' @param return_type character string. See \code{\link{set_return_type}} for
#'   details and options.
#'
#' @return \code{data.frame} (or other requested type) containing output from
#'   the WebHDFS operation
#' @export
#'
#' @examples
#' hdfs_ls("/data/raw/tma/SCHEDL")
#'
hdfs_ls <- function(path, recursive = FALSE, return_type=get_return_type()) {

  # TODO: handle wildcards by getting the directory above it, everything from there, with the rest of the path below it

  dat <- hdfs_get(path, "LISTSTATUS", return_type=return_type)

  dat$modificationTime <- hdfs_timestamp_to_posix(dat$modificationTime)
  dat$accessTime <- hdfs_timestamp_to_posix(dat$accessTime)

  if (recursive == TRUE) {
    # split results into files and directories
    dat_file <- dat[dat$type == "FILE"]
    dat_dir <- dat[dat$type == "DIRECTORY"]

    # for each sub-directory
    for (sub_dir in dat_dir$pathSuffix) {
      # search sub-directories for files
      sub_path <- paste0(path, "/", sub_dir)
      sub_result <- hdfs_ls(sub_path, recursive=recursive, return_type=return_type)

      # combine files from sub-directory with files in existing list
      dat_file <- rbind(dat_file, sub_result)
    }

    # return file list
    return(dat_file)
  } else {
    return(dat)
  }

}

