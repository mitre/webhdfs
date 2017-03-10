

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

  # handle wildcards by getting the directory above it, everything from there, with the rest of the path below it
  # maybe make this a separate function, also handle vector
  wildcard_pos <- regexpr("\\*", path)
  if (wildcard_pos > 0) {
    prefix <- substr(path, 1, wildcard_pos-1)
    suffix <- substr(path, wildcard_pos+1, nchar(path))

    # expand wildcard by getting the sub-directories in its place
    expand_path <- hdfs_ls(prefix, recursive = FALSE)
    path_vector <- paste0(prefix, expand_path$pathSuffix, suffix)

    # TODO: need to handle file not found exception
    dat_vector <- hdfs_ls(path_vector[1], recursive = recursive, return_type=return_type)
    if (length(path_vector) > 1) {
      for (i in 2:length(path_vector)) {
        tmp_result <- hdfs_ls(path_vector[i], recursive = recursive, return_type=return_type)
        dat_vector <- rbind(dat_vector, tmp_result)
      }
    }

    return(dat_vector)
  }

  # main call to HDFS command
  dat <- hdfs_get(path, "LISTSTATUS", return_type=return_type)

  # formatting as POSIX (which could be done by hdfs_get)
  dat$modificationTime <- hdfs_timestamp_to_posix(dat$modificationTime)
  dat$accessTime <- hdfs_timestamp_to_posix(dat$accessTime)

  # optionally recurse through sub-directories
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

