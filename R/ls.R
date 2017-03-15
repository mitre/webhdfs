

#' List a directory
#'
#' Shortcut for WebHDFS LISTSTATUS operation
#'
#' Wildcards (*) in the provided \code{path} are handled by getting all contents at that
#' level of the directory structure, and appending any remaining portions of the
#' \code{path} below that level.
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
#' \dontrun{
#' # basic usage to list directory contents
#' hdfs_ls("/data/")
#'
#' # recursively list files within sub-directories
#' hdfs_ls("/data/blah/2016", recursive = TRUE)
#'
#' # use wildcard to get the first day of every month in 2016
#' hdfs_ls("/data/blah/2016/*/01")
#'
#' # use wildcards to get all days in July in every year
#' hdfs_ls("/data/blah/*/07/*")
#' }
#'
#'
hdfs_ls <- function(path, recursive = FALSE, return_type=get_return_type()) {

  # handle wildcards by
  # - getting the directory above it
  # - get everything from that directory
  # - check the remainder of the supplied path below that directory
  # (maybe make this a separate function, also handle vectorized inputs)
  wildcard_pos <- regexpr("\\*", path)
  if (wildcard_pos > 0) {
    prefix <- substr(path, 1, wildcard_pos-1)
    suffix <- substr(path, wildcard_pos+1, nchar(path))

    # expand wildcard by getting the sub-directories in its place
    expand_path <- hdfs_ls(prefix, recursive = FALSE)
    path_vector <- paste0(prefix, expand_path$pathSuffix, suffix)

    # loop through each expanded path and list its contents
    dat_vector <- data.frame()
    if (length(path_vector) > 0) {
      for (i in 1:length(path_vector)) {
        tmp_result <- hdfs_ls(path_vector[i], recursive = recursive, return_type=return_type)
        dat_vector <- rbind(dat_vector, tmp_result)
      }
    }

    return(dat_vector)
  }

  # main call to HDFS command
  dat <- hdfs_get(path, "LISTSTATUS", return_type=return_type)

  # formatting as POSIX (which could be done by hdfs_get)
  if (all(c("modificationTime", "accessTime") %in% names(dat))) {
    dat$modificationTime <- hdfs_timestamp_to_posix(dat$modificationTime)
    dat$accessTime <- hdfs_timestamp_to_posix(dat$accessTime)
  }

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

