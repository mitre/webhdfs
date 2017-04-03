

#' Open and read a file
#'
#' Shortcut to the WebHDFS OPEN command.
#'
#'
#' @param path Character containing file system path
#' @param handler Function name to be used to load data from WebHDFS.  It is
#'   incumbent on the user for this function to be available in the current
#'   session, and for the function to handle the type of data referenced by
#'   \code{path}.  This is passed on to \code{\link{hdfs_get}}.
#' @param offset_bytes The starting position to be processed, in bytes.  Default
#'   (NULL) means start from the beginning of the file.
#' @param length_bytes The number of bytes to be processed.  Default (NULL)
#'   means the entire file will be read.
#' @param buffer_size The size of the buffer to be used in transferring data.
#' @param user Character username to use in WebHDFS operation.  If not provided,
#'   \code{webhdfs.user} will be used and if that has not been set, a call to
#'   \code{\link{guess_user}} will be made.
#' @param return_type character string. See \code{\link{set_return_type}} for
#'   details and options.
#'
#' @return \code{data.frame} (or other requested type) containing output from
#'   the WebHDFS operation
#' @seealso
#' \url{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Open_and_Read_a_File}
#' for documentation of WebHDFS OPEN command and its parameters.
#'
#' @return
#' @export
#'
#' @examples
hdfs_open <- function(path, handler,
                      offset_bytes = NULL, length_bytes = NULL, buffer_size = NULL,
                      user = get_user(),
                      return_type=get_return_type()) {


  # build command for hdfs_get with OPEN command and any supplied options
  command <- paste0("OPEN",
                    ifelse(!is.null(offset), paste0("&offset=", offset), ""),
                    ifelse(!is.null(length), paste0("&length=", length), ""),
                    ifelse(!is.null(buffer_size), paste0("&buffersize=", buffer_size), ""))

  dat <- hdfs_get(path, command, user = user, return_type = return_type, handler = handler)

  return(dat)
}
