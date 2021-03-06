% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{hdfs_summary}
\alias{hdfs_summary}
\title{Get content summary of a directory}
\usage{
hdfs_summary(path, user = get_user(), return_type = get_return_type())
}
\arguments{
\item{path}{Character containing file system path}

\item{user}{Character username to use in WebHDFS operation.  If not provided,
\code{webhdfs.user} will be used and if that has not been set, a call to
\code{\link{guess_user}} will be made.}

\item{return_type}{character string. See \code{\link{set_return_type}} for
details and options.}
}
\value{
\code{data.frame} (or other requested type) containing output from
  the WebHDFS operation
}
\description{
Shortcut for WebHDFS GETCONTENTSUMMARY operation
}
\examples{
\dontrun{
hdfs_sumary("/data/blah/")
}

}
