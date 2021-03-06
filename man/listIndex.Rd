% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.R
\name{listIndex}
\alias{listIndex}
\title{The firestore list index function}
\usage{
listIndex(projectID, filter = NULL, pageSize = NULL, pageToken = NULL,
  databaseID = "(default)", token = "none")
}
\arguments{
\item{projectID}{The Firestore project ID {string}}

\item{filter}{string used to filter index {string}}

\item{pageSize}{The standard List page size}

\item{pageToken}{The standard List page token {string}}

\item{databaseID}{The database under which this operation will be performed {string}}

\item{token}{The user access token {string}}
}
\value{
If successful, the response body will be empty
}
\description{
List index on firestore
}
\examples{
\dontrun{
}
}
\author{
Jiasheng Zhu
}
