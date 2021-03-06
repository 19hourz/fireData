% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.R
\name{createDocument}
\alias{createDocument}
\title{The firestore createDocument function}
\usage{
createDocument(projectID, documentPath, document = "none", name = FALSE,
  databaseID = "(default)", token = "none")
}
\arguments{
\item{projectID}{The Firestore project ID {string}}

\item{documentPath}{path for the new document {string}}

\item{document}{the document to be created}

\item{name}{indicate if name is included in the path, if set false, will automatically generate a name}

\item{databaseID}{The database under which document will be added {string}}

\item{token}{The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}}
}
\value{
returns a http response
}
\description{
The function allows to create new document on firestore databases
}
\examples{
\dontrun{
}
}
\author{
Jiasheng Zhu
}
