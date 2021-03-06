% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.R
\name{indexField}
\alias{indexField}
\title{The firestore indexField function}
\usage{
indexField(fieldPath, mode = "MODE_UNSPECIFIED")
}
\arguments{
\item{fieldPath}{The path of the field. Must match the field path specification described
by [google.firestore.v1beta1.Document.fields][fields]. Special field path __name__ may be
used by itself or at the end of a path. __type__ may be used only at the end of path. {string}}

\item{mode}{one of ASCENDING or DESCENDING {string}}
}
\value{
returns an index field
}
\description{
Create an indexField
}
\details{
Index methods
}
\examples{
\dontrun{
}
}
\author{
Jiasheng Zhu
}
