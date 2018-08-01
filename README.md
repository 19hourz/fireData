
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/19hourz/fireData/master/LICENSE.txt)
[![Build Status](https://travis-ci.org/19hourz/fireData.svg?branch=master)](https://travis-ci.org/19hourz/fireData)
[![codecov](https://codecov.io/gh/19hourz/fireData/branch/master/graph/badge.svg)](https://codecov.io/gh/19hourz/fireData)

# Firedata - Enabling easy cloud stats through Firestore

This project is part of Firedata that adds support for Google Cloud Firestore. Methods are implemented through Google Firestore REST API. Before using, please set up Firebase accordingly. Certain functions requires using OAuth 2.0 access token. To gain the access token, credentials of the project will be needed and can be found from [here](https://console.developers.google.com/apis/credentials). Firestore brought additional features upon realtime database such as querying.

## Setup

**Firestore Setup:**
- visit at https://firebase.google.com/
- login and enter the "console"
- add new project
- find the API keys and projectID at the "overview" page clicking on add firebase to web-app
- certain functions requires OAuth 2.0 access which needed client_id and client_secret, these can be found from https://console.developers.google.com/apis/credentials

**R Package Setup:**

```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("19hourz/fireData")

library(fireData)
```

## Examples

* Create a document using data frame with path (default)/mydata/mydocument
```
df <- data.frame(matrix(rnorm(20), nrow=10))
response <- createDocument(projectID, "mydata", df, documentName = "mydocument")
```

* Get a document with path (default)/mydata/mydocument, the returned response will be the data frame created previously
```
response <- getDocument(projectID, "mydata/mydocument")
```
* To returned the original response:
```
response <- getDocument(projectID, "mydata/mydocument", decode = FALSE)
```

* Use encode to convert an R variable to firestore document and use decode to convert a http response that contains a document back to R variable. To decode a parsed http response, use option **parse = FALSE**. A specific example is shown below:
```
# batch getting two documents
response <- batchGetDocuments(projectID, c("projects/gsoc2018-d05d8/databases/(default)/documents/mydata/mydocument1", "projects/gsoc2018-d05d8/databases/(default)/documents/mydata/mydocument2"))

parsed_response <- httr::content(response, "parsed")

# get the varible contained in mydocument1
decode(parsed_response[[1]]$found, FALSE)

# get the varible contained in mydocument2
decode(parsed_response[[2]]$found, FALSE)
```
