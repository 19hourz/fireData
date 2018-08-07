
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/19hourz/fireData/master/LICENSE.txt)
[![Build Status](https://travis-ci.org/19hourz/fireData.svg?branch=master)](https://travis-ci.org/19hourz/fireData)
[![codecov](https://codecov.io/gh/19hourz/fireData/branch/master/graph/badge.svg)](https://codecov.io/gh/19hourz/fireData)

# Firedata - Enabling easy cloud stats through Firestore

This project is part of Firedata that adds support for Google Cloud Firestore. Methods are implemented through Google Firestore REST API. Before using, please set up Firebase accordingly. Certain functions requires using OAuth 2.0 access token. To gain the access token, credentials of the project will be needed and can be found from [here](https://console.developers.google.com/apis/credentials). Firestore brought additional features upon realtime database such as querying.

**Check out the repo** [here](https://github.com/19hourz/fireData), commits made during the project are [commits](https://github.com/19hourz/fireData/commits?author=19hourz)

### Things covered in this project:

* All REST endpoints are implemented in corresponding R functions with similar names.
* Unit testing for every function, codecov 100% for lines added in this project.
* Native R encode and decode functions that transfer between R variables (data.frame, vector, array and other primitive types) and Firestore documents.

### Things to be done in the future

* A more integrated query generation function. Since querying is an important feature of Firestore, this feature needs to be better implemented.
* Make decoding and encoding integratable with other languages, i.e, share data across different languages.
* At the time of this project, the write function is not yet implemented
* OAuth 2.0 function that adds scopes for cloud-platform and datastore, necessary for some firestore functions.

## Setup

**Firestore Setup:**
- visit at https://firebase.google.com/
- login and enter the "console"
- add new project
- find the API keys and projectID at the "overview" page clicking on add firebase to web-app
- certain functions requires OAuth 2.0 access which needed client_id and client_secret, these can be found from https://console.developers.google.com/apis/credentials

**R Package Setup:**

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("19hourz/fireData")

library(fireData)
```

## Examples

* Create a document using data frame with path (default)/mydata/mydocument
```R
df <- data.frame(matrix(rnorm(20), nrow=10))
response <- createDocument(projectID, "mydata/mydocument", df, name = TRUE)
```

* Get a document with path (default)/mydata/mydocument, the returned response will be the data frame created previously
```R
response <- getDocument(projectID, "mydata/mydocument")
```
* To return the original response
```R
response <- getDocument(projectID, "mydata/mydocument", decode = FALSE)
```

* Update for an existing document
```R
df <- data.frame(matrix(rnorm(20), nrow=10))
patchDocument(projectID, "mydata/mydocument", df)
```

* Use encode to convert an R variable to firestore document and use decode to convert a http response that contains a document back to R variable. To decode a parsed http response, use option parse = FALSE. A specific example is shown below
```R
createDocument(projectID, "mydata/anotherdocument", df, name = TRUE)
response <- batchGetDocuments(projectID, c("projects/gsoc2018-d05d8/databases/(default)/documents/mydata/mydocument", "projects/gsoc2018-d05d8/databases/(default)/documents/mydata/anotherdocument"))
parsed_response <- httr::content(response, "parsed")
df1 <- decode(parsed_response[[1]]$found, FALSE)
df2 <- decode(parsed_response[[2]]$found, FALSE)
```

* To delete a document
```R
deleteDocument(projectID, "mydata/anotherdocument")
```

* To list all documents under a collectionID
```R
listDocuments(projectID, "mydata", 10)
```

**Other functions may require an OAuth 2.0 token, you can acquire that temporarily from [here](https://developers.google.com/oauthplayground/) (only works for certain security rules)**

* List all collection ids (use "" to list root level collection ids)
```R
listCollectionIds(projectID, "", 1, token = TOKEN)
```

**Index related methods**

* Create index
```R
i <- index("users", c(indexField("X1","ASCENDING"),indexField("X2","ASCENDING")))
response <- createIndex(projectID, i, token = TOKEN)
```

* Get index information using the response from cerateIndex
```R
response <- httr::content(response, "parsed")
name <- response$metadata$index
patterns <- gregexpr('/', name)
pos <- patterns[[1]][length(patterns[[1]])]
indexid <- substring(name, pos + 1)
response <- getIndex(projectID, indexid, token = TOKEN)
```

* List indexes in the project
```R
listIndex(projectID, token = TOKEN)
```

* Delete an index
```R
response <- deleteIndex(projectID, indexid, token = TOKEN)
```

## Aknowledgements:

I hereby want to thank my mentor Robin Kohze and peer Paul Spende for their support during this Google Summer of Code 2018 project.

