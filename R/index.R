## Below are implementation for Cloud Firestore

# firestore_root <- "https://firestore.googleapis.com/"
# version_prefix <- "v1beta1/"
# projects <- "projects/"
# databases <- "databases/"
# documents <- "documents/"
# authPrefix <- "Bearer "

#' @title The firestore createDocument function
#' @author Jiasheng Zhu
#' @description The function allows to create new document on firestore databases
#' @param projectID The Firestore project ID {string}
#' @param documentPath path for the new document {string}
#' @param documentName name for the new document {string}
#' @param document the document to be created
#' @param databaseID The database under which document will be added {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @return returns a http response
#' @export
#' @examples
#' \dontrun{
#' }
createDocument <- function(projectID, documentPath, document = "none", documentName = "none", databaseID = "(default)", token = "none") {
  if(document != "none"){
    document <- generateDocument(document)
  }
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) == "/"){
    documentPath <- substring(documentPath, 0, nchar(documentPath)-1)
  }
  if (documentName == "none") {
    URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents/", documentPath)
  } else {
    URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents/", documentPath, "?documentId=", documentName)
  }
  if (token == "none") {
    if(document != "none"){
      Response <- httr::POST(url = URL, body = document)
    } else {
      Response <- httr::POST(url = URL)
    }
  } else {
    token <- paste0("Bearer ", token)
    if(document != "none"){
      Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = document)
    } else {
      Response <- httr::POST(url = URL, httr::add_headers(Authorization = token))
    }
  }
  return(Response)
}

#' @title The firestore deleteDocument function
#' @author Jiasheng Zhu
#' @description The function allows to delete a document on firestore databases
#' @param projectID The Firestore project ID {string}
#' @param documentPath path for the document to be deleted {string}
#' @param databaseID The database under which document will be deleted {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @return returns empty when the operation is successful, otherwise a http response with error
#' @export
#' @examples
#' \dontrun{
#' response <- deleteDocument("gsoc2018-d05d8", documentPath = "this/trythis")
#' }
deleteDocument <- function(projectID, documentPath, databaseID = "(default)", token = "none") {
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents/", documentPath)
  if (token == "none") {
    Response <- httr::DELETE(url = URL)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::DELETE(url = URL, httr::add_headers(Authorization = token))
  }
  return(Response)
}

#' @title The firestore get function
#' @author Jiasheng Zhu
#' @description Get single document
#' @param projectID The Firestore project ID {string}
#' @param documentPath path for the document to be deleted {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @param decode Whether to decode the result to R variblaes
#' @return returns the data frame if successful, else return the full http response
#' @export
#' @examples
#' \dontrun{
#' }
getDocument <- function(projectID, documentPath, databaseID = "(default)", token = "none", decode = TRUE) {
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) == "/"){
    documentPath <- substring(documentPath, 0, nchar(documentPath)-1)
  }
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents/", documentPath)
  if (token == "none") {
    Response <- httr::GET(url = URL)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::GET(url = URL, httr::add_headers(Authorization = token))
  }
  if(decode){
    return(decode(Response))
  } else {
    return(Response)
  }
}

#' @title The firestore list function
#' @author Jiasheng Zhu
#' @description Lists all documents underneath a collection id
#' @param projectID The Firestore project ID {string}
#' @param collectionPath path to a collection {string}
#' @param pageSize The maximum number of documents to return
#' @param databaseID The database under which this operation will be performed {string}
#' @param pageToken A page token. Used to identify a previous list collectionIDs operation {string}
#' @param orderBy The order to sort results by, for example, priority desc, name{string}
#' @param showMissing If the list should show missing documents.
#' A missing document is a document that does not exist but has sub-documents.
#' These documents will be returned with a key but will not have fields, Document.create_time, or Document.update_time set.
#' Requests with showMissing may not specify where or orderBy.
#' @param token The user access token that can be retrieved with the auth() function.
#' An OAuth 2.0 access token is required for listCollectionIDs. {string}
#' @return A HTTP response that contains the documents and a nextPageToken
#' @export
#' @examples
#' \dontrun{
#' }
listDocuments <- function(projectID, collectionPath, pageSize, databaseID = "(default)", pageToken = "none", orderBy = "none", showMissing = FALSE, token = "none") {
  if(substring(collectionPath, nchar(collectionPath), nchar(collectionPath)) == "/"){
    collectionPath <- substring(collectionPath, 0, nchar(collectionPath)-1)
  }
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents/", collectionPath, "?pageSize=", pageSize)
  if(pageToken != "none"){
    URL <- paste0(URL, "&pageToken=", pageToken)
  }
  if(orderBy != "none"){
    URL <- paste0(URL, "&orderBy=", orderBy)
  }
  if(showMissing){
    URL <- paste0(URL, "&showMissing=TRUE")
  }

  if (token == "none") {
    Response <- httr::GET(url = URL)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::GET(url = URL, httr::add_headers(Authorization = token))
  }
  return(Response)
}

#' @title The firestore batch get function
#' @author Jiasheng Zhu
#' @description Get multiple documents by names
#' @param projectID The Firestore project ID {string}
#' @param documents A vector consists of documents names in the format projects/(projectId)/databases/(databaseId)/documents/(document_path) {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param mask The fields to return. If not set, returns all fields. {string}
#' @param transaction Reads documents in a transaction {string}
#' @param newTransaction Starts a new transaction and reads the documents.
#' Defaults to a read-only transaction. The new transaction ID will be returned as the first response in the stream.{string}
#' @param readTime Reads documents as they were at the given time. This may not be older than 60 seconds. {string}
#' @param token The user access token that can be retrieved with the auth() function.
#' An OAuth 2.0 access token is required for listCollectionIDs. {string}
#' @return A HTTP response that contains the documents, transaction, readTime and missing documents
#' @export
#' @examples
#' \dontrun{
#' }
batchGetDocuments <- function(projectID, documents, databaseID = "(default)", mask = "none", transaction = "none", newTransaction = "none", readTime = "none", token = "none"){
  URL = paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents:batchGet")
  request <- list()
  request$documents <- documents
  if(mask != "none"){
    request$mask <- mask
  }
  if(transaction != "none"){
    request$transaction <- transaction
  }
  if(newTransaction != "none"){
    request$newTransaction <- newTransaction
  }
  if(readTime != "none"){
    request$readTime <- readTime
  }
  if (token == "none") {
    Response <- httr::POST(url = URL, body = jsonlite::toJSON(request, auto_unbox = TRUE))
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = jsonlite::toJSON(request, auto_unbox = TRUE))
  }
  return(Response)
}

#' @title The firestore patch function
#' @author Jiasheng Zhu
#' @description Patch single document
#' @param projectID The Firestore project ID {string}
#' @param documentPath path for the document to be deleted {string}
#' @param document The document represented in the form firestore requires
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @return returns a http response with the document if successful
#' @export
#' @examples
#' \dontrun{
#' }
patchDocument <- function(projectID, documentPath, document, databaseID = "(default)", token = "none") {
  document <- generateDocument(document, name = paste0(projects, projectID, "/", databaseID, "/", documents, documentPath))
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) == "/"){
    documentPath <- substring(documentPath, 0, nchar(documentPath)-1)
  }
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents/", documentPath)
  if (token == "none") {
    Response <- httr::PATCH(url = URL, body = document)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::PATCH(url = URL, httr::add_headers(Authorization = token), body = document)
  }
  return(Response)
}


#' @title The firestore beginTransaction function
#' @author Jiasheng Zhu
#' @description Begin a transaction
#' @param projectID The Firestore project ID {string}
#' @param options options for the transactions {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @return http response with the transaction number
#' @export
#' @examples
#' \dontrun{
#' }
beginTransaction <- function(projectID, options, databaseID = "(default)", token = "none") {
  if(substring(databaseID, nchar(databaseID), nchar(databaseID)) != "/"){
    databaseID <- paste0(databaseID, "/")
  }
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "documents:beginTransaction")
  if (token == "none") {
    Response <- httr::POST(url = URL, body = options)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = options)
  }
  return(Response)
}

#' @title The firestore commit function
#' @author Jiasheng Zhu
#' @description Commit a transaction
#' @param projectID The Firestore project ID {string}
#' @param options options for the commit, including write details and transaction number {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @return http response with the write results and commit time
#' @export
#' @examples
#' \dontrun{
#' }
commit <- function(projectID, options, databaseID = "(default)", token = "none") {
  if(substring(databaseID, nchar(databaseID), nchar(databaseID)) != "/"){
    databaseID <- paste0(databaseID, "/")
  }
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "documents:commit")
  if (token == "none") {
    Response <- httr::POST(url = URL, body = options)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = options)
  }
  return(Response)
}

#' @title The firestore rollback function
#' @author Jiasheng Zhu
#' @description Rollback a transaction
#' @param projectID The Firestore project ID {string}
#' @param transaction The transaction to be rolled-back {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @return empty http response if successful
#' @export
#' @examples
#' \dontrun{
#' }
rollback <- function(projectID, transaction, databaseID = "(default)", token = "none") {
  if(substring(databaseID, nchar(databaseID), nchar(databaseID)) != "/"){
    databaseID <- paste0(databaseID, "/")
  }
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "documents:rollback")
  request_body <- paste0('{"transaction": "', transaction, '"}')
  if (token == "none") {
    Response <- httr::POST(url = URL)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token))
  }
  return(Response)
}

#' @title The firestore list collectionID function
#' @author Jiasheng Zhu
#' @description Lists all the collection IDs underneath a document
#' @param projectID The Firestore project ID {string}
#' @param documentPath path to a document {string}
#' @param pageSize The maximum number of collection ids to return
#' @param databaseID The database under which this operation will be performed {string}
#' @param pageToken A page token. Used to identify a previous list collectionIDs operation {string}
#' @param token The user access token that can be retrieved with the auth() function.
#' An OAuth 2.0 access token is required for listCollectionIDs. {string}
#' @return A HTTP response that contains the collectionIDs and a nextPageToken
#' @export
#' @examples
#' \dontrun{
#' }
listCollectionIds <- function(projectID, documentPath, pageSize, databaseID = "(default)", pageToken = "none", token = "none") {
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) == "/"){
    documentPath <- substring(documentPath, 0, nchar(documentPath)-1)
  }
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents/", documentPath, ":listCollectionIds")
  request_body <- paste0('{"pageSize": ', pageSize)
  if(pageToken == "none"){
    request_body <- paste0(request_body, "}")
  } else {
    request_body <- paste0(request_body, ', "pageToken": "', pageToken, '"}')
  }
  if (token == "none") {
    Response <- httr::POST(url = URL, body = request_body)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = request_body)
  }
  return(Response)
}


#' @title The firestore run query function
#' @author Jiasheng Zhu
#' @description Run a query
#' @param projectID The Firestore project ID {string}
#' @param query Strutured query {string}
#' @param documentPath path to a document, optional {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param transaction Reads documents in a transaction {string}
#' @param newTransaction Starts a new transaction and reads the documents.
#' Defaults to a read-only transaction. The new transaction ID will be returned as the first response in the stream.{string}
#' @param readTime Reads documents as they were at the given time. This may not be older than 60 seconds. {string}
#' @param token The user access token that can be retrieved with the auth() function.
#' An OAuth 2.0 access token is required for listCollectionIDs. {string}
#' @return A HTTP response that contains the documents, transaction, readTime and missing documents
#' @export
#' @examples
#' \dontrun{
#' }
runQuery <- function(projectID, query, documentPath = "none", databaseID = "(default)", transaction = "none", newTransaction = "none", readTime = "none", token = "none"){
  URL = paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/documents")
  if(documentPath == "none"){
    URL = paste0(URL, ":runQuery")
  } else {
    URL = paste0(URL, "/", documentPath, ":runQuery")
  }
  request <- list()
  request$structuredQuery <- query
  if(transaction != "none"){
    request$transaction <- transaction
  }
  if(newTransaction != "none"){
    request$newTransaction <- newTransaction
  }
  if(readTime != "none"){
    request$readTime <- readTime
  }
  if (token == "none") {
    Response <- httr::POST(url = URL, body = jsonlite::toJSON(request, auto_unbox = TRUE))
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = jsonlite::toJSON(request, auto_unbox = TRUE))
  }
  return(Response)
}

#' Index methods

#' @title The firestore indexField function
#' @author Jiasheng Zhu
#' @description Create an indexField
#' @param fieldPath The path of the field. Must match the field path specification described
#' by [google.firestore.v1beta1.Document.fields][fields]. Special field path __name__ may be
#' used by itself or at the end of a path. __type__ may be used only at the end of path. {string}
#' @param mode one of ASCENDING or DESCENDING {string}
#' @return returns an index field
#' @export
#' @examples
#' \dontrun{
#' }
indexField <- function(fieldPath, mode = "MODE_UNSPECIFIED"){
  if(mode == "MODE_UNSPECIFIED"){
    stop("Please specify a mode")
  }

  field <- list()
  field$fieldPath <- fieldPath
  field$mode <- mode

  return(jsonlite::toJSON(field, auto_unbox = TRUE))
}

#' @title The firestore index function
#' @author Jiasheng Zhu
#' @description Create an index
#' @param collectionID The collection ID to which this index applies {string}
#' @param fields The fields to index {string}
#' @return returns a json representation of the index
#' @export
#' @examples
#' \dontrun{
#' }
index <- function(collectionID, fields){
  index_json <- paste0('{"collectionId":"', collectionID, '","fields": [')
  for(field in fields){
    index_json <- paste0(index_json, field, ",")
  }
  index_json <- paste0(index_json, ']}')
  return(index_json)
}

#' @title The firestore create index function
#' @author Jiasheng Zhu
#' @description Create an index on firestore
#' @param projectID The Firestore project ID {string}
#' @param index representation of an index {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token that can be retrieved with the auth() function.
#' An OAuth 2.0 access token is required for listCollectionIDs. {string}
#' @return A response that contains the status of the operation
#' @export
#' @examples
#' \dontrun{
#' }
createIndex <- function(projectID, index, databaseID = "(default)", token = "none"){
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/indexes")
  if (token == "none") {
    Response <- httr::POST(url = URL, body = index)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = index)
  }
  return(Response)
}

#' @title The firestore delete index function
#' @author Jiasheng Zhu
#' @description Delete an index on firestore
#' @param projectID The Firestore project ID {string}
#' @param indexID ID of the index to delete {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token {string}
#' @return If successful, the response body will be empty
#' @export
#' @examples
#' \dontrun{
#' }
deleteIndex <- function(projectID, indexID, databaseID = "(default)", token = "none"){
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/indexes/", indexID)
  if (token == "none") {
    Response <- httr::DELETE(url = URL)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::DELETE(url = URL, httr::add_headers(Authorization = token))
  }
  return(Response)
}

#' @title The firestore get index function
#' @author Jiasheng Zhu
#' @description Get an index on firestore
#' @param projectID The Firestore project ID {string}
#' @param indexID ID of the index to delete {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token {string}
#' @return If successful, the response body will contain an instance of the index
#' @export
#' @examples
#' \dontrun{
#' }
getIndex <- function(projectID, indexID, databaseID = "(default)", token = "none"){
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/indexes/", indexID)
  if (token == "none") {
    Response <- httr::GET(url = URL)
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::GET(url = URL, httr::add_headers(Authorization = token))
  }
  return(Response)
}

#' @title The firestore list index function
#' @author Jiasheng Zhu
#' @description List index on firestore
#' @param projectID The Firestore project ID {string}
#' @param filter string used to filter index {string}
#' @param pageSize The standard List page size
#' @param pageToken The standard List page token {string}
#' @param databaseID The database under which this operation will be performed {string}
#' @param token The user access token {string}
#' @return If successful, the response body will be empty
#' @export
#' @examples
#' \dontrun{
#' }
listIndex <- function(projectID, filter = NULL, pageSize = NULL, pageToken = NULL, databaseID = "(default)", token = "none"){
  URL <- paste0("https://firestore.googleapis.com/v1beta1/projects/", projectID, "/databases/", databaseID, "/indexes")

  if (token == "none") {
    Response <- httr::GET(url = URL, query = list(filter = filter, pageSize = pageSize, pageToken = pageToken))
  } else {
    token <- paste0("Bearer ", token)
    Response <- httr::GET(url = URL, httr::add_headers(Authorization = token), query = list(filter = filter, pageSize = pageSize, pageToken = pageToken))
  }
  return(Response)
}

#' @title Generate Firestore document
#' @author Jiasheng Zhu
#' @description Convert data frame to a Firestore document
#' @param data The data frame to be converted {data.frame}
#' @param max_digits The maximum digits to keep in the convertion, default is NA, (all)
#' @param auto_unbox automatically \code{\link{unbox}} all atomic vectors of length 1. It is usually safer to avoid this and instead use the \code{\link{unbox}} function to unbox individual elements.
#'   An exception is that objects of class \code{AsIs} (i.e. wrapped in \code{I()}) are not automatically unboxed. This is a way to mark single values as length-1 arrays.
#' @param name an optional name field can be added to the document
#' @return converted json string
#' @export
#' @examples
#' \dontrun{
#' }
generateDocument <- function(data, name = "none") {
  field <- encode(data)
  if(name == "none") {
    document = paste0('{"fields":', field, '}')
  } else {
    document = paste0('{"name": "',name,'","fields":', field, '}')
  }
  return(document)
}

#' @title Encode a R variable to Firestore document
#' @author Jiasheng Zhu
#' @description Convert any R variable to string which conforms to Firestore document format
#' @param value The data frame to be converted
#' @return string representing Firestore document
#' @export
#' @examples
#' \dontrun{
#' }
encode <- function(value){
  #vector
  if(is.vector(value) && length(value) > 1){
    str <- ''
    for(element in value){
      str <- paste0(str, recursive_encode(element), ",")
    }
    return(paste0('{"fireData" : { "arrayValue": { "values" : [', str, ']}}}'))
  }

  #array and matrix
  if(is.array(value)){
    data_str <- recursive_encode(as.vector(value))
    dim_str <- recursive_encode(dim(value))
    return(paste0('{"array" : { "mapValue": { "fields" : { "data" : ', data_str, ',
                  "dim"  : ', dim_str, '}}}}'))
    }

  #data.frame
  if(is.data.frame(value)){
    str <- ''
    for(name in names(value)){
      str <- paste0(str,'"', name, '" : ', recursive_encode(value[[name]]), ', ')
    }
    return(paste0('{"data.frame":{"mapValue":{"fields" : {', str, '}}}}'))
  }

  #list
  if(is.list(value)){
    str <- ''
    for(name in names(value)){
      str <- paste0(str,'"', name, '" : ', recursive_encode(value[[name]]), ', ')
    }
    return(paste0('{"list":{"mapValue":{"fields" : {', str, '}}}}'))
  }

  if(is.null(value))
    return(paste0('{"fireData" : { "nullValue": "', value, '" }}'))

  if(is.logical(value))
    return(paste0('{"fireData" : { "booleanValue": "', value, '" }}'))

  if(is.integer(value))
    return(paste0('{"fireData" : { "integerValue": "', value, '" }}'))

  if(is.double(value))
    return(paste0('{"fireData" : { "doubleValue": "', value, '" }}'))

  #TODO check this type's correctness
  if(is.numeric.Date(value))
    return(paste0('{"fireData" : { "timestampValue": "', value, '" }}'))

  if(is.character(value))
    return(paste0('{"fireData" : { "stringValue": "', value, '" }}'))

  #TODO Binary (byte) type is not included

  # NOTE: We avoid doing an isinstance() check for a Document
  #       here to avoid import cycles.
  #TODO reference type
  #TODO geo point type

  stop('Cannot convert to a Firestore Value', value, 'Invalid type', typeof(value))
  }

#' @title Encode a R variable to Firestore document recursively
#' @author Jiasheng Zhu
#' @description Convert any R variable to string which conforms to Firestore document format recursively
#' @param value The data frame to be converted
#' @return string representing Firestore document
#' @export
#' @examples
#' \dontrun{
#' }
recursive_encode <- function(value){
  if(is.vector(value) && length(value) > 1){
    str <- ''
    for(element in value){
      str <- paste0(str, recursive_encode(element), ",")
    }
    return(paste0('{ "arrayValue": { "values" : [', str, '] } }'))
  }

  if(is.list(value)){
    str <- ''
    for(name in names(value)){
      str <- paste0(str,'"', name, '" : ', recursive_encode(value[[name]]), ', ')
    }
    return(paste0('{"mapValue":{"fields" : {', str, '}}}'))
  }

  if(is.null(value))
    return(paste0('{ "nullValue": "', value, '" }'))

  if(is.logical(value))
    return(paste0('{ "booleanValue": "', value, '" }'))

  if(is.integer(value))
    return(paste0('{ "integerValue": "', value, '" }'))

  if(is.double(value))
    return(paste0('{ "doubleValue": "', value, '" }'))

  #TODO check this type's correctness
  if(is.numeric.Date(value))
    return(paste0('{ "timestampValue": "', value, '" }'))

  if(is.character(value))
    return(paste0('{ "stringValue": "', value, '" }'))

  #TODO Binary (byte) type is not included

  # NOTE: We avoid doing an isinstance() check for a Document
  #       here to avoid import cycles.
  #TODO reference type
  #TODO geo point type

  stop('Cannot convert to a Firestore Value', value, 'Invalid type', typeof(value))
}

#' @title Decode a Firestore document to R variable
#' @author Jiasheng Zhu
#' @description Convert string which conforms to Firestore document format to R variable
#' @param response HTTP response including a Firestore document
#' @return R variable
#' @export
#' @examples
#' \dontrun{
#' }
decode <- function(response){
  parsed_response <- httr::content(response, "parsed")
  if(is.null(parsed_response$fields)){
    stop("Invalid response: could not find fields")
    return(response)
  } else {
    if(names(parsed_response$fields) == "array"){
      data <- recursive_decode(parsed_response$fields[["array"]]$mapValue$fields$data)
      dim <- recursive_decode(parsed_response$fields[["array"]]$mapValue$fields$dim)
      result <- array(data, dim)
    } else if(names(parsed_response$fields) != "fireData"){
      result <- recursive_decode(parsed_response$fields[[names(parsed_response$fields)]])
      result <- do.call(names(parsed_response$fields), result)
    } else {
      result <- recursive_decode(parsed_response$fields[["fireData"]])
    }
  }
  return(result)
}

#' @title Decode a Firestore document to R variable recursively
#' @author Jiasheng Zhu
#' @description Convert Firestore document to R variable recursively
#' @param fields a potion of a Firestore document
#' @return R variable
#' @export
#' @examples
#' \dontrun{
#' }
recursive_decode <- function(fields){
  for(name in names(fields)){
    if(!length(names(fields)) == 1) stop("A key is mapped to more than one value")
    if(names(fields) == "mapValue"){
      result <- list()
      for (col in names(fields$mapValue$fields)) {
        result[[col]] <- recursive_decode(fields$mapValue$fields[[col]])
      }
      return(result)
    }

    else if(names(fields) == "arrayValue"){
      vector_length = length(fields$arrayValue$values)
      result <- vector(length = vector_length)
      for (i in 1:vector_length) {
        result[i] <- recursive_decode(fields$arrayValue$values[i][[1]])
      }
      return(result)
    }

    #TODO may need for force type conversion
    else if(names(fields) == "doubleValue"){
      return(fields$doubleValue)
    }

    else if(names(fields) == "integerValue"){
      return(fields$integerValue)
    }

    else if(names(fields) == "nullValue"){
      return(NULL)
    }

    else if(names(fields) == "booleanValue"){
      return(fields$booleanValue)
    }

    else if(names(fields) == "timestampValue"){
      return(fields$timestampValue)
    }

    else if(names(fields) == "stringValue"){
      return(fields$stringValue)
    }

    else {
      stop('Cannot convert to a Firestore Value ', fields, 'Invalid type', names(fields))
    }
  }
}
