if (!require("devtools")) install.packages("devtools")
devtools::install_github("Kohze/fireData")
library(fireData)

authres <- auth(projectAPI = "AIzaSyA6fjtX9P6y-4GsTq_P92uIfkn4LjXBI7E", email = "gnehsaijuhz@hotmail.com", password = "gsoc2018")
token <- authres$idToken

firestore_root <- "https://firestore.googleapis.com/"
version_prefix <- "v1beta1/"
projects <- "projects/"
databases <- "databases/"
documents <- "documents/"
authPrefix <- "Bearer "

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
createDocument <- function(projectID, documentPath, document, documentName = "none", databaseID = "(default)", token = "none") {
  if(is.data.frame(document)){
    document <- generateDocument(document)
  } else if(!is.character(document)){
    stop("Only supports data frame or string as document types")
  }
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) == "/"){
    documentPath <- substring(documentPath, 0, nchar(documentPath)-1)
  }
  if (documentName == "none") {
    URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath)
  } else {
    URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath, "?documentId=", documentName)
  }
  if (token == "none") {
    Response <- httr::POST(url = URL, body = document)
  } else {
    token <- paste0(authPrefix, token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = document)
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
#' response <- deleteDocument("gsoc2018-d05d8", documentPath = "firedata/demo")
#' }
deleteDocument <- function(projectID, documentPath, databaseID = "(default)", token = "none") {
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath)
  if (token == "none") {
    Response <- httr::DELETE(url = URL)
  } else {
    token <- paste0(authPrefix, token)
    Response <- httr::DELETE(url = URL, httr::add_headers(Authorization = token))
  }
  return(Response)
}

#' @title The firestore write function
#' @author Jiasheng Zhu
#' @description Streams batches of document updates and deletes, in order.
#' @param projectID The Firestore project ID {string}
#' @param databaseID The database under which write operation will be performed {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when the database rules specify the need for user authentications. {string}
#' @return
#' @export
#' @examples
#' \dontrun{
#' }
write <- function(projectID, databaseID = "(default)", streamId = "",  token = "none") {
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) != "/"){
    documentPath <- paste0(documentPath, "/")
  }
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath, documentName)
  if (token == "none") {
    Response <- httr::DELETE(url = URL)
  } else {
    token <- paste0(authPrefix, token)
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
#' @return returns the data frame if successful, else return the full http response
#' @export
#' @examples
#' \dontrun{
#' }
getDocument <- function(projectID, documentPath, databaseID = "(default)", token = "none") {
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) == "/"){
    documentPath <- substring(documentPath, 0, nchar(documentPath)-1)
  }
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath)
  if (token == "none") {
    Response <- httr::GET(url = URL)
  } else {
    token <- paste0(authPrefix, token)
    Response <- httr::GET(url = URL, httr::add_headers(Authorization = token))
  }
  parsed_response <- httr::content(Response, "parsed")
  if(!is.null(parsed_response$error)){
    warning("There is something wrong with the request, returning the full response")
    return(Response)
  } else if (!is.null(parsed_response$fields$hcjson$stringValue)) {
    json_result = parsed_response$fields$hcjson$stringValue
    data = jsonlite::fromJSON(json_result)
    return(data)
  } else {
    warning("The requested document is not in the required format, returning the full response")
    return(Response)
  }
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
  if(is.data.frame(document)){
    document <- generateDocument(document, name = paste0(projects, projectID, "/", databaseID, "/", documents, documentPath))
  } else if(!is.character(document)){
    stop("Only supports data frame or string as document types")
  }
  if(substring(documentPath, nchar(documentPath), nchar(documentPath)) == "/"){
    documentPath <- substring(documentPath, 0, nchar(documentPath)-1)
  }
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath)
  if (token == "none") {
    Response <- httr::PATCH(url = URL, body = document)
  } else {
    token <- paste0(authPrefix, token)
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
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "documents:rollback")
  if (token == "none") {
    Response <- httr::POST(url = URL, body = options)
  } else {
    token <- paste0(authPrefix, token)
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
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "documents:commit")
  if (token == "none") {
    Response <- httr::POST(url = URL, body = options)
  } else {
    token <- paste0(authPrefix, token)
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
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "documents:rollback")
  request_body <- paste0('{"transaction": "', transaction, '"}')
  if (token == "none") {
    Response <- httr::POST(url = URL)
  } else {
    token <- paste0(authPrefix, token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token))
  }
  return(Response)
}

#' @title Generate Firestore document field
#' @author Jiasheng Zhu
#' @description Convert data frame to a Firestore document with single field
#' @param data The data frame to be converted {data.frame}
#' @param max_digits The maximum digits to keep in the convertion, default is NA, (all)
#' @param auto_unbox automatically \code{\link{unbox}} all atomic vectors of length 1. It is usually safer to avoid this and instead use the \code{\link{unbox}} function to unbox individual elements.
#'   An exception is that objects of class \code{AsIs} (i.e. wrapped in \code{I()}) are not automatically unboxed. This is a way to mark single values as length-1 arrays.
#' @return converted json string
#' @export
#' @examples
#' \dontrun{
#' }
generateField <- function(data, max_digits = NA, auto_unbox = TRUE) {
  if(!is.data.frame(data)){
    stop("Only supports data frame type")
  }
  json_str = '"fields": {'
  for(col in names(data)){
    col_str = jsonlite::toJSON(data[col], digits = max_digits, auto_unbox = auto_unbox)
    json_str = paste0(json_str, '"', col, '": {"stringValue": \'', col_str, '\'},')
  }
  json_str = paste0(json_str, "}")
  return(json_str)
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
generateDocument <- function(data, max_digits = NA, auto_unbox = TRUE, name = "none") {
  field <- generateField(data, max_digits, auto_unbox)
  if(name == "none") {
    document = paste0('{', field, '}')
  } else {
    document = paste0('{"name": "',name,'",', field, '}')
  }
  return(document)
}

#' @title Generate data frame from firestore document
#' @author Jiasheng Zhu
#' @description Convert http response that contains a firestore document to a data frame
#' @param data The data frame to be converted {data.frame}
#' @return converted data frame
#' @export
#' @examples
#' \dontrun{
#' }
fromResponse <- function(response){
  parsed_response <- httr::content(response, "parsed")
  if(is.null(parsed_response$fields)){
    stop("Invalid response: could not find fields")
    return(response)
  } else {
    df <- data.frame()
    for(i in names(parsed_response$fields)){
      col <- jsonlite::fromJSON(parsed_response$fields[[i]]$stringValue)
      if(ncol(df) == 0){
        df = col
      } else{
        df = cbind(df, col)
      }
    }
  }
  return(df)
}

############ TEST ############
# Test cases are all written in a auth-free scenario
library("testthat")

test_that("Test Firestore creating document and get functionality with a single data frame", {
  df <- data.frame(matrix(rnorm(20), nrow=10))
  response <- createDocument("gsoc2018-d05d8", "test",df, documentName = "test_create")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument("gsoc2018-d05d8", "test/test_create")
  deleteDocument("gsoc2018-d05d8", "test/test_create")
  if(is.data.frame(response)){
    if(df - response <= 1e-5){
      succeed()
    } else {
      fail()
    }
  } else {
    fail("the returned response is not of type data frame")
    print(response)
  }
})

test_that("Test Firestore deleting document functionality", {
  response <- deleteDocument("gsoc2018-d05d8", "test/testcreate3")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore getting document functionality", {
  response <- getDocument("gsoc2018-d05d8", "test/test2")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore patching document functionality", {
  df <- data.frame(matrix(rnorm(20), nrow=10))
  response <- createDocument("gsoc2018-d05d8", "test",df, documentName = "test_create")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  df <- data.frame(matrix(rnorm(20), nrow=10))
  response <- patchDocument("gsoc2018-d05d8", "test/test_create",df)
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument("gsoc2018-d05d8", "test/test_create")
  deleteDocument("gsoc2018-d05d8", "test/test_create")
  if(is.data.frame(response)){
    if(df - response <= 1e-5){
      succeed()
    } else {
      fail()
    }
  } else {
    fail("the returned response is not of type data frame")
    print(response)
  }
})

test_that("Test Firestore transaction methods - beginTransaction, commit and rollback", {
  # TODO, test beginTransaction, commit and rollback
})
