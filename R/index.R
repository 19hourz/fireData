#' @title The firebase data upload function:
#' @author Robin Kohze
#' @description The function allows to upload data objects, such as variables,lists and data.frames
#' @param x A data.frame or data.table {object}
#' @param projectURL The Firebase project URL {string}
#' @param directory The optimal Firebase subdirectory {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when if the database rules specify the need for user authentications. {string}
#' @return returns http request answer which includes the random key.
#' @export
#' @examples
#' \dontrun{
#' upload(x = mtcars, projectURL = "https://firedata-b0e54.firebaseio.com/", directory = "main")
#' }
upload <- function(x, projectURL, directory = "main", token = "none"){
 output = fileConversion(x)
 if (token == "none") {
  Response = httr::POST(paste0(projectURL,"/",directory,".json"), body = jsonlite::toJSON(output, auto_unbox = TRUE))
 } else {
   Response = httr::POST(paste0(projectURL,"/",directory,".json?auth=",token), body = jsonlite::toJSON(output, auto_unbox = TRUE))
 }
  return(paste0(directory,"/",httr::content(Response)$name))
}

#' @title Data conversion function
#' @description The internal data conversion function to bring data in the right json format. In case the uploaded file is a s4 class object, the object is converted to a binary s4 object.
#' @param x the input file.
#' @return returns optionally reformatted data.
fileConversion <- function(x){
  if (isS4(x)) {
    output = classConversion(x)
  } else {
    output = x
  }
}

#' @title Data conversion function
#' @description The internal data conversion function to bring data in the right json format. In case the uploaded file is a s4 class object, the object is converted to a binary s4 object.
#' @param projectURL The firebase database url. {string}
#' @param fileName The filename or subdirectory. {string}
#' @param secretKey The optional database secret key for admin access. {string}
#' @param token The user access token that can be retrieved with the auth() function. Required when if the database rules specify the need for user authentications. {string}
#' @param isClass In case a s4 class object is downloaded, fireData expects a isClass=TRUE
#' @return returns optionally reformatted data.
#' @export
#' @examples
#' \dontrun{
#' download(projectURL = "https://firedata-b0e54.firebaseio.com/", fileName = "main/-KxwWNTVdplXFRZwGMkH")
#' }
download <- function(projectURL, fileName, secretKey = "none", token = "none", isClass = FALSE) {

   if (secretKey == "none" && token == "none") {
     urlPath = paste0(projectURL,"/",fileName,".json")
   } else if (token != "none") {
     urlPath = paste0(projectURL,"/",fileName,".json?auth=",token)
   } else {
     urlPath = paste0(projectURL,"/",fileName,".json?auth=",secretKey)
   }

   data = httr::GET(urlPath)

   if (is.null(jsonlite::fromJSON(httr::content(data,"text")))) warning("No data found at database location.")
   if (isClass) {
     retrievedData = httr::content(data,"text")
     tempPath = tempfile()
     writeBin(jsonlite::base64_dec(jsonlite::fromJSON(retrievedData)), tempPath)
     return(readRDS(tempPath))
   } else {
     return(jsonlite::fromJSON(httr::content(data,"text")))
   }
}

#' @title The firebase database backup function:
#' @param projectURL The Firebase Project Url {string}
#' @param secretKey The firebase secret key, which can be found in the Config/ Service Accounts/ Database secrets firebase page. {string}
#' @param fileName The output file name. Can be any string with .json format {string}
#' @description The backup functionality allows to download the whole database into a .json file (which can later be uploaded in the firebase console to do a restore of the DB). Generally this function may allow to save costs by not relying on the Firebase automatic backup function that is only available with the Firebase Blaze premium payment contract.
#' @return Returns either a warning or the backup file name.
#' @export
#' @examples
#' \dontrun{
#' dataBackup(projectURL = "https://firedata-efa5a.firebaseio.com", secretKey = "2bYA6k72wKna90MqPGa6yuMG7jAysoDJZwJqYXsm", "test.json")
#' }

dataBackup <- function(projectURL, secretKey="prompt", fileName){
  if (secretKey == "prompt") {
    secretKey <- readline(prompt = "secretKey: ")
    print("Connecting to SpatialMaps:")
  }
  print("Fetching Data")
  urlPath = paste0(projectURL,"/.json?auth=",secretKey)
  curl::curl_download(url = urlPath,
                destfile = fileName,
                quiet = FALSE)
  print(paste0("Backup created in ", fileName))
}

#' @title The user authentication function:
#' @description fireData::auth checks the validity of a login and returns the temporary JWT user token. FireData_auth can be used to store individual user data in specified directories that are only accessible to that specific user.
#' @param projectAPI The Firebase Project API {string}
#' @param email The user email {string}
#' @param password The user password {string}
#' @return Returns the content of the firebase API request, such as the state of registration, idToken, and validity of the user password.
#' @export
#' @examples
#' \dontrun{
#' auth(projectAPI = "AIzaSyAjZLO9-CRV3gObpwdFz-k8AiTOxHSBmdc", email = "robin@kohze.com", password = "12341234")
#' }
auth <- function(projectAPI, email="prompt", password="prompt"){
  if (password == "prompt" && email == "prompt") {
        email <- readline(prompt = "Email: ")
        password <- readline(prompt = "Password: ")
        print("Connecting to SpatialMaps:")
  }
  AuthUrl = paste0("https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyPassword?key=", projectAPI)
  userData = httr::POST(url = AuthUrl, body = list("email" = email, "password" = password, "returnSecureToken" = "True"), encode = "json")
  return(httr::content(userData))
}

#' @title Firebase user creation function
#' @param projectAPI The Firebase Project API {string}
#' @param email The user email {string}
#' @param password The user password {string}
#' @description Creates a new firebase user account. All user accounts can accessed at the firebase.com project console. One of the advantages of firebase accounts in R is the ability to access a website and analyse the data of the website with the very same login.
#' @return Registers a new user and returns the status.
#' @export
#' @examples
#' \dontrun{
#' createUser(projectAPI = "AIzaSyAjZLO9-CRV3gObpwdFz-k8AiTOxHSBmdc", email = "your@email.com", password = "12341234" )
#' }
createUser <- function(projectAPI, email="prompt", password="prompt"){
  if (password == "prompt" && email == "prompt") {
    email <- readline(prompt = "Email: ")
    password <- readline(prompt = "Password: ")
    print("Connecting to SpatialMaps:")
  }
  AuthUrl = paste0("https://www.googleapis.com/identitytoolkit/v3/relyingparty/signupNewUser?key=", projectAPI)
  userData = httr::POST(url = AuthUrl, body = list("email" = email, "password" = password), encode = "json")
  return(httr::content(userData))
}

#' @title Password resett function:
#' @param projectAPI The Firebase Project API {string}
#' @param email The user email {string}
#' @description Resets the user password and sends an email to the user account.
#' @return Success or warning message.
#' @export
#' @examples
#' \dontrun{
#' resetPassword(projectAPI = "AIzaSyAjZLO9-CRV3gObpwdFz-k8AiTOxHSBmdc", email = "useYourOwn@email.com")
#' }
resetPassword <- function(projectAPI, email){
  AuthUrl = paste0("https://www.googleapis.com/identitytoolkit/v3/relyingparty/getOobConfirmationCode?key=", projectAPI)
  userData = httr::POST(url = AuthUrl, body = list("email" = email, "requestType" = "PASSWORD_RESET"), encode = "json")
  if ("error" %in% names(httr::content(userData))) {
    warning(paste0("User email ", email, " was not found in the database"))
  } else {
    print(paste0("Password reset email was send to ", email))
  }
}

#' @title Internal class to binary conversion:
#' @param x is the S4 class object
#' @description The internal conversion is needed to conserve all class information
#' @return returns base64 encoded binary value of class object
classConversion <- function(x){
  #convert object to base64
  tempPath = tempfile()
  saveRDS(x, file = tempPath)
  binarySet = readBin(tempPath, what = "raw", n = 50000000)
  base64Set = jsonlite::base64_enc(binarySet)
  #adding key by assigning to data.frame
  pRolocList = list("base64Set" =  base64Set)
  return(pRolocList)
}

#' @title Path key replacement function:
#' @description replaces all disallowed path symbols with a "-"
#' @param path is the db path {string}
#' @return the approved and cleaned path_string
#' @export
path_check <- function(path){
  path_replaced = gsub("\\$","-", path)
  path_replaced = gsub("\\#","-", path_replaced)
  path_replaced = gsub("\\]","-", path_replaced)
  path_replaced = gsub("\\[","-", path_replaced)
  path_replaced = gsub("\\/","-", path_replaced)
  path_replaced = gsub("\\.","-", path_replaced)
  if (path_replaced != path) warning(paste0("path changed to ", path_replaced))
  return(path)
}

## Below are implementation for Cloud Firestore

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
#' response <- deleteDocument("gsoc2018-d05d8", documentPath = "this/trythis")
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
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath)
  if (token == "none") {
    Response <- httr::GET(url = URL)
  } else {
    token <- paste0(authPrefix, token)
    Response <- httr::GET(url = URL, httr::add_headers(Authorization = token))
  }
  if(parse){
    return(decode(Response))
  } else {
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

#' @title The firestore list collectionID function
#' @author Jiasheng Zhu
#' @description Lists all the collection IDs underneath a document
#' @param projectID The Firestore project ID {string}
#' @param documentPath path to a document {string}
#' @param pageSize The maximum number of collection ids to receive in a transaction
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
  URL <- paste0(firestore_root, version_prefix, projects, projectID, "/", databases, databaseID, "/", documents, documentPath, ":listCollectionIds")
  request_body <- paste0("{pageSize: ", pageSize)
  if(pageToken == "none"){
    request_body <- paste0(request_body, "}")
  } else {
    request_body <- paste0(request_body, ", pageToken: ", pageToken, "}")
  }
  if (token == "none") {
    Response <- httr::POST(url = URL, body = request_body)
  } else {
    token <- paste0(authPrefix, token)
    Response <- httr::POST(url = URL, httr::add_headers(Authorization = token), body = request_body)
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
generateDocument <- function(data, max_digits = NA, auto_unbox = TRUE, name = "none") {
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

#' @title Authenticating through OAuth 2.0
#' @author Jiasheng Zhu
#' @description Call this function before calling GetOauthCode(), make sure localhost:[port] is listed as an authorized redirect URIs on client id credentials
#' @param client_id client_id from credentials of the project
#' @param port The port that the local host is listening to, a default of 9001 is used
#' @param access_type An offline access type is the default, alternative is online
#' @export
#' @examples
#' \dontrun{
#' }
Oauth <- function(client_id, port = 9001, access_type = "offline"){
  oauth_handle <<- httpuv::startServer("127.0.0.1", port,
                                       list(
                                         call = function(req) {
                                           oauth_redirected_request <<- req
                                           list(
                                             status = 200L,
                                             headers = list(
                                               'Content-Type' = 'text/html'
                                             ),
                                             body = 'You can close this window now and execute the GetOauthCode(client_id, client_secret) function'
                                           )
                                         }
                                       )
  )
  response <- httr::POST(url = paste0("https://accounts.google.com/o/oauth2/v2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdatastore&state=state_parameter_passthrough_value&redirect_uri=http%3A%2F%2F127.0.0.1:", port, "&access_type=", access_type, "&response_type=code&prompt=consent&client_id=", client_id))
  browseURL(response$url)
}

#' @title Get the OAuth 2.0 level access token
#' @author Jiasheng Zhu
#' @description Call this function after finishing the consent to Firestore at localhost (Oauth())
#' @param client_id client_id from credentials of the project
#' @param client_secret client_secret from credentials of the project
#' @param port The port that the local host is listening to, a default of 9001 is used
#' @return Access token
#' @export
#' @examples
#' \dontrun{
#' }
GetOauthCode <- function(client_id, client_secret, port = 9001){
  if(!exists("oauth_redirected_request")) stop("You must run Oauth() and consent first")
  code <- stringr::str_remove(get("QUERY_STRING", envir = oauth_redirected_request), ".*code=")
  httpuv::stopServer(oauth_handle)
  response <- httr::POST(url = paste0("https://www.googleapis.com/oauth2/v4/token?code=", code, "&client_id=", client_id, "&client_secret=", client_secret, "&grant_type=authorization_code&redirect_uri=http%3A%2F%2F127.0.0.1:", port))
  response <- httr::content(response, "parsed")
  return(response$access_token)
}
