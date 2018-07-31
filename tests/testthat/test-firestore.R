library("testthat")
library("fireData")

projectID <- "gsoc2018-d05d8"

#refresh the access token
TOKEN <- "ya29.GlsJBuiJwi7OUp-xB4gXYNTWcvp6zPt2pSeBFPROb5AfPg9FyB8-wOxbxZjguLhYLQ9mA-hkBpsMzkthEGAT6B9M0z4s5qaRj3DvjLxDx1ocDgq4UotovhvgF4Wj"

# Test create, get and delete

test_that("Test Firestore creating document and get functionality with a single data frame", {
  df <- data.frame(matrix(rnorm(20), nrow=10))
  response <- createDocument(projectID, "test",df, documentName = "test_create")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument(projectID, "test/test_create")
  if(is.data.frame(response)){
    if(all(df - response <= 1e-5)){
      succeed("Create and get produce same output")
    } else {
      fail("There are discrenpancies between original data frame and the one retrived")
    }
  } else {
    fail("the returned response is not of type data frame")
    print(response)
  }
  response <- deleteDocument(projectID, "test/test_create")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore creating document and get functionality with vector", {
  v <- c(rnorm(20))
  response <- createDocument(projectID, "test",v, documentName = "test_vector")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument(projectID, "test/test_vector")
  if(is.vector(response)){
    if(all(v - response <= 1e-5)){
      succeed("Create and get produce same output")
    } else {
      fail("There are discrenpancies between original data frame and the one retrived")
    }
  } else {
    fail("the returned response is not of type vector")
    print(response)
  }
  response <- deleteDocument(projectID, "test/test_vector")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore creating document and get functionality with matrix", {
  m <- matrix(rnorm(20), nrow=10)
  response <- createDocument(projectID, "test",m, documentName = "test_matrix")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument(projectID, "test/test_matrix")
  if(is.matrix(response)){
    if(all(m - response <= 1e-5)){
      succeed("Create and get produce same output")
    } else {
      fail("There are discrenpancies between original data frame and the one retrived")
    }
  } else {
    fail("the returned response is not of type matrix")
    print(response)
  }
  response <- deleteDocument(projectID, "test/test_matrix")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore creating document and get functionality with array", {
  a <- array(rnorm(20), dim=c(5,2,2))
  response <- createDocument(projectID, "test/",a, documentName = "test_array", token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  getDocument(projectID, "test/test_array/", decode = FALSE)
  response <- getDocument(projectID, "test/test_array/", token = TOKEN)
  if(is.array(response)){
    if(all(a - response <= 1e-5)){
      succeed("Create and get produce same output")
    } else {
      fail("There are discrenpancies between original data frame and the one retrived")
    }
  } else {
    fail("the returned response is not of type array")
    print(response)
  }
  response <- deleteDocument(projectID, "test/test_array", token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

# test list

test_that("Test Firestore list documents", {
  response <- listDocuments(projectID, "test", 1)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- listDocuments(projectID, "test/", 1, pageToken = response$nextPageToken, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- listDocuments(projectID, "cities", 2, orderBy = "name", token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

# test batch get

test_that("Test Firestore batch get documents", {
  response <- batchGetDocuments(projectID, c("projects/gsoc2018-d05d8/databases/(default)/documents/test/test", "projects/gsoc2018-d05d8/databases/(default)/documents/test/test2"), token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- batchGetDocuments(projectID, c("projects/gsoc2018-d05d8/databases/(default)/documents/test/test", "projects/gsoc2018-d05d8/databases/(default)/documents/test/test2"))
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

# test patch

test_that("Test Firestore patch documents", {
  df <- data.frame(matrix(rnorm(20), nrow=10))
  response <- createDocument(projectID, "test", document = df, documentName = "test_patch")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument(projectID, "test/test_patch")
  if(is.data.frame(response)){
    if(all(df - response <= 1e-5)){
      succeed("Create and get produce same output")
    } else {
      fail("There are discrenpancies between original data frame and the one retrived")
    }
  } else {
    fail("the returned response is not of type data frame")
    print(response)
  }

  df <- data.frame(matrix(rnorm(20), nrow=10))
  patchDocument(projectID, "test/test_patch", df, token = TOKEN)
  response <- patchDocument(projectID, "test/test_patch/", df)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- getDocument(projectID, "test/test_patch")
  if(is.data.frame(response)){
    if(all(df - response <= 1e-5)){
      succeed("Create and get produce same output")
    } else {
      fail("There are discrenpancies between original data frame and the one retrived")
    }
  } else {
    fail("the returned response is not of type data frame")
    print(response)
  }

  response <- deleteDocument(projectID, "test/test_patch")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

# following methods requires OAuth token

# Test list collection IDs

test_that("Test Firestore listCollectionIDs", {
  response <- listCollectionIds(projectID, "test/test/", 1, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- listCollectionIds(projectID, "test/test/", 1, pageToken = response$nextPageToken, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  # only to cover tests
  listCollectionIds(projectID, "test/test", 1)
})

# test begin transactions

test_that("Test Firestore begin transaction and rollback", {
  response <- createDocument(projectID, "test", documentName = "test_tran")
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  options <- paste0('{"options":{"readOnly":{"readTime":"', response$updateTime, '"}}}')
  response <- beginTransaction(projectID, options, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- deleteDocument(projectID, "test/test_tran")
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  # only to cover tests
  beginTransaction(projectID, options)
})

test_that("Test Firestore commit", {
  response <- createDocument(projectID, "test", token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  options <- '{"writes":[{"delete":"projects/gsoc2018-d05d8/databases/(default)/documents/test/test_commit"}]}'
  response <- commit(projectID, options, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  # only to cover tests
  commit(projectID, options)
})

# test rollback
# response <- rollback(projectID, response$transaction, token = TOKEN)
# response <- httr::content(response, "parsed")
# expect_null(response$error)

# Query and index tests

test_that("Test Firestore index methods", {
  expect_error(indexField("country"))
  i <- index("users", c(indexField("first","ASCENDING"),indexField("last","ASCENDING")))
  response <- createIndex(projectID, i, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  name <- response$metadata$index
  patterns <- gregexpr('/', name)
  pos <- patterns[[1]][length(patterns[[1]])]
  indexid <- substring(name, pos + 1)
  response <- getIndex(projectID, indexid, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- listIndex(projectID, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  query <- list()
  query$from$collectionId = "users"
  query$from$allDescendants = "TRUE"
  response <- runQuery(projectID, query, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  response <- deleteIndex(projectID, indexid, token = TOKEN)
  response <- httr::content(response, "parsed")
  expect_null(response$error)

  # only to cover tests
  createIndex(projectID, i)
  getIndex(projectID, indexid)
  listIndex(projectID)
  deleteIndex(projectID, indexid)
})
