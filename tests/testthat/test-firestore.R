library("testthat")
library("fireData")

projectID <- "gsoc2018-d05d8"

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
  response <- createDocument(projectID, "test",a, documentName = "test_array")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument(projectID, "test/test_array")
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
  response <- deleteDocument(projectID, "test/test_array")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore list documents", {
  response <- listDocuments(projectID, "test", 1)
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})
