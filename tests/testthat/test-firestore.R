library("testthat")
library("fireData")

# Test create, get and delete

test_that("Test Firestore creating document and get functionality with a single data frame", {
  df <- data.frame(matrix(rnorm(20), nrow=10))
  response <- createDocument("gsoc2018-d05d8", "test",df, documentName = "test_create")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument("gsoc2018-d05d8", "test/test_create")
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
  response <- deleteDocument("gsoc2018-d05d8", "test/test_create")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore creating document and get functionality with vector", {
  v <- c(rnorm(20))
  response <- createDocument("gsoc2018-d05d8", "test",v, documentName = "test_vector")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument("gsoc2018-d05d8", "test/test_vector")
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
  response <- deleteDocument("gsoc2018-d05d8", "test/test_vector")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore creating document and get functionality with matrix", {
  m <- matrix(rnorm(20), nrow=10)
  response <- createDocument("gsoc2018-d05d8", "test",m, documentName = "test_matrix")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument("gsoc2018-d05d8", "test/test_matrix")
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
  response <- deleteDocument("gsoc2018-d05d8", "test/test_matrix")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test Firestore creating document and get functionality with array", {
  a <- array(rnorm(20), dim=c(5,2,2))
  response <- createDocument("gsoc2018-d05d8", "test",a, documentName = "test_array")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument("gsoc2018-d05d8", "test/test_array")
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
  response <- deleteDocument("gsoc2018-d05d8", "test/test_array")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})


