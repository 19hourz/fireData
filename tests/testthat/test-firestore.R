library("testthat")
library("fireData")

test_that("Test Firestore creating document and get functionality with a single data frame", {
  df <- data.frame(matrix(rnorm(20), nrow=10))
  response <- createDocument("gsoc2018-d05d8", "test",df, documentName = "test_create")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
  response <- getDocument("gsoc2018-d05d8", "test/test_create")
  deleteDocument("gsoc2018-d05d8", "test/test_create")
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
})
