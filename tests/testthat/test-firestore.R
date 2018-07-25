library("testthat")
library("fireData")

test_that("Test Firestore list collection ids functionality", {
  response <- listCollectionIds("gsoc2018-d05d8", "test/test", 1, token = code)
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})
