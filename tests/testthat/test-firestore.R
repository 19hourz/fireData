library("testthat")
library("fireData")

test_that("Test Firestore list collection ids functionality", {
  response <- listCollectionIds("gsoc2018-d05d8", "test/test", 1, token = "ya29.Glv8BZLGoUhDUdcB75R6vGmiNwxumWr1LONMNxxZrE_oxBB_ed8Slv2ubqpBq4euvYLC5poclOXAhELNg8Tulp_sGdQlWKOmblE637-bbnhPdQHd8_41xS3pHsdL")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})
