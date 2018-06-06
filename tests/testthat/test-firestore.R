library("testthat")
library("fireData")

test_that("Test the Firebase creating document functionality", {
  response <- createDocument("gsoc2018-d05d8", "test",documentName = "testcreate", databaseID = "test2")
  response <-httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test the Firebase deleting document functionality", {
  response <- deleteDocument("gsoc2018-d05d8", "test/", "testcreate")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test the Firebase getting document functionality", {
  response <- getDocument("gsoc2018-d05d8", "test/trythis")
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})

test_that("Test the Firebase patching document functionality", {
  json = '{"name": "projects/gsoc2018-d05d8/databases/(default)/documents/test/trythis",
  "fields": {
    "a": {
      "mapValue": {
        "fields": {
          "a": {
            "stringValue": "a"
          },
          "b": {
            "integerValue": "1"
          }
        }
      }
    }
  }}'
  response <- patchDocument("gsoc2018-d05d8", "test/trythisaswell", json)
  response <- httr::content(response, "parsed")
  expect_null(response$error)
})
