library(testthat)
library_if_available(stringr)

context("Routes")

test_that("routes without parameters are anchored", {
  expect_that(
    route_re("/path/test")$match,
    equals("^/path/test$"))
  expect_that(
    route_re("/path/test/")$match,
    equals("^/path/test/$"))
  expect_that(
    route_re("/")$match,
    equals("^/$"))
})

test_that("parameters identified correctly", {
  expect_that(route_re("/:test")$params, equals("test"))
  expect_that(route_re("/:test/:test")$params, equals(c("test", "test")))
  expect_that(route_re("/:test1/:test2")$params, equals(c("test1", "test2")))

  expect_that(
    route_re("a/b/c/:test1/d/e/f/:test2")$params, 
    equals(c("test1", "test2")))
})

test_that("splat identified correctly", {
  expect_that(route_re("/*")$params, equals("splat"))
  expect_that(route_re("/*/")$params, equals("splat"))
  expect_that(route_re("/*/a/b/*/c")$params, equals(c("splat", "splat")))
})

test_that("splat works within a path", {
  route <- route_re("/path/:path/*.*")

  expect_that(route$params, equals(c("path", "splat", "splat")))
  expect_that(route$match, equals("^/path/([^/?&#]+)/(.*?)\\.(.*?)$"))
})


test_that("params extracted correctly", {
  splat <- route_matcher("/path/:path/*.*")
  
  expect_that(splat$match("/hadley"), is_false())
  expect_that(splat$match("/path/test"), is_false())
  expect_that(splat$match("/path/test/a"), is_false())

  expect_that(splat$match("/path/test/a.b.c"), is_true())
  expect_that(splat$match("/path/asdfds-asfasdf/a.b.c"), is_true())
  expect_that(splat$match("/path/asdfds_asfasdf/a.b.c"), is_true())

  expect_that(splat$params("/path/a/b.c"), 
    equals(list(path = "a", splat = c("b", "c"))))
})

test_that("single splat matches all", {
  splat <- route_matcher("/*")
  
  paths <- c("/hadley", "/hadley-test", "/ha/d/l/e/y/", "/hadley.wickham")
  for(path in paths) {
    expect_that(splat$match(path), is_true())
    expect_that(splat$params(path), equals(list(splat = str_sub(path, 2))))
  }  
})