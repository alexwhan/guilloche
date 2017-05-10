context("Testing constructors")

orbit <- define_orbit(5, c(5, 0), 100)
test_that("constructors build correctly", {
  expect_s3_class(orbit, "orbit")
})

test_that("contructor checks are working", {
  expect_error(define_orbit("a", c(5, 0), 100))
  expect_error(define_orbit(5, 5, 0, 100))
})
