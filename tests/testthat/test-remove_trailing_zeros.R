test_that("0.10 -> 0.1", {
  expect_equal(remove_trailing_zeros("0.10"), "0.1")
})

test_that("0.100 -> 0.1", {
  expect_equal(remove_trailing_zeros("0.100"), "0.1")
})

test_that("123 -> 123", {
  expect_equal(remove_trailing_zeros("123"), "123")
})

test_that("00 -> 00", {
  expect_equal(remove_trailing_zeros("0.10"), "0.1")
})

test_that(". -> 0", {
  expect_equal(remove_trailing_zeros("."), "0")
})
