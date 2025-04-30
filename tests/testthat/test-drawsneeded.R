test_that("function has normal return value as expected", {
  expect_equal(drawsneeded(0.001, 0.95, 0.02, max_n = 1000), 163)
})
test_that("0 <= expected_error_rate", {
  expect_error(drawsneeded(-0.001, 0.95, 0.02, max_n = 1000))
})
test_that("expected_error_rate < 1", {
  expect_error(drawsneeded(1.001, 0.95, 0.02, max_n = 1000))
})
test_that("0 < certainty", {
  expect_error(drawsneeded(0.001, -0.0001, 0.02, max_n = 1000))
})
test_that("certainty < 1", {
  expect_error(drawsneeded(0.001, 1.000, 0.02, max_n = 1000))
})
test_that("0 < allowed_error_rate", {
  expect_error(drawsneeded(0.001, 0.95, -0.0001, max_n = 1000))
})
test_that("allowed_error_rate < 1", {
  expect_error(drawsneeded(0.001, 0.95, 1, max_n = 1000))
})
test_that("expected_error_rate < allowed_error_rate", {
  expect_error(drawsneeded(0.02, 0.95, 0.02, max_n = 1000))
})
test_that("0 < max_n", {
  expect_error(drawsneeded(0.02, 0.95, 0.02, max_n = 0))
})

