test_that("function has normal return value as expected", {
  expect_equal(drawsneeded(0.001, 0.02, 0.95, max_n = 1000), 163)
})

test_that("standard example with no errors expected, 0.95 certainty and 1% errors allowed",
          {
            expect_equal(drawsneeded(0.0, 0.01, 0.95, max_n = 1000), 298)
          })

test_that("0 <= expected_error_rate", {
  expect_error(drawsneeded(-0.001, 0.02, 0.95, max_n = 1000))
})

test_that("expected_error_rate < 1", {
  expect_error(drawsneeded(1.001, 0.02, 0.95, max_n = 1000))
})

test_that("0 < certainty", {
  expect_error(drawsneeded(0.001, 0.02, -0.0001, max_n = 1000))
})

test_that("certainty < 1", {
  expect_error(drawsneeded(0.001, 0.02, 1.000, max_n = 1000))
})

test_that("0 < allowed_error_rate", {
  expect_error(drawsneeded(0.001, -0.0001, 0.95, max_n = 1000))
})

test_that("allowed_error_rate < 1", {
  expect_error(drawsneeded(0.001, 1, 0.95, max_n = 1000))
})

test_that("expected_error_rate < allowed_error_rate", {
  expect_error(drawsneeded(0.02, 0.02, 0.95, max_n = 1000))
})

test_that("0 < max_n", {
  expect_error(drawsneeded(0.02, 0.03, 0.95, max_n = 0))
})

test_that("maximally one vector arg with length > 1", {
  expect_error(drawsneeded(c(0.02, 0.03), 0.2, 0.95, c(500, 550)))
})

test_that("first arg has length > 1", {
  expect_equal(drawsneeded(c(0.02, 0.03), 0.2, 0.95, 500), c(16, 17))
})

test_that("second arg has length > 1", {
  expect_equal(drawsneeded(0.02, c(0.2, 0.3), 0.95, 500), c(16, 9))
})

test_that("third arg has length > 1", {
  expect_equal(
    drawsneeded(0.02, 0.03, seq(
      from = 0.05, to = 0.95, by = 0.05
    ), 1000),
    c(
      1,
      4,
      7,
      10,
      15,
      21,
      28,
      37,
      49,
      64,
      83,
      107,
      138,
      178,
      230,
      300,
      398,
      548,
      825
    )
  )
})

test_that("fourth arg has length > 1", {
  expect_equal(drawsneeded(0.02, 0.06, 0.95, c(100, 200, 300)), c(-1, 104, 104))
})

