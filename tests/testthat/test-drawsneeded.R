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
  expect_equal(drawsneeded(c(0.02, 0.03), 0.2, 0.95, 500), c("0.02" = 16, "0.03" = 17))
})

test_that("second arg has length > 1", {
  expect_equal(drawsneeded(0.02, c(0.2, 0.3), 0.95, 500), c("0.2" = 16, "0.3" = 9))
})

test_that("third arg has length > 1", {
  expect_equal(
    drawsneeded(0.02, 0.03, seq(
      from = 0.05, to = 0.95, by = 0.05
    ), 1000),
    c(
      "0.05" = 1,
      "0.1" = 4,
      "0.15" = 7,
      "0.2" = 10,
      "0.25" = 15,
      "0.3" = 21,
      "0.35" = 28,
      "0.4" = 37,
      "0.45" = 49,
      "0.5" = 64,
      "0.55" = 83,
      "0.6" = 107,
      "0.65" = 138,
      "0.7" = 178,
      "0.75" = 230,
      "0.8" = 300,
      "0.85" = 398,
      "0.9" = 548,
      "0.95" = 825
    )
  )
})

test_that("fourth arg has length > 1", {
  expect_equal(drawsneeded(0.02, 0.06, 0.95, c(100, 200, 300)),
               c(
                 "100" = -1,
                 "200" = 104,
                 "300" = 104
               ))
})
