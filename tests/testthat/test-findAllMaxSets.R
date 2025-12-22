test_that("findAllMaxSets basic functionality", {
  mat <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  result <- findAllMaxSets(mat, threshold = 0.5)
  expect_type(result, "list")
})


test_that("findAllMaxSets validates force_in parameter", {
  mat <- matrix(c(1, 0.3, 0.3, 1), 2, 2)

  # Invalid force_in (out of range)
  expect_error(
    findAllMaxSets(mat, threshold = 0.5, force_in = c(3)),
    "force_in"
  )

  # Invalid force_in (zero)
  expect_error(
    findAllMaxSets(mat, threshold = 0.5, force_in = c(0)),
    "force_in"
  )
})

