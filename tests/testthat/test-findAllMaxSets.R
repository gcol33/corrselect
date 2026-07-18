test_that(".findAllMaxSetsR basic functionality", {
  mat <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  result <- corrselect:::.findAllMaxSetsR(mat, threshold = 0.5)
  expect_type(result, "list")
})


test_that(".findAllMaxSetsR validates force_in parameter", {
  mat <- matrix(c(1, 0.3, 0.3, 1), 2, 2)

  # Invalid force_in (out of range)
  expect_error(
    corrselect:::.findAllMaxSetsR(mat, threshold = 0.5, force_in = c(3)),
    "force_in"
  )

  # Invalid force_in (zero)
  expect_error(
    corrselect:::.findAllMaxSetsR(mat, threshold = 0.5, force_in = c(0)),
    "force_in"
  )
})

test_that("findAllMaxSets and .findAllMaxSetsR are distinct, non-colliding bindings (#36)", {
  # Regression test for issue #36: a hand-written dispatcher used to share
  # the exact name `findAllMaxSets` with the Rcpp-generated export in
  # RcppExports.R, so whichever file's binding loaded last silently won --
  # safe only by accident of R CMD INSTALL's C-locale file sort order.
  # Renaming the hand-written dispatcher to `.findAllMaxSetsR` removes the
  # name collision: `findAllMaxSets` now has exactly one definition (the
  # generated export, a thin one-line .Call() wrapper), and
  # `.findAllMaxSetsR` is a separate function that calls it rather than
  # hardcoding a second .Call() to the same C++ symbol.
  gen <- corrselect:::findAllMaxSets
  wrapper <- corrselect:::.findAllMaxSetsR

  expect_false(identical(body(gen), body(wrapper)))
  expect_true(any(grepl("findAllMaxSets", deparse(body(wrapper)), fixed = TRUE)))
})
