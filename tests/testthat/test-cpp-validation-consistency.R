# ===========================================================================
# Regression tests for #39: findAllMaxSets(), runELS(), and runBronKerbosch()
# used to each carry their own copy-pasted matrix/force_in validation. They
# now all call the same two shared helpers (validateCorMatrix(),
# validateForcedIndices() in src/utils.cpp), so a future edit to one
# validation message that misses the others would show up here as a
# mismatch rather than silently drifting.
# ===========================================================================

test_that("findAllMaxSets, runELS, and runBronKerbosch reject a non-square matrix identically", {
  mat_nonsquare <- matrix(1:6, nrow = 2)

  msgs <- list(
    findAllMaxSets  = tryCatch(corrselect:::findAllMaxSets(mat_nonsquare, 0.5),
                                error = conditionMessage),
    runELS          = tryCatch(corrselect:::runELS(mat_nonsquare, 0.5, integer(0)),
                                error = conditionMessage),
    runBronKerbosch = tryCatch(corrselect:::runBronKerbosch(mat_nonsquare, 0.5, integer(0), TRUE),
                                error = conditionMessage)
  )

  expect_true(all(vapply(msgs, is.character, logical(1))))
  expect_length(unique(unlist(msgs)), 1)
  expect_match(msgs$findAllMaxSets, "square")
})

test_that("findAllMaxSets, runELS, and runBronKerbosch reject an out-of-bounds force_in identically", {
  mat_ok <- diag(1, 3)

  msgs <- list(
    findAllMaxSets  = tryCatch(corrselect:::findAllMaxSets(mat_ok, 0.5, force_in = 5L),
                                error = conditionMessage),
    runELS          = tryCatch(corrselect:::runELS(mat_ok, 0.5, 5L),
                                error = conditionMessage),
    runBronKerbosch = tryCatch(corrselect:::runBronKerbosch(mat_ok, 0.5, 5L, TRUE),
                                error = conditionMessage)
  )

  expect_true(all(vapply(msgs, is.character, logical(1))))
  expect_length(unique(unlist(msgs)), 1)
  expect_match(msgs$findAllMaxSets, "force_in")
})
