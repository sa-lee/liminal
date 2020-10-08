test_that("default clamp puts everything in unit interval", {
  case <- as.matrix(fake_trees[,1:ncol(fake_trees)-1])

  clamped <- clamp(case)

  expect_true(min(clamped) >= 0)
  expect_true(max(clamped) <= 1)

  col_ranges <- apply(clamped, 2, range)

  expect_true(all(col_ranges[1,] >= 0))
  expect_true(all(col_ranges[2,] <= 1))

})


test_that("robust clamps works as expected", {
  case <- as.matrix(pdfsense[, 6:ncol(pdfsense)])

  robust <- clamp_robust(case)
  medians <- apply(robust, 2, median)
  mads <- apply(robust, 2, mad)
  expect_equivalent(medians, rep(0, ncol(case)))
  expect_equivalent(mads, rep(1, ncol(case)))
})

test_that("standardisation clamps works as expected", {
  case <- as.matrix(pdfsense[, 6:ncol(pdfsense)])

  standardise <- clamp_standardize(case)
  means <- colMeans(standardise)
  vars <- apply(standardise, 2, sd)
  expect_equivalent(means, rep(0, ncol(case)))
  expect_equivalent(vars, rep(1, ncol(case)))

  standardise <- clamp_standardize(case, sd = 1e-4)
  means <- colMeans(standardise)
  vars <- apply(standardise, 2, sd)
  expect_equivalent(means, rep(0, ncol(case)))
  expect_equivalent(vars, rep(1e-4, ncol(case)))

  sd_clamp <- clamp_sd(case)
  vars <- apply(sd_clamp, 2, sd)
  expect_equivalent(vars, rep(1, ncol(case)))

  sd_clamp <- clamp_sd(case, sd = 100)
  vars <- apply(sd_clamp, 2, sd)
  expect_equivalent(vars, rep(100, ncol(case)))

})


