test_that("morphing projections scales properly", {
  data <- matrix(rnorm(1000), ncol = 10)
  basis <- tourr::orthonormalise(matrix(rnorm(20), ncol = 2))
  proj <- data %*% basis

  morphed <- morph_center(proj, 1)
  expect_equal(dim(proj), dim(morphed))
  expect_equivalent(colMeans(morphed), rep(0, ncol(proj)))

  morphed <- morph_center(proj, .8)
  expect_equivalent(apply(morphed, 2, sd),
                    apply(proj, 2, sd) / 0.8)

  morphed <- morph_identity(proj, 1)
  expect_equal(dim(proj), dim(morphed))
  expect_equivalent(colMeans(morphed), colMeans(proj))

  morphed <- morph_identity(proj, 0.5)
  expect_equivalent(colMeans(morphed), colMeans(proj) * 2)

})
