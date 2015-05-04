context("topN")

p <- list(createMassPeaks(mass=1:5, intensity=1:5),
          createMassPeaks(mass=1:5, intensity=5:1))

test_that("topN", {
  expect_equal(topN(p, n=3), list(p[[1]][3:5], p[[2]][1:3]))
  expect_equal(topN(p, n=100), p)
})
