context("coordinates")


test_that("coordinates", {
  p <- list(createMassPeaks(mass=1:5, intensity=1:5,
                            metaData=list(imaging=list(pos=c(5, 2)))),
            createMassPeaks(mass=1:5, intensity=5:1,
                            metaData=list(imaging=list(pos=c(6, 2)))))
  m <- matrix(c(5, 6, 2, 2), nrow=2, dimnames=list(c(), c("x", "y")))
  madjust <- matrix(c(1, 2, 1, 1), nrow=2, dimnames=list(c(), c("x", "y")))
  expect_equal(coordinates(p, adjust=FALSE), m)
  expect_equal(coordinates(p), madjust)
})
