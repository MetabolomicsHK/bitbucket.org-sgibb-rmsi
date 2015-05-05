context("coordinates")


test_that("coordinates", {
  p <- list(createMassPeaks(mass=1:5, intensity=1:5,
                            metaData=list(imaging=list(pos=c(1, 1)))),
            createMassPeaks(mass=1:5, intensity=5:1,
                            metaData=list(imaging=list(pos=c(2, 1)))))
  m <- matrix(c(1, 2, 1, 1), nrow=2, dimnames=list(c(), c("x", "y")))
  expect_equal(coordinates(p), m)
})
