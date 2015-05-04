context("masterSpectrum")

masterSum <- createMassSpectrum(mass=1:5, intensity=rep(6, 5))
masterMean <- createMassSpectrum(mass=1:5, intensity=rep(3, 5))

test_that("masterSpectrum works for MassPeaks objects", {
  p <- list(createMassPeaks(mass=1:5, intensity=1:5),
            createMassPeaks(mass=1:5, intensity=5:1))

  expect_equal(masterSpectrum(p, method="sum"), masterSum)
  expect_equal(masterSpectrum(p, method="mean"), masterMean)
})

test_that("masterSpectrum works for MassSpectrum objects", {
  s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
            createMassSpectrum(mass=1:5, intensity=5:1))

  expect_equal(masterSpectrum(s, method="sum"), masterSum)
  expect_equal(masterSpectrum(s, method="mean"), masterMean)
})
