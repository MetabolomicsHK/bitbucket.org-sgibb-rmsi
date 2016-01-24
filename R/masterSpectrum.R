#' Create master spectrum
#'
#' This function creates a master spectrum out of
#' \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} objects.
#'
#' @param x \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} objects.
#' @param method used aggregation function.
#'
#' @return a \code{list} of reduced \code{\link[MALDIquant]{MassSpectrum-class}}
#' or \code{\link[MALDIquant]{MassPeaks-class}} objects.
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @examples
#'
#' library("rmsi")
#'
#' s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
#'           createMassSpectrum(mass=1:5, intensity=5:1))
#' masterSpectrum(s)
#'
#' @export
masterSpectrum <- function(x, method=c("mean", "median", "sum")) {
  if (isMassPeaksList(x)) {
    as(mergeMassPeaks(x, method=method), "MassSpectrum")
  } else if (isMassSpectrumList(x)) {
    averageMassSpectra(x, method=method)
  }
}
