#' Get coordinates
#'
#' This function returns the coordinates of
#' \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} objects.
#'
#' @param x \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} objects.
#'
#' @return a 2-column \code{matrix} (x, y) containing the coordinates.
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @export
coordinates <- function(x) {
  m <- do.call(rbind, lapply(x, function(xx)xx@metaData$imaging$pos))
  colnames(m) <- c("x", "y")
  m
}

