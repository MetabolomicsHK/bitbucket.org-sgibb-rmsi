#' Get coordinates
#'
#' This function returns the coordinates of
#' \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} objects.
#'
#' @param x \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} objects.
#' @param adjust \code{logical}, should the coordinates reset to start at
#' `c(x=1, y=1)`?
#'
#' @return a 2-column \code{matrix} (x, y) containing the coordinates.
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @export
coordinates <- function(x, adjust=TRUE) {
  m <- do.call(rbind, lapply(x, function(xx)xx@metaData$imaging$pos))
  colnames(m) <- c("x", "y", "z")[1:ncol(m)]

  if (adjust) {
    m <- apply(m, MARGIN=2L, function(x)x-min(x)+1L)
  }
  m
}

