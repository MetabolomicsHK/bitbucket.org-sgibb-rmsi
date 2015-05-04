#' Find highest intensity values
#'
#' This function returns the \code{n} highest intensity value for each
#' \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} object.
#'
#' @param x \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}} or
#' \code{\link[MALDIquant]{MassPeaks-class}} objects.
#' @param n \code{integer}, number of highest intensity values to preserve. If
#' \code{n} is larger than \code{length(x[[current]])} all intensity values
#' would be returned.
#'
#' @return a \code{list} of reduced \code{\link[MALDIquant]{MassSpectrum-class}}
#' or \code{\link[MALDIquant]{MassPeaks-class}} objects.
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @examples
#'
#' library("msir")
#'
#' s <- createMassSpectrum(mass=1:5, intensity=1:5)
#' topN(list(s), n=2)
#' @export
topN <- function(x, n=100L) {
  lapply(x, function(xx) {
         i <- sort.int(intensity(xx), decreasing=TRUE, index.return=TRUE)$ix
         xx[sort.int(i[1L:min(n, length(xx))])]
  })
}

