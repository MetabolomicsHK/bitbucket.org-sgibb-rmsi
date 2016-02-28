#' ESIprot: an universal tool for charge state determination
#' and molecular weight calculation of proteins from electrospray ionization
#' mass spectrometry data calculate.
#'
#' @param x \code{double} consecutive peaks
#' @param zrange \code{double}, charge state range
#'  \code{c(min charge, max charge)}
#' @param hydrogen \code{double}, hydrogen mass
#'
#' @author Original python implementation by Robert Winkler.
#'
#' R implementation by Sebastian Gibb \email{mail@@sebastiangibb.de}
#'
#' @references
#' R. Winkler. 2010.
#' ESIprot: a universal tool for charge state determination and molecular
#' weight calculation of proteins from electrospray ionization mass
#' spectrometry data.
#' Rapid Communications in Mass Spectrometry 24(3): 285-294
#'
#' @examples
#'
#' library("rmsi")
#'
#' cytochromc <- c(651.6, 687.7, 728.1, 773.5, 825.0, 883.9, 951.7,
#'                 1030.9, 1124.5)
#' esiprot(cytochromc)
#'
#' p <- createMassPeaks(mass=cytochromc, intensity=rep(1, length(cytochromc)))
#'
#' esiprot(p)
#'
#' @rdname esiprot
#' @aliases esiprot esiprot,MassPeaks-method
#' @export
setMethod(f="esiprot",
          signature=signature(x="numeric"),
          definition=function(x, zrange=c(1, 100), hydrogen=1.00794) {
  .esiprot(x, zrange, hydrogen)
})

setMethod(f="esiprot",
          signature=signature(x="MassPeaks"),
          definition=function(x, zrange=c(1, 100), hydrogen=1.00794) {
  .esiprot(mass(x), zrange, hydrogen)
})

.esiprot <- function(x, zrange=c(1, 100), hydrogen=1.00794) {
  stopifnot(length(zrange) == 2L)
  z <- seq(floor(max(length(x), zrange[1L])),
           ceiling(max(length(x), zrange[2L])))
  m <- .z(z, length(x)) * (x - hydrogen)
  s <- apply(m, 2, sd)
  i <- which.min(s)
  list(mw=mean(m[, i]), sd=s[i], z=z[i])
}

.z <- function(x, n) {
  x <- t(matrix(x, ncol = n, nrow = length(x)))
  x - (0L:(n - 1L))
}
