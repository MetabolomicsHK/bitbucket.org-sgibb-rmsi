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
          definition=function(x, ...) {
  .esiprot(x, ...)
})

setMethod(f="esiprot",
          signature=signature(x="MassPeaks"),
          definition=function(x, ...) {
  .esiprot(mass(x), ...)
})

.esiprot <- function(x, zrange=c(1, 100), hydrogen=1.00794) {
  z <- trunc(optimize(.esiprotopt, interval=zrange,
                      m=x, hydrogen=hydrogen)$minimum)
  x <- .mw(x, z, hydrogen)
  list(mw=mean(x), sd=sd(x), z=z)
}

.mw <- function(m, z, hydrogen) {
  .z(z, length(m)) * (m - hydrogen)
}

.z <- function(z, n) {
  trunc(z:(z - n + 1))
}

.esiprotopt <- function(x, m, hydrogen) {
  sd(.mw(m, x, hydrogen))
}
