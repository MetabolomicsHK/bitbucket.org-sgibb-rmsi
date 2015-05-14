#'@export
slides <- function(x, range, step, tolerance,
                   method=c("sum", "mean", "median"), verbose=TRUE) {
  .msg(verbose, "trim spectra to mz ", paste0(range, collapse=":"))
  x <- trim(x, range=range+c(-tolerance, tolerance))

  .msg(verbose, "create intensity matrix")
  im <- intensityMatrix(x)
  mass <- attr(im, "mass")

  coord <- coordinates(x, adjust=TRUE)
  n <- apply(coord, MARGIN=2, FUN=max)

  center <- .center(range, step)
  lr <- .lr(mass, center, tolerance)

  .msg(verbose, "create ims slides")

  if (verbose) {
    pb <- txtProgressBar(0L, length(center), style=3L)
  }
  slides <- array(0L, dim=c(n[1L], n[2L], z=length(center)))

  for (i in seq(along=center)) {
    slides[cbind(coord, i)] <- .aggregateSlide(im, lr[i,], method)

    if (verbose) {
      setTxtProgressBar(pb, i)
    }
  }
  if (verbose) {
    close(pb)
  }
  attr(slides, "center") <- center
  attr(slides, "tolerance") <- tolerance
  slides
}

.lr <- function(x, center, tolerance) {
  left <- findInterval(center-tolerance, x, all.inside=TRUE)
  right <- findInterval(center+tolerance, x, all.inside=TRUE)
  cbind(left=left, right=right)
}

.center <- function(range, step) {
  seq.int(from=range[1L], to=range[2L], by=step)
}

.aggregateSlide <- function(x, lr, method=c("sum", "mean", "median")) {
  method <- match.arg(method)

  x <- x[, lr[1L]:lr[2L], drop=FALSE]

  switch(method,
         "sum" = rowSums(x, na.rm=TRUE),
         "mean" = rowMeans(x, na.rm=TRUE),
         "median" = MALDIquant:::.colMedians(x, na.rm=TRUE))
}
