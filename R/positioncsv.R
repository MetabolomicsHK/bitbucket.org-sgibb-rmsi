#' Creates a position csv file.
#' @param file filename
#' @param x sampling area in x direction in mm
#' @param y sampling area in y direction in mm
#' @param resolution in mm
#' @export
positioncsv <- function(file, x, y, resolution) {
  nx <- trunc(x/resolution)
  ny <- trunc(y/resolution)

  xy <- cbind(x=rep(c(0L:nx, nx:0L), length.out=(nx + 1L) * (ny + 1L)),
              y=rep(0L:ny, each=nx + 1L))

  write.table(xy, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}
