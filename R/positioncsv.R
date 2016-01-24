#' Creates a position csv file.
#' @param file filename
#' @param x sampling area in x direction in mm
#' @param y sampling area in y direction in mm
#' @param resolution in mm
#' @export
positioncsv <- function(file, x, y, resolution) {
  xy <- expand.grid(x=seq(0, x, by=resolution),
                    y=seq(0, y, by=resolution))
  write.table(xy, file=file, sep=",", row.names=FALSE, col.names=FALSE)
}
