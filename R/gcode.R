#' Creates a gcode file.
#' @param file filename
#' @param x sampling area in x direction in mm
#' @param y sampling area in y direction in mm
#' @param resolution in mm
#' @param wait waiting time in sec
#' @param verbose verbose output?
#' @export
gcode <- function(file, x, y, resolution, wait, verbose=TRUE) {
  nx <- trunc(x/resolution) + 1L
  ny <- trunc(y/resolution) + 1L

  move <- logical(nx * ny)
  move[1L:nx + nx * rep(seq(0L, ny - 1L, by=2), each=nx)] <- TRUE

  m <- matrix(c(rep(sprintf("G4 S%f", wait), length(move)),
                sprintf(ifelse(move, "G0 X%f", "G0 X-%f"), resolution)),
                nrow=2, byrow=TRUE)

  m[2L, seq(nx, ncol(m), by=nx)] <- sprintf("G0 Y%f", resolution)

  .msg(verbose, "Approximate sampling time: ", nx * ny * wait)

  writeLines(c("G21", "G91", head(as.vector(m), -1L)), con=file)
}
