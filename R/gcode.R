#' Creates a gcode file.
#'
#' @param file filename
#' @param x sampling area in x direction in mm
#' @param y sampling area in y direction in mm
#' @param resolution in mm
#' @param wait waiting time in sec
#' @param verbose verbose output?
#' @seealso http://reprap.org/wiki/G-code#G-commands
#' @export
gcode <- function(file, x, y, resolution, wait, verbose=TRUE) {
  m <- .gcode(x=x, y=y, resolution=resolution, wait=wait)

  .msg(verbose, "Approximate sampling time: ", ncol(m) * wait)

  writeLines(c("G21", "G91", head(as.vector(m), -1L)), con=file)
}

#' Creates a gcode content (workhorse of gcode)
#' @param x sampling area in x direction in mm
#' @param y sampling area in y direction in mm
#' @param resolution in mm
#' @param wait waiting time in sec
#' @seealso http://reprap.org/wiki/G-code#G-commands
#' @noRd
.gcode <- function(x, y, resolution, wait) {
  nx <- trunc(x/resolution) + 1L
  ny <- trunc(y/resolution) + 1L

  move <- logical(nx * ny)
  move[1L:nx + nx * rep(seq(0L, ny - 1L, by=2), each=nx)] <- TRUE

  m <- matrix(c(rep(sprintf("G4 S%f", wait), length(move)),
                sprintf(ifelse(move, "G0 X%f", "G0 X-%f"), resolution)),
                nrow=2, byrow=TRUE)

  m[2L, seq(nx, ncol(m), by=nx)] <- sprintf("G0 Y%f", resolution)

  m
}

#' Parses gcode content (second row of .gcode output matrix)
#' @param x character vector
#' @seealso http://reprap.org/wiki/G-code#G-commands
#' @noRd
.parsegcode <- function(x, relative=TRUE) {
  x <- x[grepl("^G0", x)]

  pos <- lapply(strsplit(x, "\\s+"), function(xx) {
    xx <- xx[-1L]
    xx <- setNames(as.double(substr(xx, 2L, nchar(xx))),
                   tolower(substr(xx, 1L, 1L)))
    as.numeric(xx[c("x", "y", "z")])
  })
  pos <- do.call(rbind, pos)
  colnames(pos) <- c("x", "y", "z")
  pos[is.na(pos)] <- 0L

  if (!relative) {
    pos <- apply(pos, 2L, cumsum)
  }

  pos
}
