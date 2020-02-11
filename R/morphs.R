#' Morphing Projections
#'
#' @param .data a projection
#'
#' @export
morph_center <- function(.data) {
  scale(.data, scale = FALSE)
}

radial <- function(x, p) {
  x <- x - colMeans(x)
  rad <- sqrt(x[,1]^2 + x[,2]^2)
  ang <- atan2(x[,1], x[,2])
  r_max <- max(rad)
  rad <- cumulative_radial(rad, r_max, p)
  rad <- sqrt(rad)
  x1 <- rad * cos(ang)
  x2 <- rad * sin(ang)
  cbind(x1, x2)
}

cumulative_radial <- function(r, R, p) {
  1 - (1 - (r/R)^2)^(p/2)
}


