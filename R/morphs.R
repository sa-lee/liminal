#' Morphing Projections
#'
#' @param proj a projection
#' @param half_range scale factor for projection
#' @param p_eff  Effective dimensionality of reference data set.
#'
#' @export
#' @examples
#' proj <- matrix(rnorm(20), ncol = 2)
#' half_range <- compute_half_range(proj)
#' morph_center(proj, half_range)
#' morph_identity(proj, half_range)
#' morph_radial(proj, half_range, p_eff = 2)
#'
#' @rdname morphs
morph_center <- function(proj, half_range) {
  scale(proj, scale = FALSE) / half_range
}

#' @rdname morphs
#' @export
morph_identity <- function(proj, half_range) {
  proj / half_range
}

#' @rdname morphs
#' @export
morph_radial <- function(proj, half_range, p_eff) {
  stopifnot(ncol(proj) == 2L)
  proj <- scale(proj, scale = FALSE)
  rad <- sqrt(rowSums(proj^2))
  ang <- atan2(proj[, 1], proj[, 2])
  # transform with cumulative to get uniform distribution in radius
  rad <- cumulative_radial(rad, half_range, p_eff)
  # square-root is the inverse of the cumulative of a uniform disk
  # (rescaling to maximum radius = 1)
  rad <- sqrt(rad)
  # back transform
  cbind(x = rad * cos(ang), y = rad * sin(ang))
}

#' CDF radial transform
#'
#' @param r the radius of the 2-d projection
#' @param R the radius of the reference hypersphere
#' @param p the dimensionality of the the reference hypersphere
#'
#' @details Computes the fraction of points within radius r
#' given a 2D projection of hypersphere with radius R in p dimensions
#'
#' @noRd
cumulative_radial <- function(r, R, p) {
  1 - (1 - (r / R)^2)^(p / 2)
}



#' Given a character name giving the morph, generate a callback function
#'
#' @param morph A character(1) vector equal to one of
#' c("center", "centre", "radial", "identity")
#'
#' @return a function if morph is valid otherwise throws an error
#' @noRd
generate_morph <- function(morph, p_eff) {
  switch(morph,
    "identity" =  morph_identity,
    "center" = morph_center,
    "centre" = morph_center,
    "radial" = function(proj, half_range) morph_radial(proj, half_range, p_eff),
    stop("Unknown morph function:", morph)
  )
}
