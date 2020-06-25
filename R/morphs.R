#' Morphing Projections
#'
#' @param proj a projection
#' @param half_range scale factor for projection
#' @param p_eff  dimensionality of basis
#'
#' @export
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
  ang <- atan2(proj[,1], proj[,2])
  # transform with cumulative to get uniform distribution in radius
  rad <- cumulative_radial(rad, half_range, p_eff)
  # square-root is the inverse of the cumulative of a uniform disk
  # (rescaling to maximum radius = 1)
  rad <- sqrt(rad)
  # back transform
  cbind(x = rad * cos(ang), y = rad * sin(ang))
}

# cumulative distribution, fraction of points within radius r
# given 2D projection of hypersphere with radius R in p dimensions
cumulative_radial <- function(r, R, p){
  1 - (1 - (r/R)^2)^(p/2)
}



# Helpers for setting up data to the stream
generate_morph <- function(morph, p_eff) {
  switch(morph,
         "identity" =  morph_identity,
         "center" = morph_center,
         "centre" = morph_center,
         "radial" = function(proj, half_range) morph_radial(proj, half_range, p_eff),
         stop("Unknown morph function:", morph)
  )
}


project_onto_basis <- function(ref, basis) {
  ref %*% basis
}

tbl_projection <- function(tbl, proj) {
  stopifnot(c("x", "y") %in% names(tbl))
  tbl[, c("x", "y")] <- as.data.frame(proj)
  tbl
}



