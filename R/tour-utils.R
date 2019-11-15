# --- Alternative implementations of tourr package internals ---

#' Rescale all columns to lie in [0,1]
#' @param .data A 'matrix' like object
#' @details This function "clamps" all columns in `.data` to fit
#' onto the `ncol(.data)` dimensional unit cube.
#' @export
clamp <- function(.data) {
  rng <- matrixStats::colRanges(.data)
  vals <- sweep(.data, 2, rng[,1])
  sweep(vals, 2, rng[,2] - rng[, 1], FUN = "/")
}

#' Compute Frobenius norm of matrix-like objects x and y
#' @param x,y 'matrix' like objects
#' @export
compute_proj_dist <- function(x, y) {
  sqrt(sum((tcrossprod(x) - tcrossprod(y))^2))
}

#' Compute range of axes for bases
#'
#' @param .data A 'matrix' like object
#' @details This function computes the maximum squared
#' Euclidean distance of rows in a matrix like object. Mostly used
#' internally for setting up xy-axis ranges for a tour animation.
#'
#' @export
compute_half_range <- function(.data) max(sqrt(rowSums(.data^2)))

