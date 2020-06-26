# --- Alternative implementations of tourr package internals ---

#' Rescale all columns of a matrix
#'
#' @param .data A numeric matrix
#'
#'
#' @details These functions are used internally by the tour to rescale the
#' columns of `.data`.
#'
#'
#' @importFrom matrixStats colRanges colMedians colMads colSds
#' @export
#'
#' @examples
#' mv <- matrix(rnorm(300), ncol = 3)
#'
#' clamp(mv)
#'
#' clamp_robust(mv)
#'
#' clamp_sd(mv)
#'
#' @rdname clamps
clamp <- function(.data) {
  rng <- matrixStats::colRanges(.data)
  vals <- sweep(.data, 2, rng[,1])
  sweep(vals, 2, rng[,2] - rng[, 1], FUN = "/")
}


#' @export
#' @rdname clamps
clamp_robust <- function(.data) {
  centers <- matrixStats::colMedians(.data)
  scales <- matrixStats::colMads(.data)
  vals <- sweep(.data, 2, centers)
  sweep(vals, 2, scales, FUN = "/")
}

#' @param sd the value of each columns standard devation
#' @importFrom matrixStats colSds
#' @export
#' @rdname clamps
clamp_sd <- function(.data, sd = 1e-4) {
  scales <- matrixStats::colSds(.data) / sd
  sweep(.data, 2, scales, FUN = "/")
}

#' Compute Frobenius norm of matrix-like objects x and y
#' @param x,y 'matrix' like objects that have `tcrossprod` methods
#' @export
compute_proj_dist <- function(x, y) {
  sqrt(sum((tcrossprod(x) - tcrossprod(y))^2))
}

#' Compute range of axes for a tour
#'
#' @param .data A numeric matrix
#' @param center Subtract `colMeans(.data)` from each column in `.data`?
#' Default is TRUE.
#'
#' @details This function computes the maximum squared
#' Euclidean distance of rows in a matrix like object. Mostly used
#' internally for setting up xy-axis ranges for a tour animation.
#'
#' @examples
#' mv <- matrix(rnorm(300), ncol = 3)
#'
#' compute_half_range(mv)
#'
#' compute_half_range(mv, center = FALSE)
#'
#' @export
compute_half_range <- function(.data, center = TRUE) {
  if (center) .data <- scale(.data, scale = FALSE)
  max(sqrt(rowSums(.data^2)))
}

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

