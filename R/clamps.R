#' Rescale all columns of a matrix
#'
#' @param .data A numeric matrix
#'
#' @details These functions are used internally by the tour to rescale all
#' columns of `.data`.
#'
#' * `clamp()` rescales so all values for each column lie in the unit interval
#' * `clamp_robust()` rescales by first centering by the median and then scaling
#' by the median absolute deviation.
#' * `clamp_sd()` rescales all columns to have a fixed standard deviation.
#' * `clamp_standardize()` rescales all columns to have zero mean and unit variance.
#'
#' @importFrom matrixStats colRanges colMedians colMads colSds
#' @export
#'
#' @examples
#' mv <- matrix(rnorm(30), ncol = 3)
#'
#' clamp(mv)
#'
#' clamp_robust(mv)
#'
#' clamp_sd(mv)
#'
#' clamp_standardize(mv)
#' @rdname clamps
clamp <- function(.data) {
  rng <- matrixStats::colRanges(.data)
  vals <- sweep(.data, 2, rng[, 1])
  sweep(vals, 2, rng[, 2] - rng[, 1], FUN = "/")
}


#' @export
#' @rdname clamps
clamp_robust <- function(.data) {
  centers <- matrixStats::colMedians(.data)
  scales <- matrixStats::colMads(.data)
  vals <- sweep(.data, 2, centers)
  sweep(vals, 2, scales, FUN = "/")
}

#' @param sd the value of each columns standard deviation (default is 1)
#' @importFrom matrixStats colSds
#' @export
#' @rdname clamps
clamp_sd <- function(.data, sd = 1) {
  scales <- matrixStats::colSds(.data) / sd
  sweep(.data, 2, scales, FUN = "/")
}

#' @importFrom matrixStats colSds
#' @export
#' @rdname clamps
clamp_standardize <- function(.data, sd = 1) {
  centers <- colMeans(.data)
  scales <- matrixStats::colSds(.data) / sd
  vals <- sweep(.data, 2, centers)
  vals <- sweep(vals, 2, scales, FUN = "/")
}
