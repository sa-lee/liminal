# --- Alternative implementations of tourr package internals ---

#' Rescale all columns to lie in unit interval
#'
#' @param .data A 'matrix' like object
#' @details This function "clamps" all columns in `.data` to fit
#' onto the `ncol(.data)` dimensional unit cube.
#' @importFrom matrixStats colRanges
#' @export
clamp <- function(.data) {
  rng <- matrixStats::colRanges(.data)
  vals <- sweep(.data, 2, rng[,1])
  sweep(vals, 2, rng[,2] - rng[, 1], FUN = "/")
}

#' Rescale all columns by median and IQR
#' @param .data A 'matrix' like object
#' @importFrom matrixStats colMedians colMads
#' @export
clamp_robust <- function(.data) {
  centers <- matrixStats::colMedians(.data)
  scales <- matrixStats::colMads(.data)
  vals <- sweep(.data, 2, centers)
  sweep(vals, 2, scales, FUN = "/")
}

#' Rescale all columns to have same standard deviation
#' @param .data  A 'matrix' like object
#' @param sd the value of each columns standard devation
#' @importFrom matrixStats colSds
#' @export
clamp_sd <- function(.data, sd = 1e-4) {
  scales <- matrixStats::colSds(.data) / sd
  sweep(.data, 2, scales, FUN = "/")
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
#' @param center Subtract `colMeans(.data)` from each column in `.data`?
#' Default is TRUE.
#'
#' @details This function computes the maximum squared
#' Euclidean distance of rows in a matrix like object. Mostly used
#' internally for setting up xy-axis ranges for a tour animation.
#'
#' @export
compute_half_range <- function(.data, center = TRUE) {
  if (center) .data <- scale(.data, scale = FALSE)
  max(sqrt(rowSums(.data^2)))
}



compute_tour_path <- function(.data, tour_path, start, max_bases, step_size) {
  # checks
  stopifnot(is.numeric(step_size),
            is.numeric(max_bases),
            is.null(start) || is.matrix(start)
  )

  # set up generator
  tour <- tourr::new_tour(.data, tour_path, start)
  # starting projection
  start <- tour(0)$proj
  # dimensions
  nc <- ncol(.data)
  dims <- c(nc, ncol(start),  max_bases + 1)
  projs <- array(NA_real_, dims)

  i <- 0L
  while (i < max_bases) {

    i <- i + 1
    step <- tour(step_size)
    projs[,,i] <- step$target
    if (step$step < 0)
      break

  }

  empty <- apply(projs, 3, function(x) all(is.na(x)))
  projs <- projs[, , !empty, drop = FALSE]
  # flatten to array
  projs <-  Map(function(x) x[[1]], apply(projs, 3, list))

  tourr::planned_tour(projs)

}

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

conditional_join <- function(x, y, by = "rowid") {
  rowid.x <- seq_len(nrow(x))
  rowid.y <- seq_len(nrow(y))

  names.x <- colnames(x)
  # only extract names if they are different
  names.y <- setdiff(colnames(y), names.x)

  y <- y[, names.y]
  x[["rowid"]] <- rowid.x
  y[["rowid"]]<- rowid.y

  dplyr::inner_join(x, y, by = by)

}

