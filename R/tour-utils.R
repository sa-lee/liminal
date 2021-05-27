# --- Alternative implementations of tourr package internals ---
#' Compute Frobenius norm of matrix-like objects x and y
#' @param x,y 'matrix' like objects that have `tcrossprod` methods
#'
#' @return A numeric vector of length 1 that is the Frobenius norm
#'
#' @examples
#' x <- matrix(rnorm(300), ncol = 3)
#' y <- matrix(rnorm(300), ncol = 3)
#' compute_proj_dist(x, y)
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
#' @return A numeric vector of length 1.
#'
#' @examples
#' mv <- matrix(rnorm(300), ncol = 3)
#'
#' compute_half_range(mv)
#'
#' compute_half_range(mv, center = FALSE)
#' @export
compute_half_range <- function(.data, center = TRUE) {
  if (center) .data <- scale(.data, scale = FALSE)
  max(sqrt(rowSums(.data^2)))
}


#' Convert data.frame to a matrix and rescale the columns
#'
#' @param .data a data.frame to tour
#' @param cols Columns to tour. This can use a tidyselect specification
#' such as [tidyselect::starts_with()].
#' @param rescale One of the clamp functions defined above
#'
#' @noRd
generate_tour_matrix <- function(.data, cols, rescale) {
  if (is.null(cols)) {
    tour_data <- as.matrix(.data)
  } else {
    tour_data <- as.matrix(dplyr::select(.data, !!cols))
  }
  return(rescale(tour_data))
}

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Path for pre-defined json specs
#' @noRd
schema_dir <- function() {
  system.file("extdata", "schemas", package = "liminal")
}

#' Define xy scale domain for spec
#'
#' @noRd
set_half_range <- function(spec, half_range) {
  domain <- c(-half_range, half_range)

  spec[["encoding"]][["x"]][["scale"]][["domain"]] <- domain
  spec[["encoding"]][["y"]][["scale"]][["domain"]] <- domain

  spec
}

set_data_name <- function(spec, name) {
  stopifnot(is.character(name) && length(name) == 1L)
  spec[["data"]][["name"]] <- name
  spec
}


set_data_values <- function(spec, values) {
  spec[["data"]][["values"]] <- values
  spec
}


color_type <- function(color_vec) {
  if (is.ordered(color_vec)) {
    return("ordinal")
  }
  if (is.character(color_vec) || is.factor(color_vec)) {
    return("nominal")
  }
  if (is.null(color_vec)) {
    return(NULL)
  }
  "quantitative"
}

color_scale <- function(color_vec) {
  if (is.numeric(color_vec)) {
    return(range(color_vec))
  }
  levels(color_vec) %||% sort(unique(color_vec))
}

color_scheme <- function(domain, scheme = NULL) {
  if (!is.null(scheme)) {
    return(scheme)
  }

  if (is.numeric(domain)) {
    return("viridis")
  }

  if (length(domain) <= 10) {
    return("tableau10")
  }

  return("tableau20")
}

set_encoding_color <- function(spec, color_tbl, color_name, brush = "brush") {
  if (length(color_name) == 0) {
    spec[["encoding"]][["color"]][["condition"]] <-
      list(selection = brush, value = "black")
    return(spec)
  }

  color_vec <- color_tbl[[1]]
  domain <- color_scale(color_vec)
  scheme <- color_scheme(domain)

  color_encoding <- list(
    selection = brush,
    field = color_name,
    type = color_type(color_vec),
    scale = list(
      domain = color_scale(color_vec),
      scheme = scheme
    )
  )

  spec[["encoding"]][["color"]][["condition"]] <- color_encoding

  # if color available enable clickable legend
  spec[["selection"]][["colclick"]] <- list(
    type = "multi",
    fields = list(color_name),
    bind = list(legend = "dblclick")
  )

  spec
}

set_encoding_opacity <- function(spec, alpha) {
  conditions <- setdiff(names(spec[["selection"]]), c("grid"))

  if (length(conditions) > 1) {
    condition <- list(selection = list(`or` = conditions), value = 1)
  } else {
    condition <- list(selection = conditions, value = 1)
  }


  spec[["encoding"]][["opacity"]][["condition"]] <- condition
  spec[["encoding"]][["opacity"]][["value"]] <- alpha

  spec
}

opacity_value <- function(nr, pow = 0.3) (1 / nr)^pow

tbl_projection <- function(tbl, proj) {
  stopifnot(c("x", "y") %in% names(tbl))
  tbl[, c("x", "y")] <- as.data.frame(proj)
  tbl
}
