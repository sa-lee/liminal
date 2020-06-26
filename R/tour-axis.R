#' Generate a data.frame that represents a 'biplot' view of a basis matrix
#'
#' @param proj The current basis matrix
#' @param cols A character vector giving the labels for the biplot
#'
#' @noRd
generate_axes <- function(proj, cols) {
  tbl_zeros <- matrix(0, nrow = nrow(proj), ncol = 3)
  id <- matrix(seq_len(nrow(proj)), ncol = 1)

  proj <- rbind(
    cbind(proj, id),
    cbind(matrix(0, nrow = nrow(proj), ncol = 2), id)
  )

  colnames(proj) <- c("x", "y", "group")
  proj <- as.data.frame(proj)
  proj[["axis_name"]] <- c(cols, rep("", nrow(tbl_zeros)))
  proj
}

#' Load the biplot schema from a json file
#'
#' @param name A character(1) giving the name of the data inside the spec
#' @param half_range The default scales for the x and y encoding
#'
#' @noRd
schema_axes_tour <- function(name, half_range) {
  json <- file.path(schema_dir(), "biplot.json")
  ans <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  ans[["data"]][["name"]] <- name
  ans[["encoding"]][["x"]][["scale"]][["domain"]] <- c(-half_range, half_range)
  ans[["encoding"]][["y"]][["scale"]][["domain"]] <- c(-half_range, half_range)
  ans
}


#' Generate the initial vegaspec for the biplot view
#' @param proj The current basis matrix
#' @param half_range The target axis scales
#' @param cols A character vector giving the labels for the biplot
#' @importFrom vegawidget as_vegaspec vegawidget vega_embed
#' @noRd
spec_axes <- function(proj, half_range, cols) {
  axis_tour <- schema_axes_tour("rotations", half_range)
  axis_tour[["data"]][["values"]] <- generate_axes(proj, cols)

  vegawidget::vegawidget(
    vegawidget::as_vegaspec(axis_tour),
    embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE)
  )
}
