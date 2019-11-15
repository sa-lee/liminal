#' Simple brushed scatter
#'
#' @param .data input data.frame
#' @param  x,y,color aesthetics representing colors from `.data`
#' @param ... other parameters to pass to
#' @export
limn_xy <- function(.data, x, y, color, ...) {
  stopifnot(is.character(x) && length(x) == 1,
            is.character(y) && length(y) == 1,
            is.character(color) && length(color) == 1,
            c(x,y,color) %in% colnames(.data))


  schema <- schema_scatter()
  source <- substitute(.data)
  source <- as.character(source)
  schema[["data"]][["name"]] <- source
  schema[["data"]][["values"]] <- .data[, c(x,y,color)]
  schema[["encoding"]][["x"]][["field"]] <- x
  schema[["encoding"]][["y"]][["field"]] <- y
  schema[["encoding"]][["color"]][["condition"]][["field"]] <- color
  schema[["encoding"]][["color"]][["condition"]][["type"]] <- color_type(.data[[color]])
  spec <- vegawidget::as_vegaspec(schema)
  vegawidget::vegawidget(spec,
                         embed = vegawidget::vega_embed(actions = FALSE),
                         ...
  )
}

color_type <- function(color) {
  if (is.ordered(color)) return("ordinal")
  if (is.character(color) || is.factor(color)) return("nominal")
  if (is.null(color)) return(NULL)
  "quantitative"
}

limn_colxy <- function(.data, x, y, colors, ...) {
  .sub <- .data[, c(x,y, colors)]
  .sub <- tidyr::gather(.sub, key = "variable", value = "value", colors)
  .sub

}


