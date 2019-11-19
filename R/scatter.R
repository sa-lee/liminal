#' Brushed Scatter Plot
#'
#' @param .data input data.frame
#' @param  x,y bare symbols or expressions using columns from `.data` to map
#' to the x and y aesthetics of a scatter plot
#' @param color an optional sybmol or expression using a column from `.data` to
#' map to the color aesthetic of a scatter plot.
#' @param brush_name the name of the brush element, default is 'brush'
#' @param brush_transform aggregate data inside the brush (currently supports
#' median / mean), default is NULL.
#' @param brush_transform_mark how to display result of brush transform
#' (default is "square")
#'
#' @param ... other parameters to passed to [vegawidget::vegawidget()]
#'
#' @export
#'
#'
#' @importFrom rlang enquo
#' @importFrom dplyr transmute bind_cols
#' @examples
#' limn_xy(mtcars, mpg, hp, factor(cyl))
#'
limn_xy <- function(.data, x, y, color = NULL, brush_name = "brush", brush_transform = NULL, brush_transform_mark = "square", ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color <- rlang::enquo(color)

  schema <- schema_scatter()
  source_id <- substitute(.data)
  source_id <- as.character(source_id)
  schema[["data"]][["name"]] <- source_id

  names(schema[["selection"]]) <- brush_name

  # encodings x,y
  source_values <- dplyr::transmute(.data, !!x, !!y)
  x_nm <- colnames(source_values)[1]
  y_nm <- colnames(source_values)[2]
  schema[["encoding"]][["x"]][["field"]] <- x_nm
  schema[["encoding"]][["y"]][["field"]] <- y_nm

  # set colour encoding
  if (!rlang::quo_is_null(color)) {
    source_values <- dplyr::bind_cols(source_values,
                                      dplyr::transmute(.data, !!color)
    )
    col_nm <- colnames(source_values)[3]

    schema[["encoding"]][["color"]][["condition"]] <- list(
      field = col_nm,
      type = color_type(.data[[col_nm]]),
      selection = brush_name )

  } else {
    col_nm <- "black"
    schema[["encoding"]][["color"]][["condition"]] <- list(
      selection = brush_name,
                                                           value = col_nm)
  }

  if (!is.null(brush_transform)) {
    brush_transform <- match.arg(brush_transform, c("median", "mean"))
    transform <- list(list(filter = list(selection = brush_name)))
    aggregate <- list(aggregate = brush_transform)
    encoding <- list(x = c(schema[["encoding"]][["x"]], aggregate),
                     y = c(schema[["encoding"]][["y"]], aggregate)
    )
    brush_layer <- list(transform = transform,
                        mark = brush_transform_mark,
                        encoding = encoding)

    layer <- list(list(layer = list(
      schema[!(names(schema) %in% c("data", "$schema"))],
      brush_layer
    )))

    schema <- list(`$schema` = schema[["$schema"]],
                   data = schema[["data"]],
                   layer = layer)

  }

  schema[["data"]][["values"]] <- source_values

  spec <- vegawidget::as_vegaspec(schema)
  vegawidget::vegawidget(spec,
                         embed = vegawidget::vega_embed(actions = FALSE),
                         ...
  )
}


#' Dynamic colored scatter plot
#' @param .data input data.frame
#' @param  x,y bare symbols or expressions using columns from `.data` to map
#' to the x and y aesthetics of a scatter plot
#' @param colors a single unqouted expression of columns in `.data` that will be
#' dynamically bound to the colour aesthetic.
#' @param ... other options passed to [vegawidget::vegawidget()]
#'
#' @details Note that this assumes that colors are of the same type...
#'
#' @importFrom tidyr pivot_longer
#' @export
limn_xycol <- function(.data, x, y, colors, ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  colors <- rlang::enquo(colors)
  stopifnot(!rlang::quo_is_null(colors))

  schema <- schema_scatter()


  source_id <- substitute(.data)
  source_id <- as.character(source_id)
  schema[["data"]][["name"]] <- source_id

  # encodings x, y
  source_values <- dplyr::transmute(.data, !!x, !!y)
  source_values <- dplyr::bind_cols(source_values,
                                    dplyr::select(.data, !!colors))
  x_nm <- colnames(source_values)[1]
  y_nm <- colnames(source_values)[2]
  schema[["encoding"]][["x"]][["field"]] <- x_nm
  schema[["encoding"]][["y"]][["field"]] <- y_nm

  # pivoting allows to bind a selection client side
  source_values <- tidyr::pivot_longer(.data, !!colors)

  # now we select 'value' as the colour field
  schema[["encoding"]][["color"]][["condition"]][["field"]] <- "value"
  schema[["encoding"]][["color"]][["condition"]][["type"]] <- color_type(source_values[["value"]])

  # update the as a filter to the plot
  selection <- list(type = "single",
                    fields = list("name"),
                    bind = list(input = "select",
                                options = unique(source_values[["name"]])
                                )
                    )

  schema[["selection"]] <- c(schema[["selection"]], list(color_var = selection))

  schema[["transform"]] <- list(list(filter = list(selection = "color_var")))

  schema[["data"]][["values"]] <- source_values

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


