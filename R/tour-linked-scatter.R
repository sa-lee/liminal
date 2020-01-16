#' Tour view linked
#'
#' @param x a `data.frame` or `tibble` to tour
#' @param y a `data.frame` to link to `x`, representing a scatter plot
#' @param by a column (or columns) for defining how to join. See `dplyr::inner_join()`
#' for details. By default we use the row index for both tables.
#' @param x_color an optional bare column name in `x`, for the color mapping in the tour view
#' @param y_color an optional bare column name for the colour mapping the linked view
#' @param tour_path the name of the tour path, defaul is `tourr::grand_tour()`
#' @param clamp rescale tour variables to lie in closed interval between 0 and 1.
#' @param ...
#'
#' @export
limn_tour_xylink <- function(x, y, by = "rowid", x_color = NULL, y_color = NULL, tour_path = tourr::grand_tour(), clamp = TRUE, ...) {

  # generate tour data
  x_color <- rlang::enquo(x_color)
  x_color_tbl <- dplyr::select(x, rlang::quo(!!x_color))
  # tour cols are everything but color column
  x_tcols <- dplyr::select(x, rlang::quo(-!!x_color))
  # convert to a matrix
  tour_data <- init_tour_matrix(x_tcols, cols = NULL, clamp)
  # establish the path
  path <- tourr::new_tour(tour_data, tour_path)

  x_views <- init_tour(tour_data, path, x_color_tbl)


  y_views <- y_spec(y, y_color)

  source_values <- if (by == "rowid") {
    dplyr::bind_cols(x_views[["source_values"]], y_views[["source_values"]])
  } else {
    dplyr::inner_join(x_views[["source_values"]], y_views[["source_values"]],
                      by = by)
  }



  # generate app
  server <- limn_tour_linked_server(tour_data, path, scatter_spec, transformer)
  ui <- limn_tour_ui("linked")
  shiny::shinyApp(ui, server)

}


y_spec <- function(y, y_color) {
  # set up linked data
  y_color <- rlang::enquo(y_color)
  y_data <- dplyr::select(y, dplyr::everything(), !!y_color)
  stopifnot(ncol(y_data) <= 3)

  xf <- names(y_data)[1]
  yf <- names(y_data)[2]
  colf <- names(y_data)[3]
  coltype <- color_type(y_data[,3])

  encoding <- list(encoding =
                     list(x = list(field = xf, type = "quantitative"),
                          y = list(field = yf, type = "quantitative"),
                          color = list(condition = list(
                            field = colf,
                            type = coltype,
                            selection = list(`or` = list("brush", "y_brush"))),
                            value = "grey"
                            )
                          )
                   )
  mark <- list(mark = list(type = "circle", clip = TRUE))
  selection <- list(selection = list("y_brush" = list(type = "interval")))

  y_spec <- list(encoding,
                 mark,
                 selection)
  list(source_values = y_data, y_spec = y_spec)
}


limn_tour_linked_server <- function(tour_data, path, scatter_spec, transformer) {
  init <- init_tour(tour_data, path, data.frame())

  function(input, output, session) {
    output[["tourView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["tourView"]]),
        embed = vegawidget::vega_embed(actions = FALSE)
      )
    )
    output[["axisView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["axisView"]]),
        embed = vegawidget::vega_embed(actions = FALSE)
      )
    )

    output[["scatterView"]] <- vegawidget::renderVegawidget(scatter_spec)

    rct_tour <- rct_tour(path, session = session)
    rct_axes <- stream_axes(rct_tour, init[["cols"]])
    rct_proj <- stream_proj(rct_tour,
                            tour_data,
                            init[["source_values"]],
                            init[["half_range"]],
                            transformer)

    vegawidget::vw_shiny_set_data("axisView", "rotations", rct_axes())
    vegawidget::vw_shiny_set_data("tourView", "path", rct_proj())

  }
}
