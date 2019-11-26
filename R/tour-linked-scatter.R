#' Tour view linked
#'
#' @param .data
#' @param cols columns to tour
#' @param x,y bare column names representing aesthetics on linked scatter
#' @param colors variables to highlight
#' @param tour_path
#' @param clamp
#' @param ...
limn_tour_xylink <- function(.data, cols, x, y, colors, tour_path = tourr::grand_tour(), clamp = TRUE, transformer = function(x) scale(x, scale = FALSE), ...) {

  # generate tour
  cols <- rlang::enquo(cols)
  # set up tour parameters
  tour_data <- init_tour_matrix(.data, cols, clamp)
  path <- tourr::new_tour(tour_data, tour_path)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  colors <- rlang::enquo(colors)
  scatter_spec <- limn_xycol(.data, !!x, !!y, !!colors)

  # generate app
  server <- limn_tour_linked_server(tour_data, path, scatter_spec, transformer)
  ui <- limn_tour_ui("linked")
  shiny::shinyApp(ui, server)

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
