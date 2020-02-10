#' View a tour with external context
#'
#' @param x a `data.frame` or `tibble` to tour
#' @param y a `data.frame` to link to `x`, representing a scatter plot
#' @param by a column (or columns) for defining how to join. See `dplyr::inner_join()`
#' for details. By default we use the row index for both tables.
#' @param x_color an optional bare column name in `x`, for the color mapping in the tour view
#' @param y_color an optional bare column name for the colour mapping the linked view
#' @param tour_path the tour path to take, the default is [tourr::grand_tour()].
#' @param rescale A function that rescales tour columns. Default is [clamp()]
#' To not perform any scaling use [identity()].
#' @param morph A callback function that modifies the projection, default is to
#' center the projection using [morph_center()].
#'
#' @importFrom dplyr inner_join
#' @export
limn_tour_xylink <- function(x, y, by = "rowid", x_color = NULL, y_color = NULL, tour_path = tourr::grand_tour(), rescale = clamp, morph = morph_center) {

  # generate tour data
  x_color <- rlang::enquo(x_color)
  x_color_tbl <- dplyr::select(x, !!x_color)
  # tour cols are everything but color column
  x_tcols <- dplyr::select(x, !!rlang::quo(-!!x_color))
  # convert to a matrix
  tour_data <- init_tour_matrix(x_tcols, cols = NULL, rescale = rescale)
  # establish the path
  path <- tourr::new_tour(tour_data, tour_path)

  x_views <- init_tour(tour_data, path, x_color_tbl, morph)

  y_color <- rlang::enquo(y_color)
  y_views <- y_spec(y, !!y_color)

  x_views[["source_values"]] <- conditional_join(x_views[["source_values"]],
                                                 y_views[["source_values"]])

  tspec <- x_views[["tourView"]][c("$schema", "data")]
  hconcat <- list(hconcat = list(x_views[["tourView"]][!names(x_views[["tourView"]]) %in% c("$schema", "data")],
                                 y_views[["y_spec"]])
  )
  tspec[["data"]][["values"]] <- x_views[["source_values"]]

  print(x_views[["source_values"]])

  x_views[["tourView"]] <- c(tspec, hconcat)


  server <- function(input, output, session) {
    output[["tourView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(x_views[["tourView"]]),
        embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE)
      )
    )
    output[["axisView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(x_views[["axisView"]]),
        embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE),
      )
    )

    # reactives
    rct_active_zoom <-  vegawidget::vw_shiny_get_signal("tourView",
                                                        name = "grid",
                                                        body_value = "value")
    rct_active_brush <- vegawidget::vw_shiny_get_signal("tourView",
                                                        name = "brush",
                                                        body_value = "value")

    rct_embed_brush <- vegawidget::vw_shiny_get_signal("tourView",
                                                       name = "y_brush",
                                                       body_value = "value")

    rct_half_range <- rct_half_range(rct_active_zoom,
                                     x_views[["half_range"]])
    rct_pause <- rct_pause(rct_active_brush)

    rct_play <- shiny::eventReactive(input$play, input$play > 0)

    rct_tour <- rct_tour(path,
                         rct_event = rct_pause,
                         rct_refresh = rct_play,
                         session = session)
    rct_axes <- stream_axes(rct_tour, x_views[["cols"]])
    rct_proj <- stream_proj(rct_tour,
                            tour_data,
                            x_views[["source_values"]],
                            rct_half_range(),
                            morph)


    # observers
    vegawidget::vw_shiny_set_data("axisView", "rotations", rct_axes())
    vegawidget::vw_shiny_set_data("tourView", "path", rct_proj())

    output$half_range <- shiny::renderPrint({
      # protects against initial NULL
      list(rct_half_range(),
      rct_embed_brush())
    })
  }

  # generate app
  ui <- limn_tour_ui("linked")
  shiny::shinyApp(ui, server)

}

conditional_join <- function(x, y, by = "rowid") {
  rowid.x <- seq_len(nrow(x))
  rowid.y <- seq_len(nrow(y))

  x[["rowid"]] <- rowid.x
  y[["rowid"]]<- rowid.y

  if (by == "rowid") {
    res <- dplyr::bind_cols(x, y)
  } else {
    res <- dplyr::inner_join(x, y, by = by)
  }
  res <- dplyr::mutate(res, .selected = TRUE)
  res

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

  y_spec <- c(encoding,
              mark,
              selection)
  list(source_values = y_data, y_spec = y_spec)
}
