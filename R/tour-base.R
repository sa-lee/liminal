#' Tour a high dimensional dataset
#'
#' @param .data a data.frame to tour
#' @param cols Columns to tour. This can use a tidyselect specification
#' such as [tidyselect::starts_with()].
#' @param color A variable name in `.data`, mapping to the color aesthetic, if
#' NULL points will be colored black.
#' @param tour_path the tour path to take, the default is [tourr::grand_tour()]
#' but also works with [tourr::guided_tour()].
#' @param rescale A function that rescales `cols`, the default is to
#' [clamp()] the data to lie in the hyperdimensional unit cube. To not perform
#' any scaling use [identity()].
#' @param morph A function that modifies each projection generated,
#' the default is to center the projection using [morph_center()].
#'
#' @return A shiny app object
#' @details
#' The tour interface consists of two views
#'   1. the tour view which is a dynamic scatterplot
#'   2. the axis view which shows the direction and magnitude of the
#'   basis vectors being generated.
#' There are several other user controls available:
#'  * There is a play button, that when pressed will start the tour.
#'  * There is also a text view of the half range which is the maximum squared
#'    Euclidean distance between points in the tour view. The half range
#'    is a scale factor for projections and can be thought of as a way
#'    of zooming in and out on points. It can be dynamically modified by scrolling
#'    (via a mouse-wheel). To reset double click the tour view.
#'  * The legend can be toggled to highlight groups of points with
#'    shift+mouse-click. Multiple groups can be selected in this way. To
#'    reset double click the legend title.
#'
#'
#' @seealso [compute_half_range()],[limn_tour_xylink()]
#' @examples
#' # tour the first ten columns of the fake tree data
#' if (interactive()) {
#'   # loads the default interface
#'   limn_tour(fake_trees, dim1:dim10)
#'   # perform the same action but now coloring points
#'   limn_tour(fake_trees, dim1:dim10, color = branches)
#'}
#'
#' @export
limn_tour <- function(.data, cols, color = NULL, tour_path = tourr::grand_tour(), rescale = clamp, morph = morph_center) {
  cols <- rlang::enquo(cols)
  color <- rlang::enquo(color)
  # set up tour parameters
  tour_data <- init_tour_matrix(.data, cols, rescale = rescale)
  path <- tourr::new_tour(tour_data, tour_path)
  # setup colors
  color_data <- dplyr::select(.data, !!color)

  # generate app
  server <- limn_tour_server(tour_data, path, color_data, morph)
  ui <- gadget_tour_ui(title = "liminal tour")
  app <- shinyApp(ui, server)
  runGadget(app)
}

blank_axis <- function() {
  list(title = NULL,
       grid = FALSE,
       ticks = FALSE,
       labels = FALSE,
       domainColor = "lightgray")
}

# look into sizing policies for vegawidget https://vega.github.io/vega-lite/docs/size.html
#

render_init <- function(source_values, half_range) {
  # setup initial view
  base_schema <- schema_scatter()
  domain <- c(-half_range, half_range)

  base_schema[["mark"]] <- list(type = "circle", clip = TRUE)
  base_schema[["encoding"]][["x"]][["field"]] <- "x"
  base_schema[["encoding"]][["x"]][["scale"]] <- list(domain = domain)
  base_schema[["encoding"]][["x"]][["axis"]] <- blank_axis()

  base_schema[["encoding"]][["y"]][["field"]] <- "y"
  base_schema[["encoding"]][["y"]][["scale"]] <- list(domain = domain)
  base_schema[["encoding"]][["y"]][["axis"]] <- blank_axis()


  # selection on shift-click
  shift_click <- "[mousedown[event.shiftKey], mouseup] > mousemove"
  base_schema[["selection"]][["brush"]][["on"]] <- shift_click
  base_schema[["selection"]][["brush"]][["translate"]] <- shift_click
  base_schema[["selection"]][["brush"]][["zoom"]] <- FALSE


  # pan + zoom
  base_schema[["selection"]][["grid"]] <-  list(type = "interval",
                                                bind = "scales",
                                                translate = FALSE)
  if (ncol(source_values) == 3) {
    col_nm <- colnames(source_values)[3]
    type <- color_type(source_values[[col_nm]])
    base_schema[["encoding"]][["color"]][["condition"]][["field"]] <- col_nm
    base_schema[["encoding"]][["color"]][["condition"]][["type"]] <- type
    if (type %in% c("nominal", "ordinal")) {
      col_domain <- unique(source_values[[col_nm]])
      base_schema[["encoding"]][["color"]][["condition"]][["scale"]] <- list(domain = sort(col_domain))
    }
    # if color available enable clickable legend
    base_schema[["selection"]][["colclick"]] <- list(type = "multi",
                                                     fields = list(col_nm),
                                                     bind = list(legend = "dblclick"))
    op_value <- opacity_value(nrow(source_values))
    base_schema[["encoding"]][["opacity"]] <- list(condition = list(selection = "colclick", value = 0.9), value = op_value)


  } else {
    col_nm <- "black"
    base_schema[["encoding"]][["color"]][["condition"]] <- list(selection = "brush",
                                                           value = col_nm)
  }
  base_schema[["data"]][["name"]] <- "path"
  base_schema[["data"]][["values"]] <- source_values
  base_schema
}

render_init_axes <- function(source_values, half_range, cols) {
  axis_tour <- schema_axes_tour("rotations", half_range)
  axis_tour[["data"]][["values"]] <- generate_axes(source_values, cols)
  axis_tour
}

limn_tour_server <- function(tour_data, path, color_tbl, morph) {

  init <- init_tour(tour_data, path, color_tbl, morph)

  function(input, output, session) {
    output[["tourView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["tourView"]]),
        embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE),
      )
    )
    output[["axisView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["axisView"]]),
        embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE)
      )
    )

    # reactives
    selections <- shiny::reactiveValues(proj = path(0)$proj)


    rct_active_zoom <-  vegawidget::vw_shiny_get_signal("tourView",
                                            name = "grid",
                                            body_value = "value")
    rct_active_brush <- vegawidget::vw_shiny_get_signal("tourView",
                                                     name = "brush",
                                                     body_value = "value")

    rct_half_range <- rct_half_range(rct_active_zoom, init[["half_range"]])
    rct_x_brush_active <- rct_pause(rct_active_brush)

    rct_play <- shiny::eventReactive(input$play, input$play > 0)

    rct_tour <- rct_tour(path,
                         rct_event = rct_x_brush_active,
                         rct_refresh = rct_play,
                         selections = selections,
                         session = session)

    rct_axes <- stream_axes(rct_tour, init[["cols"]])
    rct_proj <- stream_proj(tour_data,
                            init[["source_values"]],
                            selections,
                            rct_half_range,
                            morph)

    # observers
    vegawidget::vw_shiny_set_data("axisView", "rotations", rct_axes())
    vegawidget::vw_shiny_set_data("tourView", "path", rct_proj())

    output$half_range <- shiny::renderText({
      rct_half_range()
    })

  }
}

generate_axes <- function(source_values, cols) {
  tbl_zeros <- matrix(0, nrow = nrow(source_values), ncol = 3)
  id <- matrix(seq_len(nrow(source_values)), ncol = 1)

  source_values <- rbind(
    cbind(source_values, id),
    cbind(matrix(0, nrow = nrow(source_values), ncol = 2), id)
  )

  colnames(source_values) <- c("x", "y", "group")
  source_values <- as.data.frame(source_values)
  source_values[["axis_name"]] <- c(cols, rep("", nrow(tbl_zeros)))
  source_values
}

init_tour_matrix <- function(.data, cols, rescale) {
  if (is.null(cols)) {
    tour_data <- as.matrix(.data)
  } else {
    tour_data <- as.matrix(dplyr::select(.data, !!cols))
  }
  return(rescale(tour_data))
}

init_tour <- function(tour_data, path, color_tbl, morph) {
  # half range
  half_range <- compute_half_range(tour_data)
  cols <- colnames(tour_data)

  # intialise views
  start <- path(0)$proj
  source_values <- tour_data %*% start
  source_values <- morph(source_values) / half_range
  colnames(source_values) <- c("x", "y")
  source_values <- as.data.frame(source_values)

  if (ncol(color_tbl) == 1) {
    source_values <- dplyr::bind_cols(source_values, color_tbl)
  }

  init_tour <- render_init(source_values, half_range)
  init_axes <- render_init_axes(start, half_range, cols)

  list(tourView = init_tour,
       axisView = init_axes,
       half_range = half_range,
       source_values = source_values,
       cols = cols)
}


opacity_value <- function(nr, pow = 0.3) (1 / nr)^pow
