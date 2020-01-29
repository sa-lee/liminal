#' Tour a high dimensional dataset
#'
#' @param .data a data.frame to tour
#' @param cols Columns to tour. This can use a tidyselect specification.
#' @param color A variable name in `.data`, mapping to the color aesthetic. Default
#' is to not colour any points.
#' @param tour_path the tour path to take, the default is [tourr::grand_tour()].
#' @param rescale A function that rescales tour columns. Default is [clamp()]
#' To not perform any scaling use [identity()].
#' @param morph A callback function that modifies the projection, default is to
#' center the projection using [morph_center()].
#'
#'
#'
#' @export
limn_tour <- function(.data, cols, color = NULL, tour_path = tourr::grand_tour(), rescale = clamp, morph = morph_center) {
  cols <- rlang::enquo(cols)
  color <- rlang::enquo(color)
  # set up tour parameters
  tour_data <- init_tour_matrix(.data, cols, rescale)
  path <- tourr::new_tour(tour_data, tour_path)
  # setup colors
  color_data <- dplyr::select(.data, !!color)

  # generate app
  server <- limn_tour_server(tour_data, path, color_data, morph)
  ui <- limn_tour_ui("simple")
  shiny::shinyApp(ui, server)

}

limn_tour_ui <- function(view = "simple") {
  view <- match.arg(view, c("simple", "linked"))


  # views always present
  tview <- vegawidget::vegawidgetOutput("tourView")
  aview <- vegawidget::vegawidgetOutput("axisView")

  # control panel
  title <- shiny::h4("liminal controls")
  play <- shiny::actionButton("play", "Play", icon = shiny::icon("play"))

  half_rng <- shiny::textOutput(outputId = "half_range")

  if (view == "linked") {
    brush_rdo <- shiny::radioButtons("brush_selector",
                                     "Select Brush Type",
                                     choices = c("linked", "neighbors", "centroids"))
    bottom_row <- shiny::fluidRow(
      shiny::column(4, aview),
      shiny::column(4,
                    title,
                    play,
                    brush_rdo
      ),
      shiny::column(4, shiny::h5("Half Range"), half_rng)
    )
  } else {
    bottom_row <- shiny::fluidRow(
      shiny::column(4, aview),
      shiny::column(4, play),
      shiny::column(4, shiny::h5("Half Range"), half_rng)
    )
  }

  tview_ui <-   shiny::fluidRow(shiny::column(12, tview))

  shiny::fluidPage(
    tview_ui,
    bottom_row
  )
}

blank_axis <- function() {
  list(title = NULL,
       grid = FALSE,
       ticks = FALSE,
       labels = FALSE,
       domainColor = "lightgray")
}

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

  init <- init_tour(tour_data, path, color_tbl)

  function(input, output, session) {
    output[["tourView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["tourView"]]),
        embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE),
        height = 200,
        width = 250
      )
    )
    output[["axisView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["axisView"]]),
        embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE),
        width = 200,
        height = 200
      )
    )

    # reactives
    rct_active_zoom <-  vegawidget::vw_shiny_get_signal("tourView",
                                            name = "grid",
                                            body_value = "value")
    rct_active_brush <- vegawidget::vw_shiny_get_signal("tourView",
                                                     name = "brush",
                                                     body_value = "value")

    rct_half_range <- rct_half_range(rct_active_zoom, init[["half_range"]])
    rct_pause <- rct_pause(rct_active_brush)

    rct_play <- shiny::eventReactive(input$play, input$play > 0)

    rct_tour <- rct_tour(path,
                         rct_event = rct_pause,
                         rct_refresh = rct_play,
                         session = session)
    rct_axes <- stream_axes(rct_tour, init[["cols"]])
    rct_proj <- stream_proj(rct_tour,
                            tour_data,
                            init[["source_values"]],
                            rct_half_range(),
                            morph)




    # observers

    vegawidget::vw_shiny_set_data("axisView", "rotations", rct_axes())
    vegawidget::vw_shiny_set_data("tourView", "path", rct_proj())

    output$half_range <- shiny::renderText({
      # protects against initial NULL
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

init_tour_matrix <- function(.data, cols, clamp) {
  stopifnot(is.function(clamp))
  if (is.null(cols)) {
    tour_data <- as.matrix(.data)
  } else {
    tour_data <- as.matrix(dplyr::select(.data, !!cols))
  }
  return(clamp(tour_data))
}

init_tour <- function(tour_data, path, color_tbl) {
  # half range
  half_range <- compute_half_range(tour_data)
  cols <- colnames(tour_data)

  # intialise views
  start <- path(0)$proj
  source_values <- tour_data %*% start
  source_values <- scale(source_values, scale = FALSE) / half_range
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


