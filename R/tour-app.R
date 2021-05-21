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
#' @param morph One of `c("center", "centre", "identity", "radial")`
#' that rescales each projection along the tour path. The default
#' is to center the projections and divide by half range. See [morph_center()]
#' for details.
#'
#' @return The tour interface loads a shiny app either in the Viewer pane
#' if you are using Rstudio or in a browser window. After iterating through
#' the tour and and highlighting subsets of interest, you can click the
#' 'Done' button. This will return a named list with two elements:
#'
#'  * `selected_basis`: a matrix consisting of the final projection selected
#'  * `selected_brush`: a matrix consisting of the bounding box of brush
#'
#' @details
#' The tour interface consists of two views:
#'
#'   1. the tour view which is a dynamic scatterplot
#'   2. the axis view which shows the direction and magnitude of the
#'   basis vectors being generated.
#'
#' There are several other user controls available:
#'
#'  * A play button, that when pressed will start the tour animation.
#'  * A pause button, that when pressed will pause the tour animation.
#'  * The title of the view includes the half range. The half range
#'    is a scale factor for projections and can be thought of as a way
#'    of zooming in and out on points. It can be  modified by scrolling
#'    (via a mouse-wheel movement). Double-click to reset to the default
#'    tour view.
#'  * If categorical variable has been used, the legend can be toggled to
#'    highlight categories of interest with shift + mouse click.
#'    Multiple categories can be selected in this way. To reset double click
#'    the legend title.
#'  * Brushing is activated by moving the mouse on the tour view.
#'    If the tour animation a brush event will pause it.
#'
#'
#' @seealso [compute_half_range()], [morph_center()], [limn_tour_link()]
#' @examples
#' \dontrun{
#' # tour the first ten columns of the fake tree data
#' # loads the default interface
#' limn_tour(fake_trees, dim1:dim10)
#' # perform the same action but now coloring points
#' limn_tour(fake_trees, dim1:dim10, color = branches)
#' }
#'
#' @export
limn_tour <- function(.data, cols, color = NULL, tour_path = tourr::grand_tour(), rescale = clamp, morph = "center") {
  cols <- rlang::enquo(cols)
  color <- rlang::enquo(color)
  # setup colors
  color_data <- dplyr::select(.data, !!color)
  # set up tour matrix
  tour_data <- generate_tour_matrix(.data, cols, rescale = rescale)
  # set up transformation function
  morph_projection <- generate_morph(morph, p_eff = ncol(tour_data))
  # generate app
  server <- limn_tour_server(tour_data, tour_path, color_data, morph_projection)
  ui <- gadget_tour_ui()
  app <- shinyApp(ui, server)
  runGadget(app)
}



limn_tour_server <- function(tour_data, tour_path, color_tbl, morph) {
  path <- tourr::new_tour(tour_data, tour_path)

  half_range <- compute_half_range(tour_data)

  start <- path(0)$proj

  cols <- colnames(tour_data)

  tour_frame <- generate_tour_frame(
    tour_data, start, half_range,
    color_tbl, morph
  )

  function(input, output, session) {
    output[["tourView"]] <- renderVegawidget({
      spec_tour(tour_frame, color_tbl, half_range)
    })
    output[["axisView"]] <- renderVegawidget({
      spec_axes(start, half_range, cols)
    })

    # reactiveValues, store current place in tour path
    selections <- shiny::reactiveValues(
      proj = start, do_tour = FALSE,
      force_restart = FALSE
    )


    # vega-lite event listeners
    # listen for zoom and brush events
    rct_active_zoom <- vw_shiny_get_signal("tourView",
      name = "grid",
      body_value = "value"
    )
    rct_active_brush <- vw_shiny_get_signal("tourView",
      name = "brush",
      body_value = "value"
    )

    rct_half_range <- rct_half_range(rct_active_zoom, half_range)

    rct_tour <- rct_tour(path, tour_data, tour_path, selections = selections)

    rct_axes <- reactive({
      generate_axes(selections$proj, cols)
    })

    rct_proj <- reactive({
      proj <- morph(
        tour_data %*% selections$proj,
        half_range = rct_half_range()
      )
      tbl_projection(tour_frame, proj)
    })

    # observers
    vw_shiny_set_data("axisView", "rotations", rct_axes())
    vw_shiny_set_data("tourView", "path", rct_proj())


    # if play button is pressed start tour
    shiny::observeEvent(input$play, {
      selections$do_tour <- input$play
    })

    # if pause button is pressed stop tour
    shiny::observeEvent(input$pause, {
      selections$do_tour <- FALSE
    })

    # if brush is active stop tour
    shiny::observeEvent(rct_active_brush(), {
      selections$do_tour <- length(rct_active_brush()) == 0
    })


    # if restart, pause tour, and generate new tour path
    shiny::observeEvent(input$restart, {
      selections$do_tour <- FALSE
      selections$force_restart <- TRUE
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      tour_artefacts <- list(
        selected_basis = selections$proj,
        tour_brush_box = rct_active_brush()
      )
      stopApp(tour_artefacts)
    })


    observe({
      rct_tour()
    })

    output$half_range <- shiny::renderText({
      paste("Tour with half-range:", round(rct_half_range(), 3))
    })
  }
}
