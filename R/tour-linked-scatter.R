#' View a tour, linked to a scatter plot.
#'
#' @param x a `data.frame` or `tibble` to tour
#' @param y a `data.frame` to link to `x`, representing a scatter plot
#' @param x_color an optional bare column name in `x`, for the color mapping in the tour view
#' @param y_color an optional bare column name for the colour mapping the linked view
#' @param tour_path the tour path to take, the default is [tourr::grand_tour()].
#' @param rescale A function that rescales tour columns. Default is [clamp()]
#' To not perform any scaling use [identity()].
#' @param morph A callback function that modifies the projection, default is to
#' center the projection using [morph_center()].
#' @param reference an optional data.frame or matrix to compute
#' nearest neighbors from, must have same number of rows as `x`, the default
#' is to compute from all numeric columns in `x`.
#'
#' @details
#' The linked tour interface consists of three views:
#'  1. The tour view on the left is a dynamic and interactive scatterplot. Brushing on the tour view
#'  is activated with the shift key plus a mouse drag. By default it will
#'  highlight corresponding points in the xy view and pause the animation.
#'  2. The xy view on the right is an interactive scatterplot. Brushing on the xy view
#'  will highlight points in the tour view and is activated via a mouse drag,
#'  the type of highlighting depends on the brush mode selected.
#'  3. the axis view which shows the direction and magnitude of the
#'  basis vectors being generated.
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
#'  * There are different brushing modes that can be modified via
#'    radio buttons. Both neighbors and distance based brushing operate
#'    on the k-NN graph obtained from the control panel.
#
#' @examples
#' \dontrun{
#' # tour the first ten columns of the fake tree data and link to the
#' # another layout based on t-SNE
#' # loads the default interface
#' tsne <- Rtsne::Rtsne(dplyr::select(fake_trees, dplyr::starts_with("dim")))
#' tsne_df <- data.frame(tsneX = tsne$Y[,1], tsneY = tsne$Y[,2], branches = fake_trees$branches)
#' limn_tour_xylink(dplyr::select(fake_trees, dim1:dim10, branches), tsne_df, x_color = branches, y_color = branches)
#' }
#'
#' @export
limn_tour_xylink <- function(x,
                             y,
                             x_color = NULL,
                             y_color = NULL,
                             tour_path = tourr::grand_tour(),
                             rescale = clamp,
                             morph = morph_center,
                             reference = x) {
  # generate tour data
  x_color <- rlang::enquo(x_color)
  x_color_tbl <- dplyr::select(x, !!x_color)
  # convert to a matrix, tour cols are everything but color column
  tour_data <- generate_tour_matrix(x,
                                    !!rlang::quo(-!!x_color),
                                    rescale = rescale)
  # set up transformation function
  morph_projection <- generate_morph(morph, p_eff = ncol(tour_data))

  # check embedding table is valid
  stopifnot(ncol(y)  == 2 || ncol(y) == 3)
  y_color <- rlang::enquo(y_color)
  y_color_tbl <- dplyr::select(y, !!y_color)


  # augment y with row_number
  y$row_number <- seq_len(nrow(y))

  # generate app
  ui <- limn_tour_ui("linked", nr = nrow(x) - 1L)
  shiny::shinyApp(ui, server)

}


limn_tour_linked_server <- function(tour_data, tour_path, color_tbl, morph,
                                    y_data, y_color_tbl) {
  path <- tourr::new_tour(tour_data, tour_path)

  half_range <- compute_half_range(tour_data)

  start <- path(0)$proj

  cols <- colnames(tour_data)

  tour_frame <- generate_tour_frame(tour_data, start, half_range,
                                    color_tbl, morph)

  function(input, output, session) {
    output[["tourView"]] <- renderVegawidget({
      spec_tour(tour_frame, color_tbl, half_range)
    })

    # reactiveValues, store current place in tour path
    selections <- shiny::reactiveValues(proj = start, do_tour = FALSE,
                                        force_restart = FALSE)


    # vega-lite event listeners
    # listen for zoom and brush events
    rct_active_zoom <-  vw_shiny_get_signal("tourView",
                                            name = "grid",
                                            body_value = "value")
    # listen for brush events on tour layer
    rct_active_brush <- vw_shiny_get_signal("tourView",
                                            name = "right_brush",
                                            body_value = "value")

    # listen for brush events on embed layer
    rct_embed_brush <- vw_shiny_get_signal("tourView",
                                           name = "left_brush",
                                           body_value = "value")

    rct_half_range <- rct_half_range(rct_active_zoom, half_range)

    rct_tour <- rct_tour(path, tour_data, tour_path, selections = selections)

    rct_proj <- reactive({
      proj <- morph(
        tour_data %*% selections$proj,
        half_range = rct_half_range()
      )
      tbl_projection(tour_frame, proj)
    })



    vw_shiny_set_data("tourView", "embed", y_data)

    vw_shiny_set_data("tourView", "path", rct_proj())


    # if play button is pressed start tour
    shiny::observeEvent(input$play, {
      selections$do_tour <-  input$play
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
        selected_points = rct_active_brush()
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




