#' Link a 2-d embedding with a tour
#'
#' @param embed_data A `data.frame` representing embedding coordinates
#' @param tour_data A `data.frame` that is linked to `embed_data` to tour
#' @param cols Columns in `tour_data` to tour. This can use a `tidyselect` specification
#' such as [tidyselect::starts_with()].
#' @param color An optional bare column name in either `embed_data` or `tour_data` for color
#' mapping the points in the views. If NULL points will be colored black.
#' @param tour_path the tour path to take, the default is [tourr::grand_tour()].
#' @param rescale A function that rescales tour columns. Default is [clamp()]
#' To not perform any scaling use [identity()].
#' @param morph One of `c("center", "centre", "identity", "radial")`
#' that rescales each projection along the tour path. The default
#' is to center the projections and divide by half range. See [morph_center()]
#' for details for each of these functions.
#'
#' @param reference an optional data set to compute a nearest neighbors from.
#' By default the `reference` is set to `NULL` in which case neighbors will be
#' computed from `tour_data`. Otherwise `data.frame`, `matrix` or `dist`
#' objects can be used.
#'
#' @details
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
#' @return After pressing the Done button on the interface, a list of artefacts
#' is returned to the R session.
#'
#' @examples
#' \dontrun{
#' # tour the first ten columns of the fake tree data and link to the
#' # another layout based on t-SNE
#' # loads the default interface
#' tsne <- Rtsne::Rtsne(dplyr::select(fake_trees, dplyr::starts_with("dim")))
#' tsne_df <- data.frame(tsneX = tsne$Y[,1], tsneY = tsne$Y[,2])
#' limn_tour_link(
#'   tsne_df,
#'   dplyr::select(fake_trees, dim1:dim10, branches),
#'   color = branches,
#' )
#' # assigning to an object will return a list of artefacts after clicking
#' # done on the tourr
#' res <- limn_tour_link(
#'   tsne_df,
#'   dplyr::select(fake_trees, dim1:dim10, branches),
#'   color = branches,
#' )
#'
#'}
#'
#' @export
limn_tour_link <- function(embed_data,
                           tour_data,
                           cols,
                           color = NULL,
                           tour_path = tourr::grand_tour(),
                           rescale = clamp,
                           morph = "center",
                           reference = NULL) {

  if (!identical(nrow(tour_data), nrow(embed_data))) {
    stop("tour_data and embed_data should have same number of rows")
  }

  # check embedding table is valid
  stopifnot(ncol(embed_data)  == 2 || ncol(embed_data) == 3)
  # augment embed_data with row_number
  embed_data$row_number <- seq_len(nrow(embed_data))

  cols <- rlang::enquo(cols)
  color <- rlang::enquo(color)

  # setup colors
  if (!rlang::quo_is_null(color)) {
    # is color specification in tour data?
    color_data <- try(dplyr::select(tour_data, !!color), silent = TRUE)
    # column not found...
    if (inherits(color_data, "try-error")) {
      # try embed data, if not there through an error
      color_data <- dplyr::select(embed_data, !!color)
    }
  } else {
    color_data <- NULL
  }

  # generate tour data
  tour_matrix <- generate_tour_matrix(tour_data, cols, rescale = rescale)
  # set up transformation function
  morph_projection <- generate_morph(morph, p_eff = ncol(tour_matrix))


  if (is.null(reference)) {
    reference <- reference_data_knn(dplyr::select(tour_data, !!cols))
  } else {
    if (!identical(nrow(reference), nrow(tour_matrix))) {
      stop("tour_data and reference should have same number of rows")
    }
    reference <- reference_data_knn(reference)
  }

  # generate app
  ui <- gadget_linked_ui()


  server <- limn_tour_linked_server(tour_matrix,
                                    tour_path,
                                    color_data,
                                    morph_projection,
                                    embed_data,
                                    reference)

  app <- shinyApp(ui, server)
  runGadget(app)
}


limn_tour_linked_server <- function(tour_data, tour_path, color_data, morph,
                                    embed_data, reference) {
  path <- tourr::new_tour(tour_data, tour_path)

  half_range <- compute_half_range(tour_data)

  start <- path(0)$proj

  tour_frame <- generate_tour_frame(tour_data, start, half_range, color_data, morph)
  # init k = 1 neighbours
  idx <- seq_len(nrow(tour_frame))
  tour_frame$row_number <- idx

  function(input, output, session) {
    output[["tourView"]] <- renderVegawidget({
      spec_linked_tour(tour_frame, embed_data, color_data, half_range)
    })

    # reactiveValues, store current place in tour path
    selections <- reactiveValues(proj = start,
                                        idx = idx,
                                        do_tour = FALSE,
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

    rct_neighbors <- reactive({

      if (is.null(input$k)) return()
      if (is.na(input$k)) return()

      if (input$k == 1)  {
        selections$idx <- idx
      }
      else {
        selections$idx <- find_knn(reference, num_neighbors = input$k)$idx
      }

      selections$do_tour <- FALSE

    })

    vw_shiny_set_data("tourView", "path", rct_proj())

    # if play button is pressed start tour
    observeEvent(input$play, {
      selections$do_tour <-  input$play
    })

    # if pause button is pressed stop tour
    observeEvent(input$pause, {
      selections$do_tour <- FALSE
    })

    # if brush is active stop tour
    observeEvent(rct_active_brush(), {
      selections$do_tour <- length(rct_active_brush()) == 0
    })


    # if restart, pause tour, and generate new tour path
    observeEvent(input$restart, {
      selections$do_tour <- FALSE
      selections$force_restart <- TRUE
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      tour_artefacts <- list(
        selected_basis = selections$proj,
        tour_brush_box = rct_active_brush(),
        embed_brush_box = rct_embed_brush()
      )
      stopApp(tour_artefacts)
    })

    observe({
      rct_tour()
    })

    output$half_range <- renderText({
      paste("Tour with half-range:", round(rct_half_range(), 3))
    })

  }
}
