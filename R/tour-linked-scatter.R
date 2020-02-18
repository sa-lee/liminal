#' View a tour with external context
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
#'
#' @details
#' The linked tour interface consists of three views:
#'  1. the tour view which is a dynamic scatterplot. Brushing on the tour view
#'  is activated with the shift key plus a mouse drag. By default it will
#'  highlight corresponding points in the xy view and pause the animation.
#'  2. the xy view which an interactive scatterplot. Brushing on the xy view
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
#' # tour the first ten columns of the fake tree data and link to the
#' # another layout based on t-SNE
#' if (interactive()) {
#'   # loads the default interface
#'   tsne <- Rtsne::Rtsne(dplyr::select(fake_trees, dplyr::starts_with("dim")))
#'   tsne_df <- data.frame(tsneX = tsne$Y[,1], tsneY = tsne$Y[,2], branches = fake_trees$branches)
#'   limn_tour_xylinked(dplyr::select(fake_trees, dim1:dim10, branches),
#'                      tsne_df,
#'                      x_color = branches,
#'                      y_color = branches)
#' }
#'
#' @export
limn_tour_xylink <- function(x, y, x_color = NULL, y_color = NULL, tour_path = tourr::grand_tour(), rescale = clamp, morph = morph_center) {

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

  x_views[["tourView"]][["encoding"]][["color"]][["condition"]] <-
    c(test = "datum.selectedX === true",
         x_views[["tourView"]][["encoding"]][["color"]][["condition"]][-1]
         )

  x_views[["source_values"]] <- dplyr::bind_cols(
    x_views[["source_values"]],
    selectedX = !logical(nrow(x))
  )

  x_views[["tourView"]][["data"]][["values"]] <- x_views[["source_values"]]

  y_color <- rlang::enquo(y_color)
  y_views <- y_spec(y, !!y_color)

  tspec <- x_views[["tourView"]][["$schema"]]
  hconcat <- list(hconcat = list(x_views[["tourView"]][!names(x_views[["tourView"]]) %in% c("$schema")],
                                 y_views[["y_spec"]])
  )


  x_views[["tourView"]] <- c(`$schema` = tspec, hconcat)

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
    selections <- shiny::reactiveValues(x = !logical(nrow(x)),
                                        y = !logical(nrow(y)),
                                        proj = path(0)$proj
                                        )

    rct_active_zoom <-  vegawidget::vw_shiny_get_signal("tourView",
                                                        name = "grid",
                                                        body_value = "value")

    rct_active_brush <- vegawidget::vw_shiny_get_signal("tourView",
                                                        name = "brush",
                                                        body_value = "value")

    rct_embed_brush <- vegawidget::vw_shiny_get_signal("tourView",
                                                       name = "y_brush",
                                                       body_value = "value")

    rct_half_range <- rct_half_range(rct_active_zoom, x_views[["half_range"]])
    rct_x_brush_active <- rct_pause(rct_active_brush)
    rct_y_brush_active <- rct_pause(rct_embed_brush)



    rct_play <- shiny::eventReactive(input$play, input$play > 0)

    # these set up streaming for tour view
    rct_tour <- rct_tour(path,
                         rct_event = rct_x_brush_active,
                         rct_refresh = rct_play,
                         selections = selections,
                         session = session)

    rct_axes <- stream_axes(rct_tour, x_views[["cols"]])
    rct_proj <- stream_proj(tour_data,
                            x_views[["source_values"]],
                            selections,
                            rct_half_range,
                            morph)


    # observers
    vegawidget::vw_shiny_set_data("axisView", "rotations", rct_axes())

    # only update data to the tour view
    vegawidget::vw_shiny_set_data("tourView", "path", rct_proj())

    shiny::observeEvent(rct_y_brush_active(), {
      # if y_brush is activated, will always highlight
      # the y_view
      active_brush <- rct_embed_brush()
      # setup selection

      # update selections for embed view
      transient_brush_update(y_views[["y_spec"]][["data"]][["values"]],
                             active_brush,
                             selections,
                             view = "y",
                             name = "embed",
                             session)

      selections[["x"]] <- selections[["y"]]
      vals_x <- rct_proj()
      message_view("path", vals_x, session)
    })

    # shiny::observeEvent(rct_x_brush_active(), {
    #   # if x_brush is activated, will always highlight
    #   # the x_view
    #   active_brush <- rct_active_brush()
    #   # update selections for embed view

    #
    #   # setup selection
    #   sel_seq <- selection_sequence(input$brush_logic)
    #   selections[["y"]] <- sel_seq(selections[["y"]], selections[["x"]])
    #
    # })

    output$half_range <- shiny::renderPrint({
      # protects against initial NULL
      list(rct_half_range(),
           table(selections$y),
           table(selections$x),
           selections$proj)
    })
  }

  # generate app
  ui <- limn_tour_ui("linked", nr = nrow(x) - 1L)
  shiny::shinyApp(ui, server)

}


y_spec <- function(y, y_color) {
  # set up linked data
  y_color <- rlang::enquo(y_color)
  y_data <- dplyr::select(y, dplyr::everything(), !!y_color)
  stopifnot(ncol(y_data) <= 3)
  y_data[["selectedY"]] <- TRUE

  xf <- names(y_data)[1]
  yf <- names(y_data)[2]
  colf <- names(y_data)[3]
  coltype <- color_type(y_data[,3])

  domain_x <- range(y_data[[xf]], na.rm = TRUE)
  domain_y <- range(y_data[[yf]], na.rm = TRUE)
  domain <- range(c(domain_x, domain_y))

  encoding <- list(encoding =
                     list(x = list(field = xf, type = "quantitative", scale = list(domain = domain)),
                          y = list(field = yf, type = "quantitative", scale = list(domain = domain)),
                          color = list(
                            condition = list(
                              test = "datum.selectedY === true",
                              field = colf,
                              type = coltype
                            ),
                            value = "grey"

                          ),
                          opacity = list(
                            condition = list(selection = "colclick", value = 0.9),
                            value = opacity_value(nrow(y_data))
                          )
                     )
  )

  mark <- list(mark = list(type = "circle"))
  selection <- list(selection = list("y_brush" = list(type = "interval")))
  data <- list(data = list(name = "embed", values = y_data))

  y_spec <- c(data,
              encoding,
              mark,
              selection)
  list(y_spec = y_spec)
}

inside_brush <- function(cols, vals, brush) {
  dplyr::between(vals[[cols[1]]],
                 brush[[cols[1]]][[1]],
                 brush[[cols[1]]][[2]]) &
    dplyr::between(vals[[cols[2]]],
                   brush[[cols[2]]][[1]],
                   brush[[cols[2]]][[2]])
}


transient_brush_update <- function(vals, brush, current, view, name, session) {
  view <- match.arg(view, c("x", "y"))
  not_in_view <- setdiff(c("x", "y"), view)
  target <-paste0("selected", toupper(view))
  if (length(brush) > 0) {
    cols <- names(brush)
    current[[view]] <-  inside_brush(cols, vals, brush)
    vals <- dplyr::bind_cols(
      dplyr::select(vals, -target),
      !!rlang::sym(target) := current[[view]]
    )
  } else {
    current[[view]] <- current[[not_in_view]]
  }

  message_view(name, vals, session)

}

message_view <- function(name, vals, session) {
  message <- list(outputId = "tourView",
                  name = name,
                  data_insert = vals ,
                  data_remove = TRUE,
                  run = TRUE)
  session$sendCustomMessage("changeData", message)
}


selection_sequence <- function(logic) {
  switch(logic,
         "independent" = `%||%`,
         "and" = `&`,
         "or" = `|`,
         stop("Unknown option:", logic)
  )
}
