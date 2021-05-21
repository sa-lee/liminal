# UI functions
# TODO: consider moving everything to shiny modules

gadget_tour_titlebar <- function() {
  # creates a gadget interface, once user clicks done,
  # return current basiis in view
  gadgetTitleBar(
    textOutput(outputId = "half_range", inline = TRUE),
    left = miniTitleBarCancelButton(), # use escape key or click to end
    right = miniTitleBarButton("done", "Done", primary = TRUE)
  )
}

gadget_tour_main_panel <- function(axis = TRUE, height = "100%", width = height) {
  tour_view <- vegawidgetOutput("tourView", height = height, width = width)

  if (axis) {
    flex_row <- c(1, 2)
    axis_view <- vegawidgetOutput("axisView", height = height, width = width)
    main_panel <- miniContentPanel(
      padding = 0,
      fillCol(
        fillRow(axis_view, tour_view, flex = flex_row),
        # half_range_view,
        flex = 1
      ), scrollable = FALSE
    )
  } else {
    main_panel <- miniContentPanel(
      padding = 0,
      fillCol(
        flex = 1,
        tour_view
      )
    )
  }

  main_panel
}


gadget_tour_controls <- function() {
  play <- actionButton("play", "Play", icon = icon("play"))
  reset <- actionButton("restart", "Restart", icon = icon("refresh"))
  pause <- actionButton("pause", "Pause", icon = icon("pause"))

  miniButtonBlock(
    play,
    pause,
    reset
  )
}

gadget_tour_ui <- function(axis = TRUE) {
  miniPage(
    gadget_tour_titlebar(),
    gadget_tour_main_panel(axis),
    gadget_tour_controls()
  )
}


gadget_linked_ui <- function() {
  miniPage(
    gadget_tour_titlebar(),
    miniTabstripPanel(
      miniTabPanel("Controls",
        icon = icon("sliders"),
        miniContentPanel(
          numericInput("k", "Neighbors:",
            value = 1, min = 1, max = 25
          ),
          radioButtons("metric",
            "Distance Metric",
            selected = "euclidean",
            choices = c(
              "euclidean",
              "cosine",
              "manhattan"
            )
          )
        )
      ),
      miniTabPanel("Embed + Tour",
        icon = icon("map-o"),
        gadget_tour_main_panel(axis = FALSE),
        gadget_tour_controls()
      )
    )
  )
}
