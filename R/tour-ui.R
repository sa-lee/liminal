# UI functions
# TODO: consider moving everything to shiny modules

gadget_tour_titlebar <- function(linked = FALSE) {
  # creates a gadget interface, once user clicks done,
  # return current basis in view

  title <- "tour"
  if (linked) {
    title <- paste0("embed + ", title)
  }

  tags <- tagList(title, icon("map-o"))

  gadgetTitleBar(
    span(tags),
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
  help <- actionButton("help", label = "Controls", icon = icon("question-circle"))

  miniButtonBlock(
    play,
    pause,
    reset,
    help
  )
}

gadget_tour_ui <- function(linked = FALSE, axis = TRUE) {
  miniPage(
    gadget_tour_titlebar(linked),
    gadget_tour_main_panel(axis),
    gadget_tour_controls()
  )
}
