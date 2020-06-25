# UI functions

gadget_tour_titlebar <- function(title) {
  # creates a gadget interface, once user clicks done,
  # return current basiis in view
  gadgetTitleBar(title,
                 left = miniTitleBarCancelButton(), # use escape key or click to end
                 right = miniTitleBarButton("done", "Done", primary = TRUE)
  )
}

gadget_tour_main_panel <- function(axis = TRUE, height = "100%", width = height) {
  tour_view <- vegawidgetOutput("tourView", height = height, width = width)
  padding <- 0

  # print half_rng under the view
  flex_col <- c(2, 1)
  half_range_view <- textOutput(outputId = "half_range")

  if (axis) {
    flex_row <- c(1,1)
    axis_view <- vegawidgetOutput("axisView", height = height, width = width)
    main_panel <- miniContentPanel(
      padding = padding,
      fillCol(
        fillRow(axis_view, tour_view, flex = flex_row),
        half_range_view,
        flex = flex_col
      ), scrollable = FALSE
    )
  } else {
    main_panel <-   miniContentPanel(padding = padding,
                                     fillCol(
                                       flex = flex_col,
                                       tour_view,
                                       half_range_view
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

gadget_tour_ui <- function(title = "liminal tour", axis = TRUE) {
  miniPage(
    gadget_tour_titlebar(title),
    gadget_tour_main_panel(axis),
    gadget_tour_controls()
  )
}


limn_tour_ui <- function(view = "simple", nr) {
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
                                     "Selection Type",
                                     choices = c("linked",
                                                 "neighbors",
                                                 "distance"))
    brush_fixed <- shiny::radioButtons("brush_logic",
                                       "Selection Sequence",
                                       choices = c("or", "and", "independent"))

    brush_knn <- shiny::numericInput("brush_knn",
                                     "Neighbors:",
                                     value = 10,
                                     min = 0,
                                     max = nr)

    bottom_row <- shiny::fluidRow(
      shiny::column(4, aview),
      shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(3, title, play),
                      shiny::column(3, brush_rdo),
                      shiny::column(3, brush_fixed),
                      shiny::column(3, brush_knn)
                    )
      ),
      shiny::column(2, shiny::h5("Half Range"), half_rng)
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
