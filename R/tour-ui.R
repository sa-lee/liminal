# UI functions
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
                                     "Selection Type",
                                     choices = c("linked",
                                                 "neighbors",
                                                 "centroids"))
    brush_fixed <- shiny::radioButtons("brush_logic",
                                       "Selection Sequence",
                                       choices = c("independent", "or", "and")
    )

    brush_knn <- shiny::numericInput("brush_knn",
                                     "Neighbors:",
                                     value = 10,
                                     min = 0)

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
