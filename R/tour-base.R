limn_tour <- function(.data, cols, color = NULL, tour_path = tourr::grand_tour(), clamp = TRUE, transformer = function(x) scale(x, scale = FALSE), ...) {
  cols <- rlang::enquo(cols)
  color <- rlang::enquo(color)
  # set up tour parameters
  tour_data <- init_tour_matrix(.data, cols, clamp)
  path <- tourr::new_tour(tour_data, tour_path)
  # setup colors
  color_data <- dplyr::select(.data, !!color)

  # generate app
  server <- limn_tour_server(tour_data, path, color_data, transformer)
  ui <- limn_tour_ui("simple")
  shiny::shinyApp(ui, server)

}

limn_tour_ui <- function(view = "simple") {
  view <- match.arg(view, c("simple", "linked"))

  # views always present
  tview <- vegawidget::vegawidgetOutput("tourView")
  aview <- vegawidget::vegawidgetOutput("axisView",
                                        height = "33%")

  tview_ui <-   shiny::fluidRow(tview)

  if (view == "linked") {
    scatter_view <- vegawidget::vegawidgetOutput(
      "scatterView",
      width = "100%",
      height = "80%"
    )
    tview_ui <- shiny::fluidRow(
      shiny::column(4, tview, style='padding:0px;'),
      shiny::column(4, scatter_view)
    )
  }

  shiny::fluidPage(
    tview_ui,
    shiny::fluidRow(
      shiny::column(4, aview, style='padding:0px; margin: 0 auto;')
    )
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

  base_schema[["encoding"]][["x"]][["field"]] <- "x"
  base_schema[["encoding"]][["x"]][["scale"]] <- list(domain = domain)
  base_schema[["encoding"]][["x"]][["axis"]] <- blank_axis()

  base_schema[["encoding"]][["y"]][["field"]] <- "y"
  base_schema[["encoding"]][["y"]][["scale"]] <- list(domain = domain)
  base_schema[["encoding"]][["y"]][["axis"]] <- blank_axis()

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

limn_tour_server <- function(tour_data, path, color_tbl, transformer) {

  init <- init_tour(tour_data, path, color_tbl)

  function(input, output, session) {
    output[["tourView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["tourView"]]),
        embed = vegawidget::vega_embed(actions = FALSE),
        height = 300,
        width = 450
      )
    )
    output[["axisView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init[["axisView"]]),
        embed = vegawidget::vega_embed(actions = FALSE),
        width = 350,
        height = 300
      )
    )
    rct_tour <- rct_tour(path, session = session)
    rct_axes <- stream_axes(rct_tour, init[["cols"]])
    rct_proj <- stream_proj(rct_tour,
                            tour_data,
                            init[["source_values"]],
                            init[["half_range"]],
                            transformer)

    vegawidget::vw_shiny_set_data("axisView", "rotations", rct_axes())
    vegawidget::vw_shiny_set_data("tourView", "path", rct_proj())

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

init_tour_matrix <- function(.data, cols, clamp = TRUE) {
  tour_data <- as.matrix(dplyr::select(.data, !!cols))
  if (clamp) return(clamp(tour_data))
  tour_data
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


