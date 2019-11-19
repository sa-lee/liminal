limn_tour <- function(.data, cols, color = NULL, tour_path = tourr::grand_tour(), clamp = TRUE, ...) {
  cols <- rlang::enquo(cols)
  color <- rlang::enquo(color)

  # set up tour parameters
  tour_data <- as.matrix(dplyr::select(.data, !!cols))
  color_data <- dplyr::select(.data, !!color)
  if (clamp) {
    tour_data <- clamp(tour_data)
  }
  path <- tourr::new_tour(tour_data, tour_path)

  server <- limn_tour_server(tour_data, path, color_data)
  ui <- limn_tour_ui()
  shiny::shinyApp(ui, server)

}

limn_tour_ui <- function() {
  shiny::fluidPage(
    shiny::fluidRow(
      vegawidget::vegawidgetOutput("tourView", width = "100%")
    ),
    shiny::fluidRow(
      vegawidget::vegawidgetOutput("axisView", width = "100%", height = "33%")
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
      base_schema[["encoding"]][["color"]][["condition"]][["scale"]] <- list(domain = col_domain)
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

limn_tour_server <- function(tour_data, path, color_tbl) {

  # half range
  half_range <- compute_half_range(tour_data)

  # intialise views
  start <- path(0)$proj
  source_values <- tour_data %*% start
  colnames(source_values) <- c("x", "y")
  source_values <- as.data.frame(source_values)

  if (ncol(color_tbl) == 1) {
    source_values <- dplyr::bind_cols(source_values, color_tbl)
  }

  init_tour <- render_init(source_values, half_range)
  cols <- colnames(tour_data)
  init_axes <- render_init_axes(start, half_range, cols)

  function(input, output, session) {
    output[["tourView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init_tour),
        embed = vegawidget::vega_embed(actions = FALSE)
      )
    )
    output[["axisView"]] <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(
        vegawidget::as_vegaspec(init_axes),
        embed = vegawidget::vega_embed(actions = FALSE)
      )
    )
    rct_tour <- rct_tour(path, session = session)
    rct_axes <- stream_axes(rct_tour, cols)
    rct_proj <- stream_proj(rct_tour,  tour_data, source_values, half_range)

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


