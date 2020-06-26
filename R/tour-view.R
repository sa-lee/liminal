

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

  base_schema[["mark"]] <- list(type = "circle", clip = TRUE)
  base_schema[["encoding"]][["x"]][["field"]] <- "x"
  base_schema[["encoding"]][["x"]][["scale"]] <- list(domain = domain)
  base_schema[["encoding"]][["x"]][["axis"]] <- blank_axis()

  base_schema[["encoding"]][["y"]][["field"]] <- "y"
  base_schema[["encoding"]][["y"]][["scale"]] <- list(domain = domain)
  base_schema[["encoding"]][["y"]][["axis"]] <- blank_axis()


  # selection on shift-click
  shift_click <- "[mousedown[event.shiftKey], mouseup] > mousemove"
  base_schema[["selection"]][["brush"]][["on"]] <- shift_click
  base_schema[["selection"]][["brush"]][["translate"]] <- shift_click
  base_schema[["selection"]][["brush"]][["zoom"]] <- FALSE


  # pan + zoom
  base_schema[["selection"]][["grid"]] <-  list(type = "interval",
                                                bind = "scales",
                                                translate = FALSE)
  if (ncol(source_values) == 3) {
    col_nm <- colnames(source_values)[3]
    type <- color_type(source_values[[col_nm]])
    base_schema[["encoding"]][["color"]][["condition"]][["field"]] <- col_nm
    base_schema[["encoding"]][["color"]][["condition"]][["type"]] <- type
    if (type %in% c("nominal", "ordinal")) {
      col_domain <- unique(source_values[[col_nm]])
      base_schema[["encoding"]][["color"]][["condition"]][["scale"]] <- list(domain = sort(col_domain))
    }
    # if color available enable clickable legend
    base_schema[["selection"]][["colclick"]] <- list(type = "multi",
                                                     fields = list(col_nm),
                                                     bind = list(legend = "dblclick"))
    op_value <- opacity_value(nrow(source_values))
    base_schema[["encoding"]][["opacity"]] <- list(condition = list(selection = "colclick", value = 0.9), value = op_value)


  } else {
    col_nm <- "black"
    base_schema[["encoding"]][["color"]][["condition"]] <- list(selection = "brush",
                                                                value = col_nm)
  }
  base_schema[["data"]][["name"]] <- "path"
  base_schema[["data"]][["values"]] <- source_values
  base_schema
}



generate_tour_frame <- function(tour_data, start, half_range, color_tbl, morph) {
  # intialise frame
  source_values <- tour_data %*% start
  source_values <- morph(source_values, half_range)
  colnames(source_values) <- c("x", "y")
  source_values <- as.data.frame(source_values)
  dplyr::bind_cols(source_values, color_tbl)
}


spec_tour <- function(tour_frame, half_range) {

  view_tour <- render_init(tour_frame, half_range)

  vegawidget::vegawidget(
    vegawidget::as_vegaspec(view_tour),
    embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE)
  )
}


opacity_value <- function(nr, pow = 0.3) (1 / nr)^pow
