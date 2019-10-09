# everything here should produce it's own valid specs
spec_slider <- function(data) {
  slider <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "slider", values = data),
    transform = list(list(window = list(list(op = "row_number", as = "index")))
                     ),
    mark = list(type = "tick", clip = TRUE),
    encoding = list(
      x = list(field = "index", type = "quantitative"),
      fill = list(field = "is_anchor", type = "nominal")
    )
  )
  vegawidget::as_vegaspec(slider)
}

encoding_color <- function(colour = NULL) {
  if (!is.null(colour)) {
    return(
      list(condition = 
             list(
               selection = list(`or` = list("variableBrush", "embeddingBrush")),
               field = colour, 
               type = "nominal"
             ),
           value = "grey"
      )
    )
  }
  
  list(
    condition = list(
      selection = list(`or` = list("variableBrush", "embeddingBrush")),
      value = "black"
    ),
    value = "grey"
  )
  
  list(
    condition = list(
      selection = list(`or` = list("variableBrush", "embeddingBrush")),
      value = "black"
    ),
    value = "grey")
  
}

spec_tour <- function(half_range, colour = NULL) {
  domain <- c(-half_range, half_range)
  
  tour_layer <- list(
    mark = list(type = "circle", clip = TRUE),
    encoding = list(
      x = list(field = "V1", type = "quantitative", 
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      y = list(field = "V2", type = "quantitative",
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      color = encoding_color(colour)
    )
  )
  
  tour_layer
}



spec_projection <- function(coords, colour = NULL) {
  half_range <- max(sqrt(rowSums(coords$Y^2)))
  domain <- c(-half_range, half_range)
  nl_layer <- list(
    title = paste("t-SNE with perplexity", coords$perplexity),
    mark = list(type = "circle", clip = TRUE),
    selection = list(embeddingBrush = list(type = "interval")),
    encoding = list(
      
      x = list(field = "tsne_x", type = "quantitative", 
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      y = list(field = "tsne_y", type = "quantitative",
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      color = encoding_color(colour)
    )
  )
  nl_layer
}


spec_axes <- function(half_range) {
  domain <- c(-half_range, half_range)
  axes_layer <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "rotations"),
    encoding = list(
      x = list(field = "x", type = "quantitative", 
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      y = list(field = "y", type = "quantitative",
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      )
    ),
    layer = list(
      list(
        mark = list(type = "line", clip = TRUE),
        encoding = list(
          order = list(field = "group", type = "nominal"),
          color = list(value = "black")
        )
      ),
      list(
        # transform = list(list(filter = list(field = "axis_name", valid = TRUE))),
        mark = list(type = "text", align = "left", dy = -3),
        encoding = list(
          text = list(field = "axis_name", type = "nominal")
        )
      )
    )
  )
vegawidget::as_vegaspec(axes_layer)
}


spec_shep <- function(dist) {
  domain <- c(0, max(dist) + 0.1)
  shep_layer <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "shep", values = dist),
    mark = list(type = "circle", clip = TRUE),
    encoding = list(
      x = list(field = "original", type = "quantitative",
               scale = list(domain = domain)),
      y = list(field = "embedding", type = "quantitative",
               scale = list(domain = domain)),
      color = list(value = "black"),
      opacity = list(value = 1 / 10)
    )
  )
  vegawidget::as_vegaspec(shep_layer)
}


spec_dot <- function(vars, colour = NULL) {
  dot <- list(
    height = 300,
    # data = list(name = "source", values = tour_data),
    `repeat` = list(row = vars),
    spec = list(
      selection = list("variableBrush" = list(type = "interval", resolve = "global")),
      mark = list(type = "tick"),
      encoding = list(
        x = list(field = list(`repeat` = "row"), type = "quantitative"),
        fill = encoding_color(colour)
      )
    )
  )
  dot
}


