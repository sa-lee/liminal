#' Exploring t-SNE embeddings interactively
#' 
#' @param data a data.frame or matrix
#' @param embedding list output from `Rtsne::Rtsne()`
#' @param colour a column name in `data` corresponding to categorical variable
#' @param max_bases number of bases for grand tour (see `tourr::save_history()`)
#' @param ... other stuff I want to include. 
#' 
#' @details 
#' Produces a 2 by 2 panel layout, where the upper left quadrant contains
#' 2-d projections from a grand tour (1), the upper right quadrant contains
#' the 2-d t-SNE embedding (2), the lower left quadrant contains strip plots
#' of each rescaled variable in `data` (3), the lower right quadrant contains
#' a scatter plot of distance in embedding space against distance in the data 
#' space (4).  
#' 
#' UI elements and interactions
#' 
#' A range slider is bound to the side bar panel, with a play button which
#' will animate the projections in (1).
#' 
#' There are two brushing selections available in (2): a mouse drag
#' on (2) will highlight selected points on (1) and (2) and (3). This
#' is useful for both seeing how clusters that form in embedding space move
#' in high-dimensional space and what variables define the cluster from 
#' (3). Pressing shift with a mouse drag will also highlight selected points
#' but this time revealing the simplex constructed on the embedding space
#' and how this simplex appears in high-dimensional space. 
#' 
#' The scatter plot (4) of inter point distances, can be used for checking
#' spurious embeddings... 
#' 
#' The strip plots can be used for highlighting point ranges ...
#'           
#' 
#' @return a shiny app
sneezy <- function(data, embedding, colour = NULL, max_bases) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install shiny", call. = FALSE)
  }
  
  ui <- sneezy_ui(max_bases)
  server <- sneezy_server(data, embedding, colour, max_bases)
  
  shiny::shinyApp(ui, server)
  
}

sneezy_ui <- function(max_bases) {
  
  tour_slider <- shiny::sliderInput(
    "n_bases",
    label = "Number of bases",
    min = 1,
    value = 1,
    max = max_bases,
    step = 1,
    sep = "",
    animate = shiny::animationOptions(interval = 250, loop = TRUE)
  )
  
  centroids_box <- shiny::checkboxInput("addGraph", 
                                    label = "Add nearest neighbours graph?")
  
  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        tour_slider,
        centroids_box,
        vegawidget::vegawidgetOutput("axes"),
        vegawidget::vegawidgetOutput("dist")
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(width = 8, vegawidget::vegawidgetOutput("chart"))
          )
      )
    )
  )
  
  
}

sneezy_server <- function(data, embedding, colour, max_bases) {
  
  if (is.matrix(data)) {
    valid_cols <- apply(data, 2, is.numeric)
  } else {
    valid_cols <- vapply(data, is.numeric, logical(1))
  }
  
  
  colnames(data) <- gsub(pattern = "\\.", "_", colnames(data))
  
  history <- tourr::save_history(
    data[, valid_cols, drop = FALSE],
    max_bases = max_bases
  )
  
  # rescale data
  tour_data <- tourr::rescale(data[, valid_cols])
  tour_data <- scale(tour_data, center = TRUE, scale = FALSE)
  half_range  <- max(sqrt(rowSums(tour_data^2)))
  
  
  #history <- tourr::interpolate(history)
  # initalise projections, colour variables
  if (!is.null(colour)) {
    tbl_colour <- cbind(as.data.frame(data[, valid_cols]), data[, colour, drop = FALSE])
  } else {
    tbl_colour <- as.data.frame(data[, valid_cols])
  }
  
  # static tsne_projection
  tbl_tsne <- data.frame(tsne_x = embedding$Y[,1], 
                         tsne_y = embedding$Y[,2])
  
  # first basis 
  tbl_tour <- as.data.frame(tour_data %*% matrix(history[,,1], 
                                                 nrow = ncol(tour_data),
                                                 ncol = 2L)
  )
  
  # all data going to specs
  tbl_init <- cbind(tbl_tour, tbl_tsne, tbl_colour)
  
  # for drawing axis
  tbl_zeros <- matrix(0, nrow = nrow(history[,,1]), ncol = 3)
  tbl_zeros[,3] <- seq_len(nrow(history[,,1]))
  spec_rotations <- spec_axes(half_range)
  
  # set up panels
  panel_tour <- spec_tour(half_range, colour = colour)
  panel_tsne <- spec_projection(embedding, colour = colour)
  panel_dot <- spec_dot(colnames(tour_data), colour = colour)
  spec_dist <- NULL
  
  
  # projections + tsne spec + pairwise distance plots
  spec <- list(
    hconcat = list(panel_tour, panel_tsne)
  )
  
  spec <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "projections", values = tbl_init),
    vconcat = list(spec, panel_dot)
  )
  
  jsonlite::write_json(spec, "spec_example.json", auto_unbox = TRUE, pretty = TRUE)
  
  spec <- vegawidget::as_vegaspec(spec)
  
  opt <- vegawidget::vega_embed(actions = FALSE)
  
  function(input, output) {
    
    output$dist <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(spec_dist, embed = opt)
    )
    
    cur_path <- shiny::reactive({
      matrix(history[,,input$n_bases], 
             nrow = ncol(tour_data), 
             ncol = 2L, 
             byrow = TRUE)
    })
    
    rct_axes <- shiny::reactive({
      path <- cur_path()
      path <- rbind(
        cbind(path, seq_len(nrow(path))),
        tbl_zeros
      )
      colnames(path) <- c("x", "y", "group")
      path <- as.data.frame(path)
      path$axis_name <- c(colnames(tour_data), rep("", nrow(tbl_zeros)))
      path
    })
    
    vegawidget::vw_shiny_set_data("axes", "rotations", rct_axes())
    
    output$axes <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(spec_rotations, embed = opt)
    )
    
    rct_data <- shiny::reactive({
      tbl_init[, c(1,2)] <- tour_data %*% cur_path()
      tbl_init
    })
    
    vegawidget::vw_shiny_set_data("chart", "projections", rct_data())
    spec <- vegawidget::vegawidget(spec, embed = opt)
    output$chart <- vegawidget::renderVegawidget(spec)
  }
  
}