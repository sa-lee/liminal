# Reactives for a given tour path

#' @importFrom shiny reactive invalidateLater
rct_pause <- function(rct_shift_click) {
  shiny::reactive({
    res <- rct_shift_click()
    length(res) > 0
  })
}

# reactive half range
rct_half_range <- function(rct_zoom, half_range) {
  shiny::reactive({
    res <- rct_zoom()
    res <- unlist(res)
    if (length(res)) {
      return(max(abs(res)))
    }
    return(half_range)
  })
}

rct_tour <- function(plan, aps = 1, fps = 12, rct_event, rct_refresh, session) {
  current <- plan(0)
  shiny::reactive({
    play <- rct_refresh()
    play <- current$step >= 0 && play
    play <- !rct_event() && play


    if (play) {
      current <<- plan(aps/fps)
      shiny::invalidateLater(1000/fps, session)
    }

    current
  })
}

rct_neighbours <- function(.data, num_neighbors = 10) {
  shiny::reactive({
    find_knn(.data, num_neighbors)
  })
}



rct_selection <- function(selection_type, embed_brush, source_values) {
  shiny::reactive({
    active_brush <- embed_brush()
    # no brush, everything selected; early return
    if (length(active_brush) == 0) return(logical(0))
    # one to one selection
    if (identical(selection_type, "linked")) {

      cols <- names(active_brush)
      print(cols)
      print(active_brush)
      selected <- dplyr::between(source_values[[cols[1]]],
                                 active_brush[[cols[1]]][[1]],
                                 active_brush[[cols[1]]][[2]]) &
        dplyr::between(source_values[[cols[2]]],
                       active_brush[[cols[2]]][[1]],
                       active_brush[[cols[2]]][[2]])
    }
    print(source_values[selected,])
    return(selected)
  })
}

stream_axes <- function(rct_tour, cols) {
  shiny::reactive({
    generate_axes(rct_tour()$proj, cols)
  })
}

stream_proj <- function(rct_tour, tour_data, source_values, half_range, morph) {
  shiny::reactive({
    # update tour
    x <- tour_data %*% rct_tour()$proj
    x <- morph(x) / half_range()
    source_values[, c("x","y")] <- as.data.frame(x)
    source_values
  })
}
