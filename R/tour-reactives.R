# Reactives for a given tour path

#' @importFrom shiny reactive invalidateLater
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


rct_tour <- function(plan, tour_data, tour_path, selections, aps = 1, fps = 8) {
  current <- plan(0)
  shiny::reactive({
    play <- selections[["do_tour"]]
    play <- current$step >= 0 && play

    if (play) {
      current <<- plan(aps/fps)
      selections[["proj"]] <- current$proj
      shiny::invalidateLater(1000*aps/fps)
    }

    restart <- selections[["force_restart"]]

    if (restart) {
      plan <<- tourr::new_tour(tour_data, tour_path)
      current <<- plan(0)
      selections[["proj"]] <- current$proj
      selections[["force_restart"]] <- FALSE
    }

    current
  })
}
