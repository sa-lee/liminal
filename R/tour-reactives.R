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


rct_tour <- function(plan, selections, aps = 1, fps = 8) {
  current <- plan(0)
  shiny::reactive({
    play <- selections[["do_tour"]]
    play <- current$step >= 0 && play
    #db <- shiny::debounce(rct_event, 1000/(3*fps))
    if (play) {
      current <<- plan(aps/fps)
      selections[["proj"]] <- current$proj
      shiny::invalidateLater(1000*aps/fps)
    }
    current
  })
}
