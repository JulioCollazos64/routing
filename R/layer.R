#' Layer Class
#'
#' @return A Layer object
#' @noRd internal
Layer <- R6::R6Class(
  "Layer",
  public = list(
    initialize = function(path, options, fn) {
      self$path <- path
      self$matcher <- do.call(
        pater::match,
        c(path, options)
      )

      self$handler <- fn
    },
    matcher = NULL,
    handler = NULL,
    path = character(0),
    keys = list(),
    route = NULL,
    method = character(0)
  )
)
