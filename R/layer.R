#' Layer Class
#'
#' @return A Layer object
#' @noRd internal
Layer <- R6::R6Class(
  "Layer",
  public = list(
    initialize = function(path, options, fn) {
      self$matcher <- do.call(
        pater::match,
        c(path, options)
      )

      self$handler <- fn
      self$slash <- path == "/" && isFALSE(options$end)
    },
    matcher = NULL,
    handler = NULL,
    path = character(0),
    keys = list(),
    route = NULL,
    method = character(0),
    slash = logical(0)
  ),
  list(
    match = function(path) {}
  )
)
