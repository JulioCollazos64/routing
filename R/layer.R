#' Layer Class
#'
#' @examples
#' layer <- Layer$new("/path/:id", list(), identity)
#' layer$match("/PATH/1/") # TRUE
#' layer$params # list(id = "1")
#' layer$keys # "id"
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
    params = list(),
    keys = list(),
    route = NULL,
    method = character(0),
    slash = logical(0),
    match = function(path) {
      if (isTRUE(self$slash)) {
        self$params <- list()
        self$path <- ""
        return(TRUE)
      }

      match <- self$matcher(path)

      if (isFALSE(match)) {
        self$params <- NULL
        self$path <- NULL
        return(FALSE)
      }

      self$params <- match$params
      self$keys <- names(self$params)
      self$path <- match$path

      return(TRUE)
    }
  )
)
