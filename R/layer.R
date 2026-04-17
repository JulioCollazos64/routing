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
      path <- if (isTRUE(options$strict)) path else loosen(path)
      options[["trailing"]] <- if (!is.null(options$strict)) !options$strict
      options[["strict"]] <- NULL
      self$matcher <- do.call(
        pater::match,
        c(path, options)
      )

      if (!("forward" %in% names(formals(fn)))) {
        formals(fn)[["forward"]] <- quote(expr = )
      }

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
    handleError = function(error, req, res, forward) {
      fn <- self$handler
      if (length(formalArgs(fn)) != 4) {
        return(forward(error))
      }

      tryCatch(
        expr = {
          fn(error, req, res, forward)
        },
        error = function(err) {
          forward(err)
        }
      )
    },
    handleRequest = function(req, res, forward) {
      fn <- self$handler

      if (length(formalArgs(fn)) > 3) {
        return(forward())
      }

      tryCatch(
        expr = {
          ret <- fn(req, res, forward)

          if (isResponse(ret) || inherits(ret, "forward")) {
            return(ret)
          }

          forward()
        },
        error = function(err) {
          forward(err)
        }
      )
    },
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

#' Loosen a path
#'
#' @examples
#' loosen("/hello/") # "/hello"
#' loosen("/hello//") # "/hello"
#'
loosen <- function(path) {
  if (identical(path, "/")) {
    return(path)
  }
  gsub(pattern = "/+$", replacement = "", path)
}
