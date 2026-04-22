#' @noRd
Layer <- R6::R6Class(
  "Layer",
  public = list(
    initialize = function(path, options, fn) {
      path <- if (isTRUE(options$strict)) path else loosen(path)
      options[["trailing"]] <- if (!is.null(options$strict)) !options$strict
      options[["strict"]] <- NULL
      self$matchers <- lapply(path, matcher, options)

      self$handler <- withforward(fn)
      self$slash <- identical(path, "/") && isFALSE(options$end)
    },
    matchers = list(),
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

      idx <- 1
      match <- FALSE
      while (isFALSE(match) && idx <= length(self$matchers)) {
        match <- self$matchers[[idx]](path)
        idx <- idx + 1
      }

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
#' @noRd
#' @keywords internal
loosen <- function(path) {
  if (identical(path, "/")) {
    return(path)
  }
  gsub(pattern = "/+$", replacement = "", path)
}

#' Build a matcher
#'
#' @param path A layer path.
#' @param opts Arguments passed to `pater::match()`.
#'
#' @returns A function to match against request paths.
#' @keywords internal
matcher <- function(path, opts) {
  do.call(
    pater::match,
    c(path, opts)
  )
}
