#' @noRd
Route <- R6::R6Class(
  "Route",
  public = list(
    initialize = function(path) {
      self$path <- path

      for (method in c(httpMethods, "all")) {
        f <- function(...) {}

        body(f) <- substitute(
          {
            handlers <- unlist(list(...))

            stopifnot("Argument handler is required" = length(handlers) > 0)

            for (handler in handlers) {
              stopifnot("handler must be a function" = is.function(handler))

              layer <- Layer$new("/", list(), handler)

              layer[["method"]] <- if (method == "all") NULL else method

              self$stack <- append(
                self$stack,
                list(layer)
              )

              self$methods[[method]] <- TRUE
            }
            invisible(self)
          },
          env = list(
            method = method,
            self = quote(self)
          )
        )

        self[[method]] <- f
      }
    },
    dispatch = function(req, res, done) {
      idx <- 1

      if (length(self$stack) == 0) {
        return(done())
      }

      method <- tolower(req$REQUEST_METHOD)
      req$route <- self

      forward <- function(err = NULL) {
        if (!is.null(err) && identical(err, "route")) {
          return(done())
        }

        if (!is.null(err) && identical(err, "router")) {
          return(done(err))
        }

        if (idx > length(self$stack)) {
          return(done(err))
        }

        match <- FALSE
        while (!match && idx <= length(self$stack)) {
          layer <- self$stack[[idx]]
          idx <<- idx + 1
          match <- is.null(layer$method) || layer$method == method
        }

        if (isFALSE(match)) {
          return(done(err))
        }

        if (!is.null(err)) {
          layer$handleError(err, req, res, forward)
        } else {
          layer$handleRequest(req, res, forward)
        }
      }
      forward()
    },
    handlesMethod = function(method) {
      method <- tolower(method)

      if (isTRUE(self$methods$all) || isTRUE(self$methods[[method]])) {
        return(TRUE)
      }

      FALSE
    },
    stack = list(),
    methods = list(),
    path = character(0)
  ),
  lock_objects = FALSE
)

isRoute <- function(object) {
  inherits(object, "Route")
}
