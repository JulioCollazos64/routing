#' Router Class
#'
#' @examples
#'
#' router <- Router$new()
#'
#' router$get(
#'   "/get",
#'   function(req, res) {
#'     "a"
#'   }
#' )$post(
#'   "/post",
#'   function(req, res) {
#'     "b"
#'   }
#' )
#'
#' router$get(
#'   "/hello",
#'   function(req, res) {
#'     "a"
#'   },
#'   function(req, res) {
#'     "b"
#'   }
#' )
#'
#' router$post("/bye", \(req, res) {
#'   "bye!"
#' })
#'
#'
#' router$route("/hi")$get(\(req, res) {
#'   "?"
#' })$post(\(req, res) {
#'   "!"
#' })
#'
#'
#' @return A Router object
#' @export
Router <- R6::R6Class(
  "Router",
  public = list(
    initialize = function(
      caseSensitive = FALSE,
      mergeParams = FALSE,
      strict = FALSE
    ) {
      private$caseSensitive <- caseSensitive
      private$mergeParams <- mergeParams
      private$strict <- strict

      for (method in c(httpMethods, "all")) {
        f <- function(path, ...) {}
        body(f) <- substitute({
          stopifnot(
            "Must provide a path as the first argument" = is.character(path)
          )
          route <- self$route(path)
          do.call(
            route[[method]],
            list(...)
          )
          invisible(self)
        })

        self[[method]] <- f
      }
    },
    handle = function(req, res, callback) {
      if (missing(callback)) {
        stop("argument callback is required", call. = FALSE)
      }

      idx <- 1
      removed <- ""
      slashAdded <- FALSE

      # inter-router variables
      parentParams <- req$params
      parentUrl <- req$baseUrl %||% ""

      # setup basic req values
      req$baseUrl <- parentUrl
      req$originalUrl <- req$originalUrl %||% req$PATH_INFO

      forward <- function(err = NULL) {
        # signal that handler called forward
        do.call(
          on.exit,
          list(
            substitute({
              return(
                structure(
                  returnValue() %||% list(),
                  class = "forward"
                )
              )
            })
          ),
          envir = parent.frame()
        )

        layerError <- if (identical(err, "route")) NULL else err

        if (slashAdded) {
          req$PATH_INFO <<- gsub("^.", "", req$PATH_INFO)
          slashAdded <<- FALSE
        }

        # restore altered req.url
        if (nzchar(removed)) {
          req$baseUrl <<- parentUrl
          req$PATH_INFO <<- paste0(removed, req$PATH_INFO)

          removed <<- ""
        }

        # signal to exit router
        if (identical(layerError, "router")) {
          return()
        }

        # no more matching layers
        if (idx > length(private$stack)) {
          return()
        }

        path <- req$PATH_INFO

        # find the next matching layer
        match <- FALSE
        while (!match && idx <= length(private$stack)) {
          layer <- private$stack[[idx]]
          idx <<- idx + 1
          match <- layer$match(path)
          route <- layer$route

          if (!match) {
            next
          }

          # process non-route handlers normally
          if (is.null(route)) {
            next
          }

          # routes do not match with a pending error
          if (!is.null(layerError)) {
            match <- FALSE
            next
          }

          method <- req$REQUEST_METHOD
          hasMethod <- route$handlesMethod(method)

          if (!hasMethod) {
            match <- FALSE
          }
        }

        # no match
        if (!match) {
          return()
        }

        # see: https://expressjs.com/en/5x/api.html#req.route
        if (!is.null(route)) {
          req$route <- route
        }

        # capture one-time layer values
        req$params <- if (private$mergeParams) {
          c(layer$params, parentParams)
        } else {
          layer$params
        }

        layerPath <- layer$path

        if (!is.null(err)) {
          forward(layerError %||% err)
        } else if (!is.null(route)) {
          layer$handleRequest(req, res, forward)
        } else {
          trimPrefix(
            layer,
            layerError,
            layerPath,
            path
          )
        }
      }

      trimPrefix <- function(layer, layerError, layerPath, path) {
        if (nzchar(layerPath)) {
          prefix <- substring(path, 1, last = nchar(layerPath))

          if (!identical(layerPath, prefix)) {
            forward(layerError)
            return()
          }

          n <- nchar(layerPath) + 1L
          c <- substring(path, n, n)

          if (nzchar(c) && c != "/") {
            forward(layerError)
            return()
          }

          # strip prefix from path
          removed <<- layerPath
          req$PATH_INFO <<- substring(req$PATH_INFO, nchar(removed) + 1L)

          # ensure leading slash
          if (!startsWith(req$PATH_INFO, "/")) {
            req$PATH_INFO <<- paste0("/", req$PATH_INFO)
            slashAdded <<- TRUE
          }

          # update baseUrl (no trailing slash)
          req$baseUrl <<- paste0(
            parentUrl,
            if (endsWith(removed, "/")) {
              substring(removed, 1L, nchar(removed) - 1L)
            } else {
              removed
            }
          )
        }

        if (!is.null(layerError)) {
          layer$handleError(layerError, req, res, forward)
        } else {
          layer$handleRequest(req, res, forward)
        }
      }

      forward()
    },
    use = function(...) {
      path <- "/"
      offset <- 1

      if (is.character(..1)) {
        path <- ..1
        offset <- 2
      }

      handlers <- unlist(list(...)[offset:...length()])

      if (!length(handlers)) {
        stop("argument handler is required", call. = FALSE)
      }

      for (handler in handlers) {
        if (isRouter(handler)) {
          f <- function(req, res, forward) {}
          body(f) <- substitute(
            {
              router$handle(req, res, forward)
            },
            env = list(router = handler)
          )
          handler <- f
        }

        stopifnot("handler must be a function" = is.function(handler))

        layer <- Layer$new(
          path = path,
          list(
            sensitive = private$caseSensitive,
            strict = FALSE,
            end = FALSE
          ),
          handler
        )

        layer$route <- NULL

        private$stack <- append(
          private$stack,
          list(
            layer
          )
        )
      }

      invisible(self)
    },
    route = function(path) {
      route <- Route$new(path)

      handle <- function(req, res, forward) {
        route$dispatch(req, res, forward)
      }

      layer <- Layer$new(
        path,
        list(
          sensitive = private$caseSensitive,
          strict = private$strict,
          end = TRUE
        ),
        handle
      )

      layer$route <- route

      private$stack <- append(
        private$stack,
        list(
          layer
        )
      )

      invisible(route)
    },
    getStack = function() {
      private$stack
    }
  ),
  private = list(
    stack = list(),
    caseSensitive = logical(0),
    mergeParams = logical(0),
    strict = logical(0)
  ),
  lock_objects = FALSE
)

isRouter <- function(object) {
  inherits(object, "Router")
}
