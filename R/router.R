#' @title Router
#'
#' @description
#' A port of the [pillarjs/router](https://github.com/pillarjs/router) package
#' for R. Maintains an ordered stack of layers; each layer pairs a path pattern
#' with a middleware function, a nested `Router`, or a `Route` object. Middleware
#' and nested routers are added via `$use()`; routes are added via `$route()` or
#' the HTTP-verb shortcuts. When a request arrives, the stack is walked in order
#' and each matching layer is invoked until the request is handled or the stack
#' is exhausted.
#'
#' * **`forward()` instead of `next()`** -- `next` is a reserved word in R.
#'   `forward("route")` and `forward("router")` work identically to their
#'   Express counterparts.
#' * **`forward` is implicit** -- handlers do not need to declare `forward` as
#'   an argument to call it; it is automatically injected into the handler's
#'   formals if absent. `function(req, res) { forward() }` works just as well
#'   as `function(req, res, forward) { forward() }`.
#' * **`forward` is auto-called** -- if a handler returns without calling
#'   `forward()` or sending a response, `forward()` is called automatically.
#' * **Error handlers take three arguments** -- write error handlers as
#'   `(err, req, res)`; `forward` is injected automatically as the fourth
#'   argument.
#'
#' ## HTTP verb shortcuts
#'
#' `$get()`, `$post()`, `$put()`, `$delete()`, etc. (one per HTTP verb) and
#' `$all()` are convenience wrappers with signature `(path, ...)` equivalent to
#' `router$route(path)$<verb>(...)`. They return `self` invisibly for chaining.
#'
#' @examples
#' router <- Router$new()
#'
#' router$get(
#'   "/get",
#'   function(req, res) {
#'     res$send("Hello there!")
#'   }
#' )$post(
#'   "/post",
#'   function(req, res) {
#'     res$send("Goodbye!")
#'   }
#' )
#'
#' router$get(
#'   "/hello",
#'   function(req, res) {
#'     forward()
#'   },
#'   function(req, res) {
#'     res$send("Hello!")
#'   }
#' )
#'
#' router$post("/bye", \(req, res) {
#'   res$send("Bye!")
#' })
#'
#' router$route("/hi")$get(\(req, res) {
#'   res$send("handling a GET request!")
#' })$post(\(req, res) {
#'   res$send("handling a POST request!")
#' })
#'
#' @export
Router <- R6::R6Class(
  "Router",
  public = list(
    #' @description
    #' Creates a new `Router`.
    #' @param caseSensitive (`logical(1)`)\cr
    #'   When `TRUE`, path matching is case-sensitive (`/Foo` does not match `/foo`).
    #'   Default `FALSE`.
    #' @param mergeParams (`logical(1)`)\cr
    #'   When `TRUE`, `req$params` from a parent router are merged with those of
    #'   this router instead of being replaced. Default `FALSE`.
    #' @param strict (`logical(1)`)\cr
    #'   When `TRUE`, trailing slashes are significant (`/foo/` does not match `/foo`).
    #'   Default `FALSE`.
    #' @return A new `Router` object.
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
    #' @description
    #' Dispatches a request through the router's layer stack. Normally called
    #' by a server or a parent router rather than directly.
    #' @param req (`environment`)\cr Rook request environment.
    #' @param res (`Response`)\cr Response object.
    #' @param callback Final handler.
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
    #' @description
    #' Mounts one or more middleware handlers, optionally scoped to a path prefix.
    #' A [Router] may be passed and will be wrapped automatically. Functions whose
    #' first parameter is named `err` are treated as error handlers.
    #' @param ... (`function | list`)\cr
    #'   An optional leading `character(1)` path prefix, followed by one or more
    #'   handler functions, nested lists of functions, or another [Router].
    #' @return `self` invisibly.
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
    #' @description
    #' Creates a new `Route` for `path` and appends it to the stack.
    #' @param path (`character(1)`)\cr Path pattern.
    #' @return The new `Route` invisibly.
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
    #' @description
    #' Returns the internal layer stack.
    #' @return `list` of `Layer` objects.
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
