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
      self$caseSensitive <- caseSensitive
      self$mergeParams <- mergeParams
      self$strict <- strict

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
    use = function(...) {
      path <- "/"
      offset <- 1

      if (is.character(..1)) {
        path <- ..1
        offset <- 2
      }

      handlers <- unlist(list(...)[offset:...length()])

      for (handler in handlers) {
        if (isRouter(handler)) {
          handler <- function(req, res, forward) {
            handler$handle(req, res, forward)
          }
        }
        stopifnot("handler must be a function" = is.function(handler))

        layer <- Layer$new(
          path = path,
          list(
            sensitive = self$caseSensitive,
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
          sensitive = self$caseSensitive,
          strict = self$strict,
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
    },
    caseSensitive = logical(0),
    mergeParams = logical(0),
    strict = logical(0)
  ),
  private = list(
    stack = list()
  ),
  lock_objects = FALSE
)

isRouter <- function(object) {
  inherits(object, "Router")
}
