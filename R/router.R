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

      for (method in httpMethods) {
        f <- function(path, ...) {}
        body(f) <- substitute({
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
    route = function(path) {
      route <- Route$new(path)

      layer <- Layer$new(
        path,
        list(
          self$caseSensitive,
          self$strict,
          end = TRUE
        ),
        identity
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
