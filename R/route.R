#' Route Class
#'
#' @examples
#'
#' route <- Route$new("/hello")
#' route$get(
#'  function(req, res) {
#'    "a"
#'  },
#'  function(req, res) {
#'    "b"
#'  }
#'  )$post(
#'  function(req, res) {
#'    "c"
#'  },
#'  function(req, res) {
#'    "d"
#'  }
#'  )$put(
#'  function(req, res) {
#'    "e"
#'  }
#')
#'
#' route <- Route$new("/hi")
#' route$all(
#'   \(req, res) {
#'     "a"
#'   },
#'   \(req, res) {
#'     "b"
#'   }
#' )
#' route$methods # list(all = TRUE)
#' lapply(route$stack, function(x)x$method) # list(NULL, NULL)
#' @return A Route object
#' @export
Route <- R6::R6Class(
  "Route",
  public = list(
    initialize = function(path) {
      self$path <- path

      for (method in c(httpMethods, "all")) {
        f <- function(...) {}

        body(f) <- substitute(
          {
            handlers <- list(...)

            for (handler in handlers) {
              stopifnot(isHandler(handler))

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
    stack = list(),
    methods = list(),
    path = character(0)
  ),
  private = list(
    handlesMethod = function(method) {
      if (isTRUE(self$methods$all) || isTRUE(self$methods[[method]])) {
        return(TRUE)
      }

      FALSE
    }
  ),
  lock_objects = FALSE
)

isRoute <- function(object) {
  inherits(object, "Route")
}
