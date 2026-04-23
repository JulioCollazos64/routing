httpMethods <- c(
  "get",
  "head",
  "post",
  "put",
  "delete",
  "connect",
  "options",
  "trace",
  "patch"
)

isPromise <- function(x) {
  inherits(x, what = c("promise", "Future", "mirai"))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' httpuv response
#'
#' @keywords internal
#' @noRd
isResponse <- function(x) {
  is.list(x) && all(names(x) %in% c("status", "headers", "body"))
}

#' Add forward as a function argument
#'
#' @keywords internal
#' @noRd
withforward <- function(fn) {
  if ("forward" %in% names(formals(fn))) {
    return(fn)
  }

  formals(fn)[["forward"]] <- quote(expr = )
  fn
}
