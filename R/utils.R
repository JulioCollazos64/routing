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
  inherits(x, what = c("promise", "future", "mirai"))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
