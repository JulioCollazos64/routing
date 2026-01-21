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

isHandler <- function(x) {
  is.function(x) && length(formalArgs(x)) == 2
}
