isPromise <- function(x) {
  inherits(x, what = c("promise", "future", "mirai"))
}
