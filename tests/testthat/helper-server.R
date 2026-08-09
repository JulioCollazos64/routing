Response <- R6::R6Class(
  "Response",
  public = list(
    status = NULL,
    headers = list(),
    body = NULL,
    send = function(body = NULL) {
      list(
        status = self$status,
        headers = self$headers,
        body = body
      )
    }
  )
)

createServer <- function(router) {
  if (!isRouter(router)) {
    handler <- router
  }

  staticsPaths <- router$.__enclos_env__$private$statics
  handler <- function(req, res, callback) {
    router$handle(req, res, callback)
  }

  list(
    call = function(req) {
      res <- Response$new()
      handler(req, res, finalHandler(req, res))
    },
    staticPaths = staticsPaths %||% NULL
  )
}

saw <- function(req, res, forward) {
  res$status <- 200L
  body <- paste("saw", req$REQUEST_METHOD, req$PATH_INFO)
  res$send(body)
}


setsawBase <- function(num) {
  name <- paste0("x-saw-base", num)
  function(req, res, forward) {
    res$headers[[name]] <- req$baseUrl
  }
}

sawBase <- function(req, res, forward) {
  res$status <- 200L
  res$send(paste("saw", req$baseUrl))
}


helloWorld <- function(req, res, forward) {
  res$status <- 200L
  res$send("hello, world")
}


createHitHandle <- function(num) {
  name <- paste0("x-fn-", num)
  function(req, res, forward) {
    res$headers[[name]] <- "hit"
    forward()
  }
}

shouldHitHandle <- function(num) {
  header <- paste0("x-fn-", num)
  function(r) {
    expect_equal(
      r$headers[[header]],
      "hit",
      label = paste("handle", num, "should be hit")
    )
  }
}

shouldNotHitHandle <- function(num) {
  header <- paste0("x-fn-", num)
  function(r) {
    expect_null(
      r$headers[[header]]
    )
  }
}

setsaw <- function(num) {
  name <- paste0("x-saw-", num)
  function(req, res) {
    res$headers[[name]] <- paste(req$REQUEST_METHOD, req$PATH_INFO)
  }
}

saw <- function(req, res) {
  msg <- paste("saw", req$REQUEST_METHOD, req$PATH_INFO)
  res$status <- 200L
  res$headers[["Content-Type"]] <- "text/plain"
  res$send(msg)
}

sendParams <- function(req, res) {
  res$status <- 200L
  params <- paste0(
    sprintf("%s:%s", names(req$params), req$params),
    collapse = "-"
  )

  res$send(params)
}

hitParams <- function(num) {
  name <- paste0("x-params-", num)
  function(req, res) {
    res$headers[[name]] <- yyjsonr::write_json_str(req$params)
    forward()
  }
}

sawParams <- function(req, res) {
  res$status <- 200L
  res$headers[["Content-Type"]] <- "application/json"
  res$send(yyjsonr::write_json_str(req$params))
}

raw_file_content <- function(filename) {
  size <- file.info(filename)$size
  readBin(filename, "raw", n = size)
}
