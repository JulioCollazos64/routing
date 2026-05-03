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


createServer <- function(handler) {
  httpuv::startServer(
    "127.0.0.1",
    httpuv::randomPort(),
    list(
      call = function(req) {
        res <- Response$new()

        if (isRouter(handler)) {
          handler$handle(req, res, callback = finalHandler(req, res))
        } else {
          handler(req, res)
        }
      }
    )
  )
}

# adapted from https://github.com/rstudio/httpuv/blob/main/tests/testthat/helper-app.R
# with assistance from Claude
fetch <- function(server, path, method = "GET") {
  url <- paste0("http://127.0.0.1:", server$getPort(), path)
  handle <- curl::new_handle(customrequest = toupper(method))
  pool <- curl::new_pool()
  result <- NULL
  error <- NULL

  curl::curl_fetch_multi(
    url,
    done = function(r) result <<- r,
    fail = function(e) error <<- e,
    handle = handle,
    pool = pool
  )

  while (is.null(result) && is.null(error)) {
    later::run_now(0)
    curl::multi_run(timeout = 0.01, pool = pool)
  }

  if (!is.null(error)) {
    stop(error)
  }

  list(
    status = result$status_code,
    body = rawToChar(result$content),
    headers = curl::parse_headers_list(result$headers)
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


hello_world <- function(req, res, forward) {
  res$status <- 200L
  res$send("hello, world")
}


create_hit_handle <- function(num) {
  name <- paste0("x-fn-", num)
  function(req, res, forward) {
    res$headers[[name]] <- "hit"
    forward()
  }
}

should_hit_handle <- function(r, num) {
  expect_equal(
    r$headers[[paste0("x-fn-", num)]],
    "hit",
    label = paste("handle", num, "should be hit")
  )
}

should_not_hit_handle <- function(r, num) {
  expect_null(
    r$headers[[paste0("x-fn-", num)]],
    label = paste("handle", num, "should not be hit")
  )
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
