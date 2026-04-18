#' @title Final Handler
#'
#' @description
#' Automatically manages unhandled errors and requests (404).
#'
#' @export
finalHandler <- function(req, res, options) {
  function(err) {
    headers <- NULL

    # ignore 404 if response already sent
    if (is.null(err) && !is.null(res$body)) {
      return()
    }

    if (!is.null(err)) {
      status <- getErrorStatusCode(err)

      if (is.null(status)) {
        status <- getResponseStatusCode(res)
      } else {
        headers <- getErrorHeaders(err)
      }

      # get error message
      msg <- getErrorMessage(status)
    } else {
      # not found
      status <- 404L
      msg <- paste0(
        "Cannot ",
        req$REQUEST_METHOD,
        " ",
        req$PATH_INFO
      )
    }

    # send response
    send(req, res, status, headers, msg)
  }
}
createHtmlDocument <- function(message) {
  paste(
    c(
      "<!DOCTYPE html>",
      '<html lang="en">',
      "<head>",
      '<meta charset="utf-8">',
      "<title>Error</title>",
      "</head>",
      "<body>",
      sprintf("<pre>%s</pre>", message),
      "</body>",
      "</html>"
    ),
    collapse = "\n"
  )
}
#' @noRd
#' @keywords internal
getErrorStatusCode <- function(err) {
  status <- err$status
  if (is.numeric(status) && status >= 400 && status < 600) {
    return(as.integer(status))
  }
  NULL
}

#' @noRd
#' @keywords internal
getResponseStatusCode <- function(res) {
  status <- res$status
  if (!is.numeric(status) || status < 400 || status > 599) {
    status <- 500L
  }

  status
}

#' @noRd
#' @keywords internal
getErrorHeaders <- function(err) {
  if (!length(err$headers)) {
    return()
  }

  err$headers
}

#' @noRd
#' @keywords internal
getErrorMessage <- function(status) {
  httpcode::http_code(status)$message
}

#' @noRd
#' @keywords internal
send <- function(req, res, status, headers, message) {
  # response body
  body <- createHtmlDocument(message)
  # response status
  res$status <- status

  # remove any content headers
  res$headers[["Content-Encoding"]] <- NULL
  res$headers[["Content-Language"]] <- NULL
  res$headers[["Content-Range"]] <- NULL

  # response headers - TODO

  # security headers
  res$headers[["Content-Security-Policy"]] <- "default-src 'none'"
  res$headers[["X-Content-Type-Options"]] <- "nosniff"

  # standard headers

  res$headers[["Content-Type"]] <- 'text/html; charset=utf-8'
  res$headers[["Content-Length"]] <- nchar(body, type = "bytes")

  # send
  list(
    status = res$status,
    body = body,
    headers = res$headers
  )
}
