describe("req$params", {
  on.exit(
    {
      httpuv::stopAllServers()
    },
    add = TRUE
  )

  it("should default to empty object", {
    router <- Router$new()
    router$get("/", sawParams)

    server <- createServer(router)

    r <- fetch(server, "/")

    expect_identical(r$body, "[]")
  })

  it("should not exist outside the router", {
    router <- Router$new()
    router$get("/", hitParams(1))

    server <- createServer(function(req, res) {
      router$handle(req, res, function(err = NULL) {
        if (!is.null(err)) {
          return(finalHandler(req, res)(err))
        }
        sawParams(req, res)
      })
    })

    r <- fetch(server, "/")
    expect_identical(r$headers[["x-params-1"]], "[]")
  })

  it("should overwrite value outside the router", {
    router <- Router$new()
    router$get("/", sawParams)

    server <- createServer(
      function(req, res) {
        req$params <- list(foo = "bar")
        router$handle(req, res)
      }
    )

    r <- fetch(server, "/")
    expect_identical(r$body, "[]")
  })

  it("should restore previous value outside the router", {
    router <- Router$new()
    router$get("/", hitParams(1))

    server <- createServer(
      function(req, res) {
        req$params <- list(foo = "bar")
        router$handle(req, res, function(err) {
          if (!is.null(err)) {
            return(forward(err))
          }

          sawParams(req, res)
        })
      }
    )

    r <- fetch(server, "/")
    expect_identical(r$headers$`x-params-1`, "[]")
    expect_identical(
      list(
        r$status,
        r$body
      ),
      list(
        200L,
        '{"foo":["bar"]}'
      )
    )
  })

  describe('when "mergeParams: true"', {
    it("should merge outside object with params", {
      router <- Router$new(mergeParams = TRUE)
      router$get("/:fizz", hitParams(1))

      server <- createServer(\(req, res, forward) {
        req$params <- list(foo = "bar")
        router$handle(req, res, function(err) {
          if (!is.null(err)) {
            return(forward(err))
          }
          sawParams(req, res)
        })
      })

      r <- fetch(server, "/buzz")

      expect_identical(
        r$headers$`x-params-1`,
        '{"foo":["bar"],"fizz":["buzz"]}'
      )
    })

    it("should ignore non-list outside env", {
      router <- Router$new(mergeParams = TRUE)
      router$get("/:fizz", hitParams(1))

      server <- createServer(\(req, res, forward) {
        req$params <- 6

        router$handle(req, res, function(err) {
          if (!is.null(err)) {
            return(forward(err))
          }
          sawParams(req, res)
        })
      })

      r <- fetch(server, "/buzz")

      expect_identical(
        r$headers$`x-params-1`,
        '{"fizz":["buzz"]}'
      )

      expect_identical(
        r$body,
        "[6.0]"
      )
    })

    it("should overwrite outside keys that are the same", {
      router <- Router$new(mergeParams = TRUE)
      router$get("/:foo", hitParams(1))

      server <- createServer(function(req, res, forward) {
        req$params <- list(foo = "bar")
        router$handle(req, res, callback = function(err) {
          if (!is.null(err)) {
            return(forward(err))
          }
          sawParams(req, res)
        })
      })

      r <- fetch(server, "/buzz")
      expect_identical(r$headers$`x-params-1`, '{"foo":["buzz"]}')
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          '{"foo":["bar"]}'
        )
      )
    })
  })
})
