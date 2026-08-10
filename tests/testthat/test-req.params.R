describe("req$params", {
  it("should default to empty object", {
    router <- Router$new()
    router$get("/", sawParams)

    server <- createServer(router)
    mochita(server)$get("/")$expect(
      "{}"
    )$perform()
  })

  it("should not exist outside the router", {
    router <- Router$new()
    router$get("/", hitParams(1))

    server <- createServer(function(req, res, forward) {
      router$handle(req, res, function(err) {
        if (!is.null(err)) {
          return(forward(err))
        }
        sawParams(req, res)
      })
    })

    mochita(server)$get("/")$expect(
      "x-params-1",
      "{}"
    )$perform()
  })

  it("should overwrite value outside the router", {
    router <- Router$new()
    router$get("/", sawParams)

    server <- createServer(
      function(req, res, forward) {
        req$params <- list(foo = "bar")
        router$handle(req, res)
      }
    )

    mochita(server)$get("/")$expect(200L, "{}")$perform()
  })

  it("should restore previous value outside the router", {
    router <- Router$new()
    router$get("/", hitParams(1))

    server <- createServer(
      function(req, res, forward) {
        req$params <- list(foo = "bar")
        router$handle(req, res, function(err) {
          if (!is.null(err)) {
            return(forward(err))
          }

          sawParams(req, res)
        })
      }
    )
    mochita(server)$get("/")$expect(
      "x-params-1",
      "{}"
    )$expect(
      200L,
      '{"foo":"bar"}'
    )$perform()
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

      mochita(server)$get("/buzz")$expect(
        "x-params-1",
        '{"foo":"bar","fizz":"buzz"}'
      )$expect(
        200L,
        '{"foo":"bar"}'
      )$perform()
    })

    it("should ignore non-list outside env", {
      router <- Router$new(mergeParams = TRUE)
      router$get("/:fizz", hitParams(1))

      server <- createServer(\(req, res, forward) {
        req$params <- 42

        router$handle(req, res, function(err) {
          if (!is.null(err)) {
            return(forward(err))
          }
          sawParams(req, res)
        })
      })

      mochita(server)$get("/buzz")$expect(
        "x-params-1",
        '{"fizz":"buzz"}'
      )$expect(
        200L,
        '42'
      )$perform()
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

      mochita(server)$get("/buzz")$expect(
        "x-params-1",
        '{"foo":"buzz"}'
      )$expect(
        200L,
        '{"foo":"bar"}'
      )$perform()
    })
  })
})
