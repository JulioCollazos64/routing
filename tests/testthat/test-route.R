describe("route", {
  describe("$route(path)", {
    it("should return a new route", {
      router <- Router$new()
      route <- router$route("/foo")

      expect_identical(route$path, "/foo")
    })

    it("should respond to multiple methods", {
      router <- Router$new()
      route <- router$route("/foo")
      route$get(saw)
      route$post(saw)

      server <- createServer(router)

      request <- mochita(server)

      request$get("/foo")$expect(
        200L,
        "saw GET /foo"
      )$perform()

      request$post("/foo")$expect(
        200L,
        "saw POST /foo"
      )$perform()

      request$put("/foo")$expect(
        404L
      )$perform()
    })

    it("should route without method", {
      router <- Router$new()
      route <- router$route("/foo")
      route$post(createHitHandle(1))
      route$all(createHitHandle(2))
      route$get(createHitHandle(3))

      router$get("/foo", createHitHandle(4))
      router$use(saw)

      server <- createServer(
        function(req, res, forward) {
          req$REQUEST_METHOD <- NULL
          router$handle(req, res, forward)
        }
      )

      mochita(server)$get("/foo")$expect(
        shouldNotHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(
        shouldNotHitHandle(3)
      )$expect(
        shouldNotHitHandle(4)
      )$expect(
        200L,
        "saw  /foo"
      )$perform()
    })

    it("should stack", {
      router <- Router$new()
      route <- router$route("/foo")
      route$post(createHitHandle(1))
      route$all(createHitHandle(2))
      route$get(createHitHandle(3))

      router$use(saw)

      server <- createServer(router)

      mochita(server)$get("/foo")$expect(
        shouldHitHandle(2)
      )$expect(
        shouldHitHandle(3)
      )$expect(
        200L,
        "saw GET /foo"
      )$perform()

      mochita(server)$post("/foo")$expect(
        shouldHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(
        200L,
        "saw POST /foo"
      )$perform()

      mochita(server)$put("/foo")$expect(
        shouldHitHandle(2)
      )$expect(
        200L,
        "saw PUT /foo"
      )$perform()
    })

    it("should not error on  route", {
      router <- Router$new()
      route <- router$route("/foo")

      server <- createServer(router)

      mochita(server)$get("/foo")$expect(404L)$perform()

      mochita(server)$post("/foo")$expect(404L)$perform()
    })

    it("should not invoke singular error route", {
      router <- Router$new()
      route <- router$route("/foo")

      route$all(\(err, req, res) {
        conditionMessage(err)
      })

      server <- createServer(router)

      mochita(server)$get("/foo")$expect(404L)$perform()
    })

    it("should call forward() if handler empty", {
      router <- Router$new()
      route <- router$route("/")
      route$get(\(req, res) {
        "All good"
      })

      server <- createServer(router)
      mochita(server)$get("/")$expect(
        404L
      )$expect(
        function(req) {
          expect_true(
            grepl("Cannot GET /", req$body)
          )
        }
      )$perform()
    })
  })

  describe("$all(..fn)", {
    it("should reject no arguments", {
      router <- Router$new()
      route <- router$route("/")

      expect_error(
        route$all(),
        "Argument handler is required"
      )
    })

    # it("should reject  list", {
    #   router <- Router$new()
    #   route <- router$route("/")

    #   route$all(list())
    # })

    it("should reject invalid fn", {
      router <- Router$new()
      route <- router$route("/")
      expect_error(
        route$all(2),
        "handler must be a function"
      )
    })

    it("should respond to all methods", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(saw)

      server <- createServer(router)

      mochita(server)$get("/foo")$expect(
        200L,
        "saw GET /foo"
      )$perform()

      mochita(server)$post("/foo")$expect(
        200L,
        "saw POST /foo"
      )$perform()

      mochita(server)$put("/foo")$expect(
        200L,
        "saw PUT /foo"
      )$perform()
    })

    it("should accept multiple arguments", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(
        createHitHandle(1),
        createHitHandle(2),
        helloWorld
      )

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        shouldHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(
        200L,
        "hello, world"
      )$perform()
    })

    it("should accept single list of handlers", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(
        list(
          createHitHandle(1),
          createHitHandle(2),
          helloWorld
        )
      )

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        shouldHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(
        200L,
        "hello, world"
      )$perform()
    })

    it("should accept nested lists of handlers", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(
        list(
          list(
            createHitHandle(1),
            createHitHandle(2)
          ),
          createHitHandle(3)
        ),
        helloWorld
      )

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        shouldHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(
        shouldHitHandle(3)
      )$expect(
        200L,
        "hello, world"
      )$perform()
    })
  })

  describe("error handling", {
    it("should handle errors from forward(err)", {
      router <- Router$new()
      route <- router$route("/foo")

      route$all(\(req, res) {
        forward(stop("Boom!"))
      })
      route$all(helloWorld)
      route$all(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(msg)
      })

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        500L,
        "Boom!"
      )$perform()
    })

    it("should handle errors thrown", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(\(req, res) {
        stop("boom!")
      })
      route$all(helloWorld)
      route$all(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(msg)
      })

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        500L,
        "boom!"
      )$perform()
    })

    it("should handle errors thrown in error handlers", {
      router <- Router$new()
      route <- router$route("/foo")

      route$all(\(req, res) {
        stop("boom!")
      })
      route$all(\(err, req, res) {
        stop("ouch: ", conditionMessage(err))
      })
      route$all(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(paste("caught:", msg))
      })

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        500L,
        "caught: ouch: boom!"
      )$perform()
    })

    it("should call forward(err) when the error handler is empty", {
      router <- Router$new()
      router$get(
        "/",
        \(req, res) {
          stop("Oh no!")
        },
        \(err, req, res) {
          conditionMessage(err)
        }
      )

      server <- createServer(router)
      mochita(server)$get("/")$expect(500L)$expect(
        function(res) {
          expect_true(grepl("Internal Server Error", res$body))
        }
      )$perform()
    })
  })

  describe('forward("route")', {
    it("should invoke next handler", {
      router <- Router$new()
      route <- router$route("/foo")
      route$get(\(req, res) {
        res$headers[["x-next"]] <- "route"
        forward("route")
      })
      router$use(saw)

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        "x-next",
        "route"
      )$expect(
        200L,
        "saw GET /foo"
      )$perform()
    })

    it("should invoke next route", {
      router <- Router$new()
      route <- router$route("/foo")
      route$get(\(req, res) {
        res$headers[["x-next"]] <- "route"
        forward("route")
      })
      router$route("/foo")$all(saw)

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        "x-next",
        "route"
      )$expect(
        200L,
        "saw GET /foo"
      )$perform()
    })

    it("should skip next handlers in route", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(createHitHandle(1))
      route$get(\(req, res) {
        res$headers[["x-next"]] <- "route"
        forward("route")
      })
      route$all(createHitHandle(2))
      router$use(saw)

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        shouldHitHandle(1)
      )$expect(
        "x-next",
        "route"
      )$expect(
        shouldNotHitHandle(2)
      )$expect(
        200L,
        "saw GET /foo"
      )$perform()
    })

    it("should not invoke error handlers", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(\(req, res) {
        res$headers[["x-next"]] <- "route"
        forward("route")
      })
      route$all(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(paste("caught:", msg))
      })

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        "x-next",
        "route"
      )$expect(404L)$perform()
    })
  })

  describe('forward("router")', {
    it("should exit the router", {
      router <- Router$new()
      route <- router$route("/foo")

      route$get(
        \(req, res) {
          res$headers[["x-next"]] <- "router"
          forward("router")
        },
        createHitHandle(1)
      )

      router$use(saw)

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        "x-next",
        "router"
      )$expect(
        shouldNotHitHandle(1)
      )$expect(
        404L
      )$perform()
    })

    it("should not invoke error handlers", {
      router <- Router$new()
      route <- router$route("/foo")

      route$all(\(req, res) {
        res$headers[["x-next"]] <- "router"
        forward("router")
      })

      route$all(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(paste("caught:", msg))
      })

      router$use(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(paste("caught:", msg))
      })

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        "x-next",
        "router"
      )$expect(404L)$perform()
    })
  })

  describe("path", {
    describe("usign :name", {
      it("should name a capture group", {
        router <- Router$new()
        route <- router$route("/:foo")
        route$all(sendParams)

        server <- createServer(router)
        mochita(server)$get("/bar")$expect(
          200L,
          '{"foo":"bar"}'
        )$perform()
      })

      it("should match single path segment", {
        router <- Router$new()
        route <- router$route("/:foo")
        route$all(sendParams)

        server <- createServer(router)
        mochita(server)$get("/bar/bar")$expect(404L)$perform()
      })

      it("should work multiple times", {
        router <- Router$new()
        route <- router$route("/:foo/:bar")
        route$all(sendParams)

        server <- createServer(router)
        mochita(server)$get("/fizz/buzz")$expect(
          200L,
          '{"foo":"fizz","bar":"buzz"}'
        )$perform()
      })

      it("should work inside literal parentheses", {
        router <- Router$new()
        route <- router$route("/:user\\(:opp\\)")
        route$all(sendParams)

        server <- createServer(router)
        mochita(server)$get("/tj(edit)")$expect(
          200L,
          '{"user":"tj","opp":"edit"}'
        )$perform()
      })

      it("should work with a path vector of length > 1", {
        router <- Router$new()
        route <- router$route(c("/user/:user/poke", "/user/:user/pokes"))
        route$all(sendParams)

        server <- createServer(router)

        mochita(server)$get("/user/tj/poke")$expect(
          200L,
          '{"user":"tj"}'
        )$perform()

        mochita(server)$get("/user/tj/pokes")$expect(
          200L,
          '{"user":"tj"}'
        )$perform()
      })
    })

    describe('using "{:name}"', {
      it("should name an optional parameter", {
        router <- Router$new()
        route <- router$route("{/:foo}")
        route$all(sendParams)

        server <- createServer(router)

        mochita(server)$get("/bar")$expect(
          200L,
          '{"foo":"bar"}'
        )$perform()

        mochita(server)$get("/")$expect(
          200L,
          "{}"
        )$perform()
      })

      it("should work in any segment", {
        router <- Router$new()
        route <- router$route("/user{/:foo}/delete")
        route$all(sendParams)

        server <- createServer(router)

        mochita(server)$get("/user/bar/delete")$expect(
          200L,
          '{"foo":"bar"}'
        )$perform()

        mochita(server)$get("/user/delete")$expect(
          200L,
          '{}'
        )$perform()
      })
    })
  })

  describe('using "*name"', {
    it("should name a zero-or-more repeated parameter", {
      router <- Router$new()
      route <- router$route("{/*foo}")
      route$all(sendParams)

      server <- createServer(router)

      mochita(server)$get("/")$expect(
        200L,
        '{}'
      )$perform()

      mochita(server)$get("/bar")$expect(
        200L,
        '{"foo":"bar"}'
      )$perform()

      mochita(server)$get("/fizz/buzz")$expect(
        200L,
        '{"foo":["fizz","buzz"]}'
      )$perform()
    })

    it("should work in any segment", {
      router <- Router$new()
      route <- router$route("/user{/*foo}/delete")
      route$all(sendParams)

      server <- createServer(router)

      mochita(server)$get("/user/delete")$expect(
        200L,
        '{}'
      )$perform()

      mochita(server)$get("/user/bar/delete")$expect(
        200L,
        '{"foo":"bar"}'
      )$perform()

      mochita(server)$get("/user/fizz/buzz/delete")$expect(
        200L,
        '{"foo":["fizz","buzz"]}'
      )$perform()
    })
  })
})
