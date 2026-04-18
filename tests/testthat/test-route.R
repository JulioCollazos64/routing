describe("route", {
  on.exit(
    {
      httpuv::stopAllServers()
    },
    add = TRUE
  )
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

      r <- fetch(server, "/foo")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo"
        )
      )

      r <- fetch(server, "/foo", method = "POST")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw POST /foo"
        )
      )

      r <- fetch(server, "/foo", method = "PUT")
      expect_identical(r$status, 404L)
    })

    # it("should route without method", {
    #   router <- Router$new()
    #   route <- router$route("/foo")
    #   route$post(create_hit_handle(1))
    #   route$all(create_hit_handle(2))
    #   route$get(create_hit_handle(3))

    #   router$get("/foo", create_hit_handle(4))
    #   router$use(saw)

    #   server <- createServer(router)

    #   r <- fetch(server, "/foo")
    #   r
    # })

    it("should stack", {
      router <- Router$new()
      route <- router$route("/foo")
      route$post(create_hit_handle(1))
      route$all(create_hit_handle(2))
      route$get(create_hit_handle(3))

      router$use(saw)

      server <- createServer(router)

      r <- fetch(server, "/foo")

      should_hit_handle(r, 2)
      should_hit_handle(r, 3)
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo"
        )
      )

      r <- fetch(server, "/foo", method = "POST")
      should_hit_handle(r, 1)
      should_hit_handle(r, 2)
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw POST /foo"
        )
      )

      r <- fetch(server, "/foo", method = "PUT")
      should_hit_handle(r, 2)
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw PUT /foo"
        )
      )
    })

    it("should not error on  route", {
      router <- Router$new()
      route <- router$route("/foo")

      server <- createServer(router)

      r <- fetch(server, "/foo")
      expect_identical(r$status, 404L)

      r <- fetch(server, "/foo", method = "POST")
      expect_identical(r$status, 404L)
    })

    it("should not invoke singular error route", {
      router <- Router$new()
      route <- router$route("/foo")

      route$all(\(err, req, res) {
        conditionMessage(err)
      })

      server <- createServer(router)

      r <- fetch(server, "/foo")
      expect_identical(r$status, 404L)
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

      r <- fetch(server, "/foo")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo"
        )
      )

      r <- fetch(server, "/foo", method = "POST")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw POST /foo"
        )
      )

      r <- fetch(server, "/foo", method = "PUT")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw PUT /foo"
        )
      )
    })

    it("should accept multiple arguments", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(
        create_hit_handle(1),
        create_hit_handle(2),
        hello_world
      )

      server <- createServer(router)
      r <- fetch(server, "/foo")

      should_hit_handle(r, 1)
      should_hit_handle(r, 2)
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "hello, world"
        )
      )
    })

    it("should accept single list of handlers", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(
        list(
          create_hit_handle(1),
          create_hit_handle(2),
          hello_world
        )
      )

      server <- createServer(router)
      r <- fetch(server, "/foo")

      should_hit_handle(r, 1)
      should_hit_handle(r, 2)
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "hello, world"
        )
      )
    })

    it("should accept nested lists of handlers", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(
        list(
          list(
            create_hit_handle(1),
            create_hit_handle(2)
          ),
          create_hit_handle(3)
        ),
        hello_world
      )

      server <- createServer(router)
      r <- fetch(server, "/foo")

      should_hit_handle(r, 1)
      should_hit_handle(r, 2)
      should_hit_handle(r, 3)

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "hello, world"
        )
      )
    })
  })

  describe("error handling", {
    it("should handle errors from forward(err)", {
      router <- Router$new()
      route <- router$route("/foo")

      route$all(\(req, res) {
        forward(stop("Boom!"))
      })
      route$all(hello_world)
      route$all(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(msg)
      })

      server <- createServer(router)

      r <- fetch(server, "/foo")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          500L,
          "Boom!"
        )
      )
    })

    it("should handle errors thrown", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(\(req, res) {
        stop("boom!")
      })
      route$all(hello_world)
      route$all(\(err, req, res) {
        res$status <- 500L
        msg <- conditionMessage(err)
        res$send(msg)
      })

      server <- createServer(router)
      r <- fetch(server, "/foo")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          500L,
          "boom!"
        )
      )
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
      r <- fetch(server, "/foo")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          500L,
          "caught: ouch: boom!"
        )
      )
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
      r <- fetch(server, "/foo")

      expect_identical(
        r$headers$`x-next`,
        "route"
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo"
        )
      )
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
      r <- fetch(server, "/foo")

      expect_identical(
        r$headers$`x-next`,
        "route"
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo"
        )
      )
    })

    it("should skip next handlers in route", {
      router <- Router$new()
      route <- router$route("/foo")
      route$all(create_hit_handle(1))
      route$get(\(req, res) {
        res$headers[["x-next"]] <- "route"
        forward("route")
      })
      route$all(create_hit_handle(2))
      router$use(saw)

      server <- createServer(router)
      r <- fetch(server, "/foo")

      should_hit_handle(r, 1)
      expect_identical(
        r$headers$`x-next`,
        "route"
      )
      should_not_hit_handle(r, 2)
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo"
        )
      )
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
      r <- fetch(server, "/foo")

      expect_identical(
        r$headers$`x-next`,
        "route"
      )
      expect_identical(r$status, 404L)
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
        create_hit_handle(1)
      )

      router$use(saw)

      server <- createServer(router)
      r <- fetch(server, "/foo")

      expect_identical(
        r$headers$`x-next`,
        "router"
      )
      should_not_hit_handle(r, 1)
      expect_identical(r$status, 404L)
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
      r <- fetch(server, "/foo")

      expect_identical(
        r$headers$`x-next`,
        "router"
      )

      expect_identical(r$status, 404L)
    })
  })

  describe("path", {
    describe("usign :name", {
      it("should name a capture group", {
        router <- Router$new()
        route <- router$route("/:foo")
        route$all(sendParams)

        server <- createServer(router)
        r <- fetch(server, "/bar")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "foo:bar"
          )
        )
      })

      it("should match single path segment", {
        router <- Router$new()
        route <- router$route("/:foo")
        route$all(sendParams)

        server <- createServer(router)
        r <- fetch(server, "/bar/bar")

        expect_identical(r$status, 404L)
      })

      it("should work multiple times", {
        router <- Router$new()
        route <- router$route("/:foo/:bar")
        route$all(sendParams)

        server <- createServer(router)
        r <- fetch(server, "/fizz/buzz")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "foo:fizz-bar:buzz"
          )
        )
      })

      it("should work inside literal parentheses", {
        router <- Router$new()
        route <- router$route("/:user\\(:opp\\)")
        route$all(sendParams)

        server <- createServer(router)
        r <- fetch(server, "/tj(edit)")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "user:tj-opp:edit"
          )
        )
      })

      it("should work with a path vector of length > 1", {
        router <- Router$new()
        route <- router$route(c("/user/:user/poke", "/user/:user/pokes"))
        route$all(sendParams)

        server <- createServer(router)

        r <- fetch(server, "/user/tj/poke")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "user:tj"
          )
        )

        r <- fetch(server, "/user/tj/pokes")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "user:tj"
          )
        )
      })
    })

    describe('using "{:name}"', {
      it("should name an optional parameter", {
        router <- Router$new()
        route <- router$route("{/:foo}")
        route$all(sendParams)

        server <- createServer(router)
        r <- fetch(server, "/bar")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "foo:bar"
          )
        )

        r <- fetch(server, "/")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            ""
          )
        )
      })

      it("should work in any segment", {
        router <- Router$new()
        route <- router$route("/user{/:foo}/delete")
        route$all(sendParams)

        server <- createServer(router)

        r <- fetch(server, "/user/bar/delete")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "foo:bar"
          )
        )

        r <- fetch(server, "/user/delete")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            ""
          )
        )
      })
    })
  })

  describe('using "*name"', {
    it("should name a zero-or-more repeated parameter", {
      router <- Router$new()
      route <- router$route("{/*foo}")
      route$all(sendParams)

      server <- createServer(router)

      r <- fetch(server, "/")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          ""
        )
      )

      r <- fetch(server, "/bar")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "foo:bar"
        )
      )

      r <- fetch(server, "/fizz/buzz")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          'foo:c("fizz", "buzz")'
        )
      )
    })

    it("should work in any segment", {
      router <- Router$new()
      route <- router$route("/user{/*foo}/delete")
      route$all(sendParams)

      server <- createServer(router)

      r <- fetch(server, "/user/delete")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          ""
        )
      )

      r <- fetch(server, "/user/bar/delete")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "foo:bar"
        )
      )

      r <- fetch(server, "/user/fizz/buzz/delete")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          'foo:c("fizz", "buzz")'
        )
      )
    })
  })
})
