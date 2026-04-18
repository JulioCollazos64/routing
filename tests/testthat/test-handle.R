describe("Router", {
  on.exit(
    {
      httpuv::stopAllServers()
    },
    add = TRUE
  )
  describe("$all(path,fn)", {
    describe('with "caseSensitive" option', {
      it("should not match path case-sensitive by default", {
        router <- Router$new()
        router$all("/foo/bar", saw)
        server <- createServer(router)

        r <- fetch(server, path = "/foo/bar")

        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /foo/bar"
          )
        )

        r <- fetch(server, "/FOO/bar")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /FOO/bar"
          )
        )

        r <- fetch(server, "/FOO/BAR")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /FOO/BAR"
          )
        )
      })

      it("should not match paths case-sensitively when false", {
        router <- Router$new(caseSensitive = FALSE)
        router$all("/foo/bar", saw)
        server <- createServer(router)

        r <- fetch(server, path = "/foo/bar")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /foo/bar"
          )
        )

        r <- fetch(server, "/FOO/bar")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /FOO/bar"
          )
        )

        r <- fetch(server, "/FOO/BAR")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /FOO/BAR"
          )
        )
      })

      it("should match path case-sensitively when true", {
        router <- Router$new(caseSensitive = TRUE)
        router$all("/foo/bar", saw)
        server <- createServer(router)

        r <- fetch(server, path = "/foo/bar")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /foo/bar"
          )
        )

        r <- fetch(server, "/FOO/bar")
        expect_identical(r$status, 404L)

        r <- fetch(server, "/FOO/BAR")
        expect_identical(r$status, 404L)
      })
    })

    describe('with "strict" option', {
      it("should accept optional trailing slashes by default", {
        router <- Router$new()
        router$all("/foo", saw)
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

        r <- fetch(server, "/foo/")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /foo/"
          )
        )
      })

      it("should accept optional trailing slashes when false", {
        router <- Router$new(strict = FALSE)
        router$all("/foo", saw)
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

        r <- fetch(server, "/foo/")
        expect_identical(
          list(
            r$status,
            r$body
          ),
          list(
            200L,
            "saw GET /foo/"
          )
        )
      })

      it("should not accept optional trailing slashes when true", {
        router <- Router$new(strict = TRUE)
        router$all("/foo", saw)
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

        r <- fetch(server, "/foo/")
        expect_identical(r$status, 404L)
      })
    })
  })

  describe("$use(..fn)", {
    it("should reject empty list", {
      router <- Router$new()

      expect_error(
        router$use(list()),
        "argument handler is required"
      )
    })

    it("should reject non-functions", {
      router <- Router$new()

      expect_error(
        router$use("/", "/hello"),
        "handler must be a function"
      )

      expect_error(
        router$use("/", 5),
        "handler must be a function"
      )
    })

    it("should invoke function for all requests", {
      router <- Router$new()
      router$use(saw)
      server <- createServer(router)

      r <- fetch(server, "/")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /"
        )
      )

      r <- fetch(server, "/", method = "PUT")
      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw PUT /"
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
    })

    it("should support another router", {
      inner <- Router$new()
      router <- Router$new()

      inner$use(saw)
      router$use(inner)

      server <- createServer(router)

      r <- fetch(server, "/")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /"
        )
      )
    })

    it("should accept multiple arguments", {
      router <- Router$new()
      router$use(create_hit_handle(1), create_hit_handle(2), hello_world)

      server <- createServer(router)

      r <- fetch(server, "/")

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

    it("should accept single array of middleware", {
      router <- Router$new()
      router$use(
        list(
          create_hit_handle(1),
          create_hit_handle(2),
          hello_world
        )
      )

      server <- createServer(router)

      r <- fetch(server, "/")

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

    it("should accept nested arrays of middleware", {
      router <- Router$new()
      router$use(
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

      r <- fetch(server, "/")

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

    it("should not invoke singular error function", {
      router <- Router$new()
      router$use(
        function(req, res, forward, err) {
          stop("boom!")
        }
      )

      server <- createServer(router)

      r <- fetch(server, "/")

      expect_identical(r$status, 404L)
    })
  })

  describe("req$PATH_INFO", {
    it("should strip path from req$PATH_INFO", {
      router <- Router$new()
      router$use("/foo", saw)

      server <- createServer(router)
      r <- fetch(server, "/foo/bar")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /bar"
        )
      )
    })

    it("should restore req$PATH_INFO after stripping", {
      router <- Router$new()
      router$use("/foo", setsaw(1))
      router$use(saw)

      server <- createServer(router)
      r <- fetch(server, "/foo/bar")

      expect_identical(
        r$headers$`x-saw-1`,
        "GET /bar"
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo/bar"
        )
      )
    })

    it("should strip/restore with trailing slash", {
      router <- Router$new()
      router$use("/foo", setsaw(1))
      router$use(saw)

      server <- createServer(router)
      r <- fetch(server, "/foo/")

      expect_identical(
        r$headers$`x-saw-1`,
        "GET /"
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw GET /foo/"
        )
      )
    })
  })

  describe("request rewriting", {
    it("should support altering req$REQUEST_METHOD", {
      router <- Router$new()
      router$put("/foo", create_hit_handle(1))
      router$post("/foo", create_hit_handle(2), \(req, res) {
        req$REQUEST_METHOD <- "PUT"
      })
      router$post("/foo", create_hit_handle(3))
      router$put("/foo", create_hit_handle(4))
      router$use(saw)

      server <- createServer(router)
      r <- fetch(server, "/foo", method = "POST")

      should_not_hit_handle(r, 1)
      should_hit_handle(r, 2)
      should_not_hit_handle(r, 3)
      should_hit_handle(r, 4)

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

    it("should support altering req$PATH_INFO", {
      router <- Router$new()
      router$get("/bar", create_hit_handle(1))
      router$get("/foo", create_hit_handle(2), \(req, res) {
        req$PATH_INFO <- "/bar"
      })

      router$get("/foo", create_hit_handle(3))
      router$get("/bar", create_hit_handle(4))
      router$use(saw)

      server <- createServer(router)
      r <- fetch(server, "/foo")

      should_not_hit_handle(r, 1)
      should_hit_handle(r, 2)
      should_not_hit_handle(r, 3)
      should_hit_handle(r, 4)
      expect_equal(r$status, 200L)
      expect_equal(r$body, "saw GET /bar")
    })
  })
})
