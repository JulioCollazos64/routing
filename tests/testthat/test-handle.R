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

  describe("req$baseUrl", {
    it("should contain the stripped path", {
      router <- Router$new()
      router$use("/foo", sawBase)

      server <- createServer(router)
      r <- fetch(server, "/foo/bar")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw /foo"
        )
      )
    })

    it("should contain the stripped path from multiple levels", {
      router1 <- Router$new()
      router2 <- Router$new()

      router1$use("/foo", router2)
      router2$use("/bar", sawBase)

      server <- createServer(router1)
      r <- fetch(server, "/foo/bar/baz")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "saw /foo/bar"
        )
      )
    })

    it("should be altered correctly", {
      router <- Router$new()
      sub1 <- Router$new()
      sub2 <- Router$new()
      sub3 <- Router$new()

      sub3$get("/zed", setsawBase(1))

      sub2$use("/baz", sub3)

      sub1$use("/", setsawBase(2))

      sub1$use("/bar", sub2)
      sub1$use("/bar", setsawBase(3))

      router$use(setsawBase(4))
      router$use("/foo", sub1)
      router$use(setsawBase(5))
      router$use(hello_world)

      server <- createServer(router)

      r <- fetch(server, "/foo/bar/baz/zed")

      expect_identical(r$status, 200L)
      expect_identical(r$headers$`x-saw-base1`, "/foo/bar/baz")
      expect_identical(r$headers$`x-saw-base2`, "/foo")
      expect_identical(r$headers$`x-saw-base3`, "/foo/bar")
      expect_identical(r$headers$`x-saw-base4`, "")
      expect_identical(r$headers$`x-saw-base5`, "")
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

  describe("static paths", {
    css_content <- raw_file_content(test_path("static/css/main.css"))
    html_content <- raw_file_content(test_path("static/index.html"))
    it("should serve files", {
      router <- Router$new()
      router$static(
        test_path("static"),
        "/"
      )

      server <- createServer(router)

      r <- fetch(server, "/")

      expect_identical(r$status, 200L)
      expect_identical(charToRaw(r$body), html_content)

      r <- fetch(server, "/css/main.css")

      expect_identical(r$status, 200L)
      expect_identical(charToRaw(r$body), css_content)
    })

    describe("should respect staticPath arguments", {
      it("sholuld respect indexhtml argument", {
        router <- Router$new()
        router$static(
          test_path("static"),
          "/",
          indexhtml = FALSE
        )

        server <- createServer(router)

        r <- fetch(server, "/")

        expect_identical(r$status, 404L)
        expect_identical(r$body, "404 Not Found\n")
      })

      it("should respect fallthrough argument", {
        router <- Router$new()
        router$static(
          test_path("static"),
          "/",
          fallthrough = TRUE
        )

        server <- createServer(router)

        # Should go through our finalHandler (slower, not advised)
        r <- fetch(server, "/foo")
        expect_identical(r$status, 404L)
        expect_true(grepl(pattern = "Cannot GET /foo", x = r$body))
      })

      it("should respect html_charset argument", {
        router <- Router$new()
        router$static(
          test_path("static"),
          "/",
          html_charset = ""
        )

        server <- createServer(router)

        r <- fetch(server, "/")
        expect_identical(r$status, 200L)
        expect_identical(r$headers$`content-type`, "text/html")

        # Default behaviour
        router1 <- Router$new()
        router1$static(
          test_path("static"),
          "/"
        )

        server <- createServer(router1)

        r <- fetch(server, "/")
        expect_identical(r$status, 200L)
        expect_identical(r$headers$`content-type`, "text/html; charset=utf-8")
      })

      it("should respect headers argument", {
        router <- Router$new()
        router$static(
          test_path("static"),
          "/",
          headers = list(
            "X-Powered-By" = "routing"
          )
        )

        server <- createServer(router)

        r <- fetch(server, "/")
        expect_identical(r$status, 200L)
        expect_identical(charToRaw(r$body), html_content)
        expect_identical(r$headers$`x-powered-by`, "routing")
      })
      it("should respect validation argument", {
        router <- Router$new()
        router$static(
          test_path("static"),
          "/",
          validation = c('"foo" == "bar"')
        )

        server <- createServer(router)

        r <- fetch(server, "/", headers = list(foo = "zoo"))

        expect_identical(r$status, 403L)
        expect_identical(r$body, "403 Forbidden\n")

        r <- fetch(server, "/", headers = list(foo = "bar"))

        expect_identical(r$status, 200L)
        expect_identical(charToRaw(r$body), html_content)
      })
    })
  })
})
