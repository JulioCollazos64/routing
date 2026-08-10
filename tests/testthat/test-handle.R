describe("Router", {
  describe("$all(path,fn)", {
    describe('with "caseSensitive" option', {
      it("should not match path case-sensitive by default", {
        router <- Router$new()
        router$all("/foo/bar", saw)
        server <- createServer(router)
        request <- mochita(server)

        paths <- list(
          "/foo/bar",
          "/FOO/bar",
          "/FOO/BAR"
        )

        for (path in paths) {
          request$get(path)$expect(200L, paste("saw GET", path))$perform()
        }
      })

      it("should not match paths case-sensitively when false", {
        router <- Router$new(caseSensitive = FALSE)
        router$all("/foo/bar", saw)
        server <- createServer(router)
        request <- mochita(server)

        paths <- list(
          "/foo/bar",
          "/FOO/bar",
          "/FOO/BAR"
        )

        for (path in paths) {
          request$get(path)$expect(200L, paste("saw GET", path))$perform()
        }
      })

      it("should match path case-sensitively when true", {
        router <- Router$new(caseSensitive = TRUE)
        router$all("/foo/bar", saw)
        server <- createServer(router)
        request <- mochita(server)

        request$get("/foo/bar")$expect(200L, "saw GET /foo/bar")$perform()

        request$get("/FOO/bar")$expect(404L)$perform()

        request$get("/FOO/BAR")$expect(404L)$perform()
      })
    })

    describe('with "strict" option', {
      it("should accept optional trailing slashes by default", {
        router <- Router$new()
        router$all("/foo", saw)
        server <- createServer(router)
        request <- mochita(server)

        request$get("/foo")$expect(200L, "saw GET /foo")$perform()

        request$get("/foo/")$expect(200L, "saw GET /foo/")$perform()
      })

      it("should accept optional trailing slashes when false", {
        router <- Router$new(strict = FALSE)
        router$all("/foo", saw)
        server <- createServer(router)
        request <- mochita(server)

        request$get("/foo")$expect(200L, "saw GET /foo")$perform()

        request$get("/foo/")$expect(200L, "saw GET /foo/")$perform()
      })

      it("should not accept optional trailing slashes when true", {
        router <- Router$new(strict = TRUE)
        router$all("/foo", saw)
        server <- createServer(router)
        request <- mochita(server)

        request$get("/foo")$expect(200L, "saw GET /foo")$perform()

        request$get("/foo/")$expect(404L)$perform()
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
      request <- mochita(server)

      request$get("/")$expect(200L, "saw GET /")$perform()

      request$put("/")$expect(200L, "saw PUT /")$perform()

      request$post("/")$expect(200L, "saw POST /")$perform()
    })

    it("should support another router", {
      inner <- Router$new()
      router <- Router$new()

      inner$use(saw)
      router$use(inner)

      server <- createServer(router)
      request <- mochita(server)

      request$get("/")$expect(200L, "saw GET /")$perform()
    })

    it("should accept multiple arguments", {
      router <- Router$new()
      router$use(createHitHandle(1), createHitHandle(2), helloWorld)

      server <- createServer(router)
      request <- mochita(server)

      request$get("/")$expect(
        shouldHitHandle(1)
      )$expect(
        shouldHitHandle(
          2
        )
      )$expect(200L, "hello, world")$perform()
    })

    it("should accept single list of middleware", {
      router <- Router$new()
      router$use(
        list(
          createHitHandle(1),
          createHitHandle(2),
          helloWorld
        )
      )

      server <- createServer(router)
      request <- mochita(server)

      request$get("/")$expect(
        shouldHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(200L, "hello, world")$perform()
    })

    it("should accept nested list of middleware", {
      router <- Router$new()
      router$use(
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
      request <- mochita(server)

      request$get("/")$expect(
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

    it("should not invoke singular error function", {
      router <- Router$new()
      router$use(
        function(req, res, forward, err) {
          stop("boom!")
        }
      )

      server <- createServer(router)
      request <- mochita(server)

      request$get("/")$expect(404L)$perform()
    })
  })

  describe("req$baseUrl", {
    it("should contain the stripped path", {
      router <- Router$new()
      router$use("/foo", sawBase)

      server <- createServer(router)
      request <- mochita(server)

      request$get("/foo/bar")$expect(200L, "saw /foo")$perform()
    })

    it("should contain the stripped path from multiple levels", {
      router1 <- Router$new()
      router2 <- Router$new()

      router1$use("/foo", router2)
      router2$use("/bar", sawBase)

      server <- createServer(router1)
      request <- mochita(server)

      request$get("/foo/bar/baz")$expect(200L, "saw /foo/bar")$perform()
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
      router$use(helloWorld)

      server <- createServer(router)
      mochita(server)$get(
        "/foo/bar/baz/zed"
      )$expect(
        "x-saw-base1",
        "/foo/bar/baz"
      )$expect(
        "x-saw-base2",
        "/foo"
      )$expect(
        "x-saw-base3",
        "/foo/bar"
      )$expect(
        "x-saw-base4",
        ""
      )$expect(
        "x-saw-base5",
        ""
      )$perform()
    })
  })

  describe("req$PATH_INFO", {
    it("should strip path from req$PATH_INFO", {
      router <- Router$new()
      router$use("/foo", saw)

      server <- createServer(router)
      mochita(server)$get("/foo/bar")$expect(
        200L,
        "saw GET /bar"
      )$perform()
    })

    it("should restore req$PATH_INFO after stripping", {
      router <- Router$new()
      router$use("/foo", setsaw(1))
      router$use(saw)

      server <- createServer(router)
      mochita(server)$get("/foo/bar")$expect(
        "x-saw-1",
        "GET /bar"
      )$expect(
        200L,
        "saw GET /foo/bar"
      )$perform()
    })

    it("should strip/restore with trailing slash", {
      router <- Router$new()
      router$use("/foo", setsaw(1))
      router$use(saw)

      server <- createServer(router)
      mochita(server)$get("/foo/")$expect(
        "x-saw-1",
        "GET /"
      )$expect(
        200L,
        "saw GET /foo/"
      )$perform()
    })
  })

  describe("request rewriting", {
    it("should support altering req$REQUEST_METHOD", {
      router <- Router$new()
      router$put("/foo", createHitHandle(1))
      router$post("/foo", createHitHandle(2), \(req, res) {
        req$REQUEST_METHOD <- "PUT"
      })
      router$post("/foo", createHitHandle(3))
      router$put("/foo", createHitHandle(4))
      router$use(saw)

      server <- createServer(router)
      mochita(server)$post("/foo")$expect(
        shouldNotHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(
        shouldNotHitHandle(3)
      )$expect(
        shouldHitHandle(4)
      )$expect(
        200L,
        "saw PUT /foo"
      )$perform()
    })

    it("should support altering req$PATH_INFO", {
      router <- Router$new()
      router$get("/bar", createHitHandle(1))
      router$get("/foo", createHitHandle(2), \(req, res) {
        req$PATH_INFO <- "/bar"
      })

      router$get("/foo", createHitHandle(3))
      router$get("/bar", createHitHandle(4))
      router$use(saw)

      server <- createServer(router)
      mochita(server)$get("/foo")$expect(
        shouldNotHitHandle(1)
      )$expect(
        shouldHitHandle(2)
      )$expect(
        shouldNotHitHandle(3)
      )$expect(
        shouldHitHandle(4)
      )$expect(
        200L,
        "saw GET /bar"
      )$perform()
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
      mochita(server)$get("/")$expect(200L, rawToChar(html_content))$perform()
      mochita(server)$get("/css/main.css")$expect(
        200L,
        rawToChar(css_content)
      )$perform()
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
        mochita(server)$get("/")$expect(
          404L,
          "404 Not Found\n"
        )$perform()
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
        mochita(server)$get("/foo")$expect(404L)$expect(
          function(r) {
            expect_true(grepl(pattern = "Cannot GET /foo", x = r$body))
          }
        )$perform()
      })

      it("should respect html_charset argument", {
        router <- Router$new()
        router$static(
          test_path("static"),
          "/",
          html_charset = ""
        )

        server <- createServer(router)

        mochita(server)$get("/")$expect(
          200L
        )$expect(
          "Content-Type",
          "text/html"
        )$perform()

        # Default behaviour
        router1 <- Router$new()
        router1$static(
          test_path("static"),
          "/"
        )

        server <- createServer(router1)
        mochita(server)$get("/")$expect(
          200L
        )$expect(
          "Content-Type",
          "text/html; charset=utf-8"
        )$perform()
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
        mochita(server)$get("/")$expect(
          200L,
          rawToChar(html_content)
        )$expect(
          "X-Powered-By",
          "routing"
        )$perform()
      })
      it("should respect validation argument", {
        router <- Router$new()
        router$static(
          test_path("static"),
          "/",
          validation = c('"foo" == "bar"')
        )

        server <- createServer(router)

        mochita(server)$get("/")$set(
          "foo",
          "zoo"
        )$expect(403L, "403 Forbidden\n")$perform()

        mochita(server)$get("/")$set(
          "foo",
          "bar"
        )$expect(200L, rawToChar(html_content))$perform()
      })
    })
  })
})
