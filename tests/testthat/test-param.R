describe("Router", {
  on.exit(
    {
      httpuv::stopAllServers()
    },
    add = TRUE
  )

  describe("$param(name, fn)", {
    describe("argument validation", {
      it("should reject missing name", {
        router <- Router$new()
        expect_error(router$param(), "argument name is required")
      })

      it("should reject non-character name", {
        router <- Router$new()
        expect_error(router$param(42), "argument name must be a string")
      })

      it("should reject missing fn", {
        router <- Router$new()
        expect_error(router$param("id"), "argument fn is required")
      })

      it("should reject non-function fn", {
        router <- Router$new()
        expect_error(router$param("id", 42), "argument fn must be a function")
      })
    })

    it("should map logic for a path param", {
      router <- Router$new()

      router$param("id", \(req, res, value, name) {
        req$params$id <- suppressWarnings(as.integer(value))
      })

      router$use(\(req, res) {
        "Nothing to see here"
      })

      router$get("/user/:id", \(req, res) {
        res$status <- 200L
        res$send(paste("get user", req$params$id))
      })

      server <- createServer(router)

      r <- fetch(server, "/user/2")
      expect_identical(list(r$status, r$body), list(200L, "get user 2"))

      r <- fetch(server, "/user/bob")
      expect_identical(list(r$status, r$body), list(200L, "get user NA"))
    })

    it("should allow chaining", {
      router <- Router$new()

      router$param("id", \(req, res, value, name) {
        req$params$id <- as.integer(value)
      })

      router$param("id", \(req, res, value, name) {
        req$itemId <- as.integer(value)
      })

      router$get("/user/:id", \(req, res) {
        res$status <- 200L
        res$send(paste0("get user ", req$params$id, " (", req$itemId, ")"))
      })

      server <- createServer(router)

      r <- fetch(server, "/user/2")
      expect_identical(list(r$status, r$body), list(200L, "get user 2 (2)"))
    })

    it("should automatically decode path value", {
      router <- Router$new()
      router$param("user", \(req, res, value, name) {
        req$user <- value
      })
      router$get("/user/:id", \(req, res) {
        res$status <- 200L
        res$headers[["Content-Type"]] <- "text/plain"
        res$send(paste("get user", req$params$id))
      })

      server <- createServer(router)

      r <- fetch(server, '/user/%22bob%2Frobert%22')
      expect_identical(r$body, 'get user "bob/robert"')
    })

    # neither utils::URLdecode nor httpuv::decodeURIComponent return an error here...
    # better report this.

    # it("should 400 on invalid path value", {
    #   router <- Router$new()
    #   router$param("user", \(req, res, value, name) {
    #     req$user <- user
    #   })

    #   router$get("/user/:id", \(req, res) {
    #     res$status <- 200L
    #     res$headers[["Content-Type"]] <- "text/plain"
    #     res$send(paste("get user", req$params$id))
    #   })

    #   server <- createServer(router)

    #   r <- fetch(server, "/user/%bob")
    # })

    it("should only invoke fn when necessary", {
      router <- Router$new()

      router$param("id", \(req, res, value, name) {
        res$headers[["x-id"]] <- value
      })

      router$param("user", \(req, res, value, name) {
        stop("boom")
      })

      router$get("/user/:user", saw)
      router$put("/user/:id", saw)

      server <- createServer(router)

      r <- fetch(server, "/user/bob", method = "GET")
      expect_identical(r$status, 500L)

      r <- fetch(server, "/user/bob", method = "PUT")
      expect_identical(
        list(r$status, r$headers[["x-id"]], r$body),
        list(200L, "bob", "saw PUT /user/bob")
      )
    })

    it("should only invoke fn once per request", {
      router <- Router$new()

      router$param("user", \(req, res, value, name) {
        req$count <- (req$count %||% 0) + 1
        req$user <- value
      })

      router$get("/user/:user", create_hit_handle(1))
      router$get("/user/:user", create_hit_handle(2))

      router$use(\(req, res) {
        res$status <- 200L
        res$send(paste("get user", req$user, req$count, "times"))
      })

      server <- createServer(router)

      r <- fetch(server, "/user/bob")
      expect_identical(
        list(r$status, r$body),
        list(200L, "get user bob 1 times")
      )
    })

    it("should keep changes to req$params value", {
      router <- Router$new()

      router$param("id", \(req, res, value, name) {
        req$count <- (req$count %||% 0) + 1
        req$params$id <- as.integer(value)
      })

      router$get("/user/:id", \(req, res) {
        res$headers[["x-user-id"]] <- as.character(req$params$id)
        forward()
      })

      router$get("/user/:id", \(req, res) {
        res$status <- 200L
        res$send(paste("get user", req$params$id, req$count, "times"))
      })

      server <- createServer(router)

      r <- fetch(server, "/user/01")
      expect_identical(list(r$status, r$body), list(200L, "get user 1 1 times"))
    })

    it("should invoke fn if path value differs", {
      router <- Router$new()

      router$param("user", \(req, res, value, name) {
        req$count <- (req$count %||% 0L) + 1L
        req$user <- value
        req$vals <- c(req$vals, value)
      })

      router$get("/:user/bob", create_hit_handle(1))
      router$get("/user/:user", create_hit_handle(2))

      router$use(\(req, res) {
        res$status <- 200L
        res$send(paste0(
          "get user ",
          req$user,
          " ",
          req$count,
          " times: ",
          paste(req$vals, collapse = ", ")
        ))
      })

      server <- createServer(router)

      r <- fetch(server, "/user/bob")
      expect_identical(
        list(r$status, r$body),
        list(200L, "get user bob 2 times: user, bob")
      )
    })

    it("should catch exception in fn", {
      router <- Router$new()

      router$param("user", \(req, res, value, name) {
        stop("boom")
      })

      router$get("/user/:user", \(req, res) {
        res$status <- 200L
        res$send(paste("get user", req$params$user))
      })

      server <- createServer(router)

      r <- fetch(server, "/user/bob")
      expect_identical(r$status, 500L)
    })

    it("should catch exception in chained fn", {
      router <- Router$new()

      router$param("user", \(req, res, value, name) {})

      router$param("user", \(req, res, value, name) {
        stop("boom")
      })

      router$get("/user/:user", \(req, res) {
        res$status <- 200L
        res$send(paste("get user", req$params$user))
      })

      server <- createServer(router)

      r <- fetch(server, "/user/bob")
      expect_identical(r$status, 500L)
    })

    describe('forward("route")', {
      it("should cause route with param to be skipped", {
        router <- Router$new()

        router$param("id", \(req, res, value, name) {
          id <- suppressWarnings(as.integer(value))
          if (is.na(id)) {
            forward("route")
          } else {
            req$params$id <- id
          }
        })

        router$get("/user/:id", \(req, res) {
          res$status <- 200L
          res$send(paste("get user", req$params$id))
        })

        router$get("/user/new", \(req, res) {
          res$status <- 400L
          res$send("cannot get a new user")
        })

        server <- createServer(router)

        r <- fetch(server, "/user/2")
        expect_identical(list(r$status, r$body), list(200L, "get user 2"))

        r <- fetch(server, "/user/bob")
        expect_identical(r$status, 404L)

        r <- fetch(server, "/user/new")
        expect_identical(
          list(r$status, r$body),
          list(400L, "cannot get a new user")
        )
      })

      it("should invoke fn if path value differs", {
        router <- Router$new()

        router$param("user", \(req, res, value, name) {
          req$count <- if (is.null(req$count)) 1L else req$count + 1L
          req$user <- value
          req$vals <- c(req$vals, value)
          forward(if (identical(value, "user")) "route" else NULL)
        })

        router$get("/:user/bob", create_hit_handle(1))
        router$get("/user/:user", create_hit_handle(2))

        router$use(\(req, res) {
          res$status <- 200L
          res$send(paste0(
            "get user ",
            req$user,
            " ",
            req$count,
            " times: ",
            paste(req$vals, collapse = ", ")
          ))
        })

        server <- createServer(router)

        r <- fetch(server, "/user/bob")
        should_not_hit_handle(r, 1)
        should_hit_handle(r, 2)
        expect_identical(
          list(r$status, r$body),
          list(200L, "get user bob 2 times: user, bob")
        )
      })
    })
  })
})
