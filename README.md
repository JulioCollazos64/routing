
<!-- README.md is generated from README.Rmd. Please edit that file -->

# routing

<!-- badges: start -->

<!-- badges: end -->

`routing` is an R port of
[pillarjs/router](https://github.com/pillarjs/router), the routing
mechanism that underpins Express.js. It gives R web frameworks a syntax
that mimics how Express.js applications are written, so the same
patterns that make Express.js applications easy to read and reason about
are now available in R.

## Installation

``` r
pak::pak("JulioCollazos64/routing")
```

Let’s look at an example:

``` r
library(routing)
router <- Router$new()
router$use(function(req,res){
  print(req)
})
router$get("/hello", function(req,res){
  res$send("Hello world!")
})
```

Even if we knew nothing about web development, we can clearly see the
things that matter from this code snippet:

- **Router** - We create an object called `router` that holds all the
  rules for how incoming requests should be handled.
- **Middleware** - `router$use()` appends a piece of code that runs on
  every requests, regardless of path or method.
- **Route definition**
  - **HTTP verb** - `get` means this route only responds to HTTP GET
    requests
  - **PATH** - `/hello` is the URL path this route matches

Moreover, the routing mechanism happens in the *same order you write
your code* so you can read how your request goes from top to bottom.

## How it differs from Express.js

1.  We use `forward()` instead of `next()` to pass control to the next
    handler.
2.  You don’t need to declare `forward` as an argument to your handler
    to call it inside.
3.  If your handler doesn’t return a response or call `forward()`, one
    is called on your behalf.
