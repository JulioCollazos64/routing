# routing 1.1.1

* Fixed routing to match when `req$REQUEST_METHOD` is `NULL`, for
  compatibility with upstream.
* Internal test suite changes only, no other user-facing changes.

# routing 1.1.0

* Added `$static()` method to serve static files via httpuv's background I/O
  thread, mounted at a given URL prefix. 

# routing 1.0.0

* Initial CRAN submission.
