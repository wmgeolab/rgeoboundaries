gb_cache <- NULL

.onLoad <- function(libname, pkgname) {
  x <- hoardr::hoard()
  x$cache_path_set("gb_cache")
  gb_cache <<- x
}