gb_cache <- NULL


.onLoad <- function(libname, pkgname) {
  x <- hoardr::hoard()
  x$cache_path_set("gb_cache")
  gb_cache <<- x
  make_gb_cache_dir(gb_cache)
  invisible(NULL)
}

#' @noRd
make_gb_cache_dir <- function(cache) {
  dir <- cache$cache_path_get()
  if (!dir.exists(dir) && interactive()) {
    ok <- ask_permission(
      paste0("rgeoboundaries want to save the downloaded data in the cache directory:\n\n",
             dir, "\n\n",
             "It allows you to easily re-use files you downloaded. Create this directory?"))
    if (!ok)
      return(invisible(NULL))
    cache$mkdir()
  }
}
