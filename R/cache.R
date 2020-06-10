#' Caching geoBoundaries downloaded files
#'
#' Manage cached geoBoundaries downloaded files
#'
#' @name gb_cache
#'
#' @details The default cache directory is
#' `~/.cache/R/gb_cache`, but you can set
#' your own path using `gb_set_cache(path)`
#'
#' @importFrom hoardr hoard
#'
#' @examples \dontrun{
#' gb_cache
#' ## change the default cache directory
#' tmp <- tempdir()
#' gb_set_cache(tmp)
#'
#' ## print current cache directory
#' gb_get_cache()
#'
#' ## List available files in the current cache directory
#' gb_list_cache()
#'
#' l <- gb_list_cache()[1] ## get the first file
#' gb_delete_from_cache(l) ## delete it
#'
#' gb_clear_cache() ## delete all cached files
#' }
NULL


#' Set the cache directory
#'
#' @rdname gb_cache
#'
#' @param path character; path of the directory to set
#'
#' @return the cache directory
#' @export
gb_set_cache <- function(path) {
  assert_cache(rhdx_cache)
  rhdx_cache$cache_path_set(path)
}

#' Displays the full path to the cache directory
#'
#' @rdname gb_cache
#'
#' @param create logical; if TRUE create missing cache
#'
#' @export
gb_get_cache <- function(create = FALSE) {
  dir <- gb_cache$cache_path_get()
  if (create & !dir.exists(dir))
    gb_cache$mkdir()
  dir
}

#' Clear all cached files
#'
#' @rdname gb_cache
#'
#' @note This function will clear all cached files
#' @param force logical; force delete. default: `FALSE`
#' @export
gb_clear_cache <- function(force = FALSE) {
  files <- list.files(gb_cache$cache_path_get(), full.names = TRUE)
  unlink(files, recursive = TRUE, force = force)
}

#' List of files available in the cache directory
#'
#' @rdname gb_cache
#'
#' @return list of files in the cache
#' @export
gb_list_cache <- function(full_path = FALSE) {
  list.files(gb_cache$cache_path_get(), full.names = full_path)
}

#' Delete a given file from cache
#'
#' @rdname gb_cache
#'
#' @param file Character, the file to delete
#'
#' @export
gb_delete_from_cache <- function(file) {
  gb_cache$delete(file)
}
