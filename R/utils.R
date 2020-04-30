#' @noRd
baseurl <- function() "https://www.geoboundaries.org/gbRequest.html"

#' @noRd
assert_country_iso3 <- function(country_iso3) {
  dict <- tolower(countrycode::codelist$iso3c)
  cond <- tolower(country_iso3) %in% dict
  if (!cond)
    stop("Not a valid ISO3 country code!", call. = FALSE)
}

#' @noRd
assert_adm_level <- function(adm_level) {
  dict <- c("adm0", "adm1", "adm2", "adm3")
  cond <- tolower(adm_level) %in% dict
  if (!cond)
    stop("Not a valid ADM level code! Use ADM0, ADM1, ADM2 or ADM3", call. = FALSE)
}

#' @noRd
cli <- crul::HttpClient$new(baseurl())

#' @noRd
get_download_link <- function(country_iso3, adm_level) {
  assert_country_iso3(country_iso3)
  assert_adm_level(adm_level)
  res <- cli$get(query = list(ISO = toupper(country_iso3), ADM = toupper(adm_level)))
  res <- res$parse(encoding = "UTF-8")
  res <- jsonlite::fromJSON(res)
  res$downloadURL
}

#' @noRd
download_geoboundaries <- function(dir = NULL, country_iso3, adm_level, quiet = FALSE, ...) {
  link <- get_download_link(country_iso3, adm_level)
  zipfile <- file.path(dir, basename(link))
  if (is.null(dir))
    zipfile <- tempfile(fileext = ".zip")
  download.file(link, destfile = zipfile, quiet = quiet, ...)
  zipfile
}

#' @noRd
.get_geoboundaries <- function(country_iso3, adm_level, quiet_download = FALSE) {
  zipfile <- download_geoboundaries(country_iso3 = country_iso3, adm_level = adm_level, quiet = quiet_download)
  file <- grep("\\.geojson$", unzip(zipfile, list = TRUE)$Name, value = TRUE)
  file <- file.path(zipfile, file)
  sf::read_sf(file.path("/vsizip/", file))
}

#' @noRd
get_geoboundaries <- memoise::memoize(.get_geoboundaries)
