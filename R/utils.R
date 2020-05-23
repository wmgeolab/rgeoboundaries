#' @noRd
baseurl <- function() "https://www.geoboundaries.org/gbRequest.html"

#' @noRd
#' @noRd
#' @importFrom countrycode countryname
country_to_iso3 <- function(country) {
  iso3 <- countrycode::countryname(country, destination = "iso3c")
  if (any(is.na(iso3))) {
    cntry <- paste(country[is.na(iso3)], collapse = ", ")
    stop(paste(cntry, "not valid name(s)!"), call. = FALSE)
  }
  iso3
}

#' @noRd
assert_admin_level <- function(adm_level) {
  if (length(adm_level) >= 2)
    stop("You can't mix differents administrative level!")
  dict <- paste0("adm", 0:5)
  cond <- tolower(adm_level) %in% dict
  if (!cond)
    stop("Not a valid ADMIN level code! Use 'ADM0', ADM1', 'ADM2', 'ADM3' 'ADM4' or 'ADM5'", call. = FALSE)
}

#' @noRd
#' @importFrom glue glue
build_urls <- function(iso3, adm_level) {
  assert_admin_level(adm_level)
  template_url <- paste0(baseurl(), "?ISO={iso3}&ADM={adm_level}")
  glue::glue(template_url, iso3 = toupper(iso3), adm_level = toupper(adm_level))
}

#' @noRd
#' @importFrom crul HttpClient
cli <- crul::HttpClient$new(baseurl())

#' @noRd
#' @importFrom crul Async
.gb_metadata <- function(country, adm_level) {
  iso3 <- country_to_iso3(country)
  assert_admin_level(adm_level)
  if (length(iso3) >= 2) {
    urls <- build_urls(iso3, adm_level)
    cli <- crul::Async$new(url = urls)
    l <- cli$get()
    l <- lapply(seq_along(l), function(i) {
      r <- l[[i]]
      r <- r$parse(encoding = "UTF-8")
      r <- jsonlite::fromJSON(r, simplifyVector = FALSE)
      if (length(r) == 0)
        warning(paste(toupper(adm_level), "not available for", country[i]), call. = TRUE)
      as.data.frame(r)
    })
    do.call(rbind, l)
  } else {
    res <-  cli$get(query = list(ISO = toupper(iso3),
                                 ADM = toupper(adm_level)))
    res <- res$parse(encoding = "UTF-8")
    res <- jsonlite::fromJSON(res, simplifyVector = FALSE)
    if (length(res) == 0)
      stop(paste(toupper(adm_level), "not available for", country))
    as.data.frame(res)
  }
}

#' @noRd
#' @importFrom memoise memoise
gb_metadata <- memoise::memoise(.gb_metadata)

#' @noRd
get_download_link <- function(country, adm_level)
  gb_metadata(country, adm_level)[["downloadURL"]]

#' @noRd
get_geojson_from_link <- function(link)
  gsub("\\-all\\.zip", ".geojson", basename(link))

#' @noRd
get_shp_from_link <- function(link)
  gsub("\\-all\\.zip", "\\-shp\\.zip", basename(link))
