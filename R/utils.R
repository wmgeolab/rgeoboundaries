#' @noRd
baseurl <- function() "https://www.geoboundaries.org/gbRequest.html"

#' @noRd
drop_nulls <- function(x)
  Filter(Negate(is.null), x)

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
assert_adm_level <- function(adm_level) {
  if (length(adm_level) >= 2)
    stop("You can't mix differents administrative level!")
  dict <- paste0("adm", 0:5)
  cond <- tolower(adm_level) %in% dict
  if (!cond)
    stop("Not a valid ADMIN level code! Use 'ADM0', ADM1', 'ADM2', 'ADM3' 'ADM4' or 'ADM5'",
         call. = FALSE)
}

#' @noRd
assert_version <- function(version = NULL) {
  if (!is.null(version)) {
    if (length(version) >= 2)
      stop("You can't mix differents version of data!")
    dict <- c("2_0_0", "2_0_1", "3_0_0")
    cond <- tolower(version) %in% dict
    if (!cond)
      stop("Not a valid ADMIN type! Use '2_0_0', '2_0_1' or '3_0_0'",
           call. = FALSE)
  }
}

#' @noRd
assert_type <- function(type = NULL) {
  if (!is.null(type)) {
    if (length(type) >= 2)
      stop("You can't mix differents type!")
    dict <- c("HPSCU", "HPSCU", "HPSCGS", "SSCGS", "SSCU")
    cond <- toupper(type) %in% dict
    if (!cond)
      stop("Not a valid type! Use 'HPSCU', 'HPSCGS' 'SSCGS' or 'SSCU'",
           call. = FALSE)
  }
}

create_query <- function(x) {
  f <- function(y)
    paste0(y, "={", y, "}")
  res <- paste0(f(x), collapse = "&")
  paste0("?", res)
}


#' @noRd
#' @importFrom glue glue_data
build_urls <- function(iso3, adm_level,
                       type = NULL, version = NULL) {
  l <- list(ISO = iso3,
            ADM = adm_level,
            TYP = type,
            VER = version)
  l <- lapply(drop_nulls(l), toupper)
  template_url <- paste0(baseurl(),
                         create_query(names(l)))
  glue::glue_data(l, template_url)
}

#' @rdname gb_metadata
#' @importFrom crul Async HttpClient
#' @noRd
.gb_metadata <- function(country, adm_level, type = NULL, version = NULL) {
  iso3 <- country_to_iso3(country)
  assert_adm_level(adm_level)
  assert_type(type)
  assert_version(version)
  if (length(iso3) >= 2) {
    urls <- build_urls(iso3, adm_level, type, version)
    async_cli <- crul::Async$new(url = urls)
    l <- async_cli$get()
    l <- lapply(seq_along(l), function(i) {
      r <- l[[i]]
      r <- r$parse(encoding = "UTF-8")
      r <- jsonlite::fromJSON(r, simplifyVector = FALSE)
      if (length(r) == 0)
        warning(paste(toupper(adm_level),
                      "not available for", country[i]), call. = TRUE)
      as.data.frame(r)
    })
    do.call(rbind, l)
  } else {
    cli <- crul::HttpClient$new(baseurl())
    res <-  cli$get(query = list(ISO = toupper(iso3),
                                 ADM = toupper(adm_level),
                                 TYP = toupper(type),
                                 VER = toupper(version)))
    res <- res$parse(encoding = "UTF-8")
    res <- jsonlite::fromJSON(res, simplifyVector = FALSE)
    if (length(res) == 0)
      stop(paste(toupper(adm_level), "not available for", country))
    as.data.frame(res)
  }
}

#' Get metadata for a country, administrative level, type of data and version
#'
#' Get metadata for a country and an administrative level
#'
#' @param country characher; a vector of country names
#' @param adm_level characher; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. admO being the country.
#' @param type character; defaults to HPSCU. One of HPSCU, HPSCGS, SSCGS, or SSCU. Determines the type of boundary link you receive. More on details
#' @param version character; defaults to the most recent version of geoBoundaries available. The geoboundaries version requested, with underscores.
#' For example, 3_0_0 would return data from version 3.0.0 of geoBoundaries.
#' @rdname gb_metadata
#' @importFrom memoise memoise
#' @export
gb_metadata <- memoise::memoise(.gb_metadata)


#' Get download link for a country, administrative level, type of data and version
#'
#' Get download link for a country, administrative level, type of data and version
#'
#' @param country characher; a vector of country names
#' @param adm_level characher; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. admO being the country.
#' @param type character; defaults to HPSCU. One of HPSCU, HPSCGS, SSCGS, or SSCU. Determines the type of boundary link you receive. More on details
#' @param version character; defaults to the most recent version of geoBoundaries available. The geoboundaries version requested, with underscores.
#' For example, 3_0_0 would return data from version 3.0.0 of geoBoundaries.
#' @noRd
get_download_links <- function(country, adm_level, type = NULL, version = NULL)
  as.character(gb_metadata(country = country,
                           adm_level = adm_level,
                           type = type,
                           version = version)[["downloadURL"]])


extract_shp <- function(zipf, dir) {
  x <- unzip(zipf, list = TRUE)
  shp <- grep("\\-shp\\.zip$", x$Name, value = TRUE)
  zipfcopy <- file.path(dir, basename(zipf))
  res <- file.path(dir, shp)
  if (!file.exists(res)) {
    suppressWarnings(file.copy(zipf, zipfcopy))
    unzip(zipfcopy, files = shp, exdir = dir)
  }
  res
}

#' @noRd
#' @importFrom webmiddens use_midden
get_shp_from_links <- function(links) {
  tmpd <- tempdir()
  zipf <- basename(links)
  tmpf <- file.path(tmpd, zipf)
  cache_dir <- gb_get_cache(TRUE)
  zipfcopy <- file.path(cache_dir, zipf)
  cond <- file.exists(zipfcopy)
  links <- links[!cond]
  tmpf_to_download <- tmpf[!cond]
  ll <- length(links)
  if (ll <= 0) {
    res <- vapply(zipfcopy,
                  function(x)
                    extract_shp(x, cache_dir),
                  character(1), USE.NAMES = FALSE)
  } else {
    if (ll < 2) {
      cli <- crul::HttpClient$new(url = links)
    } else {
      cli <- crul::Async$new(url = links)
    }
    res <- cli$get(disk = tmpf_to_download)
    res <- vapply(tmpf, function(x) {
      extract_shp(x, cache_dir)
    }, character(1), USE.NAMES = FALSE)
  }
  res
}
