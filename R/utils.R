#' @noRd
baseurl <- function() "https://www.geoboundaries.org/api/current/gbOpen/"

#' @noRd
drop_nulls <- function(x)
  Filter(Negate(is.null), x)

#' @noRd
#' @importFrom countrycode countryname countrycode
country_to_iso3 <- function(country) {
  if (!is.null(country)) {
    ind <- nchar(country) >= 4
    if (any(ind)) {
      iso3 <- countryname(country[ind],
                          destination = "iso3c")
      iso3 <- c(country[!ind], iso3)

    } else {
      iso3  <- suppressWarnings(countrycode(country,
                                            origin = "iso3c",
                                            destination = "iso3c"))
    }
    if (any(is.na(iso3))) {
      cntry <- paste(country[is.na(iso3)], collapse = ", ")
      stop(paste(cntry, "not valid name(s)!"), call. = FALSE)
    }
    iso3
  }
}

#' @noRd
assert_adm_lvl <- function(adm_lvl, dict = paste0("adm", 0:5)) {
  if (length(adm_lvl) >= 2)
    stop("You can't mix different administrative levels!")
  cond <- tolower(adm_lvl) %in% dict
  if (!cond)
    stop(paste("Not a valid ADMIN level code! Use",
               paste(dict, collapse = ", "),
               ), call. = FALSE)
}

#' @noRd
assert_version <- function(version = NULL) {
  if (!is.null(version)) {
    if (length(version) >= 2)
      stop("You can't mix different versions of data!")
    dict <- c("2_0_1", "3_0_0")
    cond <- tolower(version) %in% dict
    if (!cond)
      stop("Not a valid version! Use '2_0_1' or '3_0_0'",
           call. = FALSE)
  }
}

#' @noRd
assert_type <- function(type = NULL) {
  if (!is.null(type)) {
    if (length(type) >= 2)
      stop("You can't mix different types!")
    dict <- c("HPSCU", "HPSCU", "HPSCGS", "SSCGS", "SSCU", "CGAZ")
    cond <- toupper(type) %in% dict
    if (!cond)
      stop("Not a valid type! Use 'HPSCU', 'HPSCGS' 'SSCGS', 'SSCU' or CGAZ",
           call. = FALSE)
  }
}

#' @noRd
create_query <- function(x) {
  res <- paste0(x, collapse = "/")
  paste0("/", res)
}


#' @noRd
#' @importFrom glue glue_data
build_urls <- function(iso3, adm_lvl,
                       type = NULL, version = NULL) {
  l <- list(iso3,
            adm_lvl,
            type,
            version)
  l <- lapply(drop_nulls(l), toupper)
  template_url <- paste0(baseurl(),
                         create_query(l))

  glue_data(l, template_url)
}

#' @rdname gb_meta
#' @importFrom crul Async HttpClient
#' @importFrom jsonlite fromJSON
#' @noRd
.gb_meta <- function(country = NULL, adm_lvl, type = NULL, version = NULL) {

  iso3 <- country_to_iso3(country)
  assert_type(type)
  assert_version(version)

  if (grepl("^[0-5]$", adm_lvl))
    adm_lvl <- paste0("ADM", adm_lvl)

  if (length(iso3) >= 2) {
    urls <- build_urls(iso3, adm_lvl, type, version)
    async_cli <- Async$new(url = urls)
    l <- async_cli$get()
    l <- lapply(seq_along(l), function(i) {
      r <- l[[i]]
      r <- r$parse(encoding = "UTF-8")
      r <- fromJSON(r)
      if (length(r) == 0)
        warning(paste(toupper(adm_lvl),
                      "not available for", country[i]), call. = TRUE)
      r
    })
    do.call(rbind, l)
  } else {
    cli <- HttpClient$new(baseurl())
    res <-  cli$get(query = list(ISO = toupper(iso3),
                                 ADM = toupper(adm_lvl),
                                 TYP = toupper(type),
                                 VER = toupper(version)))
    res <- res$parse(encoding = "UTF-8")
    res <- fromJSON(res)
    if (length(res) == 0)
      stop(paste(toupper(adm_lvl), "not available for", country))
    res
  }
}

#' Get metadata for a country, administrative level, type of data and version
#'
#' Get metadata for a country and an administrative level
#'
#' @param adm_lvl characher or integer; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. adm0 being the country. 0, 1, 2, 3, 4 or 5 can also be used.
#' @param country characher; a vector of country names
#' @param type character; defaults to HPSCU. One of HPSCU, HPSCGS, SSCGS, or SSCU.
#'  Determines the type of boundary link you receive. More on details
#' @param version character; defaults to the most recent version of geoBoundaries available.
#'  The geoboundaries version requested, with underscores.
#' For example, 3_0_0 would return data from version 3.0.0 of geoBoundaries.
#' @rdname gb_meta
#' @importFrom memoise memoise
#' @noRd
gb_meta <- memoise(.gb_meta)


#' Get metadata for a country, administrative level, type of data and version
#'
#' Get metadata for a country and an administrative level, type of data and version
#'
#' @param adm_lvl characher; administrative level, adm0, adm1, adm2, adm3, adm4, adm5 or all. adm0 being the country and all to access all available levels.
#' @param country characher; a vector of country names or iso3 country code.
#' @param type character; defaults to HPSCU. One of HPSCU, HPSCGS, SSCGS, or SSCU.
#'  Determines the type of boundary link you receive. More on details
#' @param version character; defaults to the most recent version of geoBoundaries available.
#'  The geoboundaries version requested, with underscores.
#' For example, 3_0_0 would return data from version 3.0.0 of geoBoundaries.
#' @rdname gb_metadata
#' @export
gb_metadata <- function(country = NULL, adm_lvl = "all", type = NULL, version = NULL) {
  assert_adm_lvl(adm_lvl, c("all", paste0("adm", 0:5), 0:5))
  gb_meta(country = country,
          adm_lvl = adm_lvl,
          type = type,
          version = version)
}



#' Get the highest administrative level available for a given country
#'
#' Get the highest administrative level available for a given country
#'
#' @importFrom countrycode countrycode
#'
#' @param country characher; a vector of country names or iso3 country code.
#' @param type character; defaults to HPSCU. One of HPSCU, HPSCGS, SSCGS, or SSCU.
#'  Determines the type of boundary link you receive. More on details
#' @param version character; defaults to the most recent version of geoBoundaries available.
#' @param license character; the license of the maximum administrative level
#'
#' @return a data.frame with the country names and corresponding highest administrative level
#' @export
gb_max_adm_lvl <- function(country = NULL, type = NULL,
                           version = NULL, license = NULL) {
  ord <- country_to_iso3(country)
  df <- gb_meta(country = country,
                adm_lvl = "all",
                type = type,
                version = version)
  available_licenses <- unique(df$boundaryLicense)
  if (!is.null(license)) {
    stopifnot("License not available!" = license %in% available_licenses)
    df <- df[df$boundaryLicense == license, ]
  }

  res <- tapply(df$boundaryType, df$boundaryISO, max)
  res <- res[toupper(ord)]
  res <- as.integer(gsub("[^0-5]", "", res))
  names(res) <- country
  res
}

#' Get download link for the zip with data for a country, administrative level, type of data and version
#'
#' Get download link for the zip with data for a country, administrative level, type of data and version
#'
#' @param country characher; a vector of country names
#' @param adm_lvl characher; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. adm0 being the country.
#' @param type character; defaults to HPSCU. One of HPSCU, HPSCGS, SSCGS, or SSCU.
#'  Determines the type of boundary link you receive. More on details
#' @param version character; defaults to the most recent version of geoBoundaries available.
#'  The geoboundaries version requested, with underscores.
#' For example, 3_0_0 would return data from version 3.0.0 of geoBoundaries.
#' @noRd
get_zip_links <- function(country = NULL, adm_lvl, type = NULL, version = NULL) {
  assert_adm_lvl(adm_lvl, c(paste0("adm", 0:5), 0:5))
  l <- gb_meta(country = country,
               adm_lvl = adm_lvl,
               type = type,
               version = version)[["downloadURL"]]
  as.character(l)
}

#' @noRd
#' @importFrom utils download.file
get_cgaz_shp_link <- function(adm_lvl = "adm0", quiet = TRUE) {
  assert_adm_lvl(adm_lvl, c(paste0("adm", 0:2), 0:2))

  if (grepl("^[0-2]$", adm_lvl))
    adm_lvl <- paste0("adm", adm_lvl)

  adm_lvl <- tolower(adm_lvl)
  base_url <- "https://www.geoboundaries.org/data/geoBoundariesCGAZ-3_0_0/"
  url_root <- switch(adm_lvl,
                adm0 = paste0(base_url,
                              "ADM0/simplifyRatio_25/shp/geoBoundariesCGAZ_ADM0"),
                adm1 = paste0(base_url,
                              "ADM1/simplifyRatio_25/shp/geoBoundariesCGAZ_ADM1"),
                adm2 = paste0(base_url,
                              "ADM2/simplifyRatio_25/shp/geoBoundariesCGAZ_ADM2"))
  cache_dir <- gb_get_cache(create = TRUE)
  folder_name <- basename(url_root)
  cgaz_folder <- file.path(cache_dir, folder_name)
  if (!dir.exists(cgaz_folder))
    dir.create(cgaz_folder)
  prefixes <- c(".shp", ".prj", ".shx", ".dbf")
  urls <- paste0(url_root, prefixes)
  destfiles <- file.path(cgaz_folder, basename(urls))
  if (length(list.files(cgaz_folder)) == 0) {
    cli <- Async$new(url = urls)
    cli$get(disk = destfiles)
  }
  grep("shp$", destfiles, value = TRUE)
}


#' @importFrom utils unzip
#' @noRd
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
#' @importFrom crul HttpClient Async
get_shp_from_links <- function(links) {
  tmpd <- tempdir()
  zipf <- basename(links)
  tmpf <- file.path(tmpd, zipf)
  cache_dir <- gb_get_cache(create = TRUE)
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
      cli <- HttpClient$new(url = links)
    } else {
      cli <- Async$new(url = links)
    }
    res <- cli$get(disk = tmpf_to_download)
    res <- vapply(tmpf, function(x) {
      extract_shp(x, cache_dir)
    }, character(1), USE.NAMES = FALSE)
  }
  res
}
