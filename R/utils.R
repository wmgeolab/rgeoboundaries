#' @noRd
baseurl <- function() "https://www.geoboundaries.org/api/current"

#' @noRd
drop_nulls <- function(x)
  Filter(Negate(is.null), x)

#' @noRd
#' @importFrom countrycode countryname countrycode
country_to_iso3 <- function(country) {
  if (!is.null(country)) {
    if ("kosovo" %in% tolower(country))
      country[tolower(country) %in% "kosovo"] <- "XKX"
    ind <- nchar(country) >= 4
    if (any(ind)) {
      iso3 <- countryname(country[ind],
                          destination = "iso3c")
      iso3 <- c(country[!ind], iso3)

    }
    else {
      iso3 <- country
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
create_query <- function(x) {
  f <- function(y)
    paste0("{",y,"}")
  res <- paste0(f(x), collapse = "/")
  paste0("/", res)
}

#' @noRd
#' @importFrom glue glue_data
build_urls <- function(iso3, adm_lvl,
                       release_type) {
  iso3 <- toupper(iso3)
  adm_lvl <- toupper(adm_lvl)
  l <- list(RELEASE_TYPE = release_type,
            ISO = iso3,
            ADM = adm_lvl)
  l <- drop_nulls(l)
  template_url <- paste0(baseurl(),
                         create_query(names(l)))
  glue_data(l, template_url)
}

#' @importFrom crul HttpRequest AsyncQueue
#' @noRd
xget_async <- function(urls) {
  reqs <- lapply(urls, function(url) {
    req <- HttpRequest$new(url)
    req$retry("get",
              times = 3,
              retry_only_on = c(500, 503),
              terminate_on = 404)
  })
  sleep <- 0.1
  res <- AsyncQueue$new(.list = reqs,
                        bucket_size = Inf,
                        sleep = sleep)
  res$request()
  cond <- res$status_code() >= 300L
  if (any(cond)) {
    msg <- res$content()[cond]
    stop("One of the coutry names or iso3 codes is invalid",
         call. = FALSE)
  }
  res <- res$parse(encoding = "UTF-8")
  res
}

#' @noRd
xget <- function(urls) {
  cli <- HttpClient$new(urls)
  res <- cli$retry("get",
                   times = 3,
                   retry_only_on = c(500, 503),
                   terminate_on = 404)
  if (res$status_code >= 300)
    stop("The country name or iso3 code is invalid!",
         call. = FALSE)
  res$raise_for_ct_json()
  res$parse("UTF-8")
}

#' @rdname gb_meta
#' @importFrom crul Async HttpClient
#' @importFrom jsonlite fromJSON
#' @noRd
.gb_meta <- function(country = NULL, adm_lvl,
                     release_type = c("gbOpen", "gbHumanitarian", "gbAuthoritative")) {
  release_type <- match.arg(release_type)
  iso3 <- country_to_iso3(country)
  adm_lvl <- toupper(adm_lvl)

  if (grepl("^[0-5]$", adm_lvl))
    adm_lvl <- paste0("ADM", adm_lvl)

  urls <- build_urls(iso3, "ALL", release_type)

  if (length(urls) >= 2) {
    res <- xget_async(urls)
    res <- do.call(rbind, lapply(res,
                          \(x) as.data.frame(fromJSON(x))))
  } else {
    res <- xget(urls)
    res <- as.data.frame(fromJSON(res))
  }

  if (adm_lvl != "ALL") {
    if (!adm_lvl %in% res$boundaryType) {
      stop(paste0(adm_lvl, " not available!"),
           call. = FALSE)
    } else {
      res <- res[res$boundaryType %in% adm_lvl, ]
    }
  }
  res
}

#' Get metadata for a country, administrative level, type of data and version
#'
#' Get metadata for a country and an administrative level
#'
#' @param adm_lvl characher or integer; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. adm0 being the country. 0, 1, 2, 3, 4 or 5 can also be used.
#' @param country characher; a vector of country names or iso 3 country codes
#' @param release_type character; This is one of gbOpen, gbHumanitarian, or gbAuthoritative. For most users, we suggest using gbOpen, as it is CC-BY 4.0 compliant, and can be used for most purposes so long as attribution is provided. gbHumanitarian files are mirrored from UN OCHA, but may have less open licensure. gbAuthoritative files are mirrored from UN SALB, and cannot be used for commerical purposes, but are verified through in-country processes. Default to gbOpen.
#' @rdname gb_meta
#' @importFrom memoise memoise
#' @noRd
gb_meta <- memoise(.gb_meta)


#' Get metadata for a country, administrative level, type of data and version
#'
#' Get metadata for a country and an administrative level, type of data and version
#'
#' @param adm_lvl characher or integer; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. adm0 being the country. 0, 1, 2, 3, 4 or 5 can also be used.
#' @param country characher; a vector of country names or iso 3 country codes
#' @param release_type character; This is one of gbOpen, gbHumanitarian, or gbAuthoritative. For most users, we suggest using gbOpen, as it is CC-BY 4.0 compliant, and can be used for most purposes so long as attribution is provided. gbHumanitarian files are mirrored from UN OCHA, but may have less open licensure. gbAuthoritative files are mirrored from UN SALB, and cannot be used for commerical purposes, but are verified through in-country processes. Default to gbOpen.
#' @rdname gb_metadata
#' @export
gb_metadata <- function(country = NULL, adm_lvl = "all",
                        release_type = c("gbOpen", "gbHumanitarian", "gbAuthoritative")) {
  assert_adm_lvl(adm_lvl, c("all", paste0("adm", 0:5), 0:5))
  gb_meta(country = country,
          adm_lvl = adm_lvl,
          release_type = release_type)
}

#' Get the highest administrative level available for a given country
#'
#' Get the highest administrative level available for a given country
#'
#' @importFrom countrycode countrycode
#'
#' @param country characher; a vector of country names or iso3 country codes.
#' @param release_type character; This is one of gbOpen, gbHumanitarian, or gbAuthoritative.
#' For most users, we suggest using gbOpen, as it is CC-BY 4.0 compliant, and can be used
#' for most purposes so long as attribution is provided. gbHumanitarian files are
#' mirrored from UN OCHA, but may have less open licensure. gbAuthoritative files are
#' mirrored from UN SALB, and cannot be used for commerical purposes,
#' but are verified through in-country processes. Default to gbOpen.
#' @return a data.frame with the country names and corresponding highest administrative level
#' @export
gb_max_adm_lvl <- function(country = NULL,
                           release_type = c("gbOpen", "gbHumanitarian", "gbAuthoritative")) {
  release_type <- match.arg(release_type)
  ord <- country_to_iso3(country)
  df <- gb_meta(country = country,
                adm_lvl = "all",
                release_type = release_type)
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
#' @param adm_lvl characher; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. adm0
#' being the country.
#' @param release_type character; This is one of gbOpen, gbHumanitarian, or gbAuthoritative.
#' For most users, we suggest using gbOpen, as it is CC-BY 4.0 compliant, and can be used for most
#' purposes so long as attribution is provided. gbHumanitarian files are mirrored from UN OCHA,
#' but may have less open licensure. gbAuthoritative files are mirrored from UN SALB, and cannot
#' be used for commerical purposes, but are verified through in-country processes. Default to gbOpen.
#' @noRd
#' @importFrom jsonlite toJSON
get_adm_shp_link <- function(country = NULL,
                          adm_lvl,
                          type = c("unsimplified", "simplified"),
                          release_type = c("gbOpen", "gbHumanitarian", "gbAuthoritative"),
                          force = FALSE,
                          quiet = TRUE) {
  type <- match.arg(type)
  release_type <- match.arg(release_type)
  assert_adm_lvl(adm_lvl, c(paste0("adm", 0:5), 0:5))

  l <- gb_meta(country = country,
               adm_lvl = adm_lvl,
               release_type = release_type)
  urls <- l[["staticDownloadLink"]]
  temp_dir <- tempdir()
  
  if (!dir.exists(temp_dir)) {
    message("Creating temporary directory ", temp_dir)
    dir.create(temp_dir)
  }
  
  destfiles <- file.path(temp_dir, basename(urls))
  if (!all(file.exists(destfiles)) | isTRUE(force)) {
    sys_timeout <- getOption('timeout')
    options(timeout = 600)  # Set custom timeout duration
    lapply(seq_along(urls), \(i)
           download.file(urls[i],
                         destfile = destfiles[i],
                         quiet = quiet))
    options(timeout = sys_timeout)
  }
  files <- switch(type,
                  unsimplified = gsub("-all.zip$", ".shp",
                                      basename(destfiles)),
                  simplified = gsub("-all.zip$", "_simplified.shp",
                                    basename(destfiles)))
  data.frame(path = path.expand(file.path(destfiles, files)),
             shapeCanonical = l[["boundaryCanonical"]],
             shapeGroup = l[["boundaryISO"]],
             shapeType = l[["boundaryType"]])
}

#' @noRd
#' @importFrom utils download.file
get_cgaz_shp_link <- function(adm_lvl = "adm0", quiet = TRUE, force = FALSE) {
  assert_adm_lvl(adm_lvl, c(paste0("adm", 0:2), 0:2))

  if (grepl("^[0-2]$", adm_lvl))
    adm_lvl <- paste0("adm", adm_lvl)

  adm_lvl <- tolower(adm_lvl)
  base_url <- "https://github.com/wmgeolab/geoBoundaries/raw/main/releaseData/CGAZ/"
  url_root <- switch(adm_lvl,
                adm0 = paste0(base_url,
                              "geoBoundariesCGAZ_ADM0.zip"),
                adm1 = paste0(base_url,
                              "geoBoundariesCGAZ_ADM1.zip"),
                adm2 = paste0(base_url,
                              "geoBoundariesCGAZ_ADM2.zip"))
  temp_dir <- tempdir()
  
  if (!dir.exists(temp_dir)) {
    message("Creating temporary directory ", temp_dir)
    dir.create(temp_dir)
  }
  urls <- url_root
  destfiles <- file.path(temp_dir, basename(urls))
  if (!all(file.exists(destfiles)) | isTRUE(force)) {
    sys_timeout <- getOption('timeout')
    options(timeout = 600)  # Set custom timeout duration
    download.file(urls,
                  destfile = destfiles,
                  quiet = quiet)
    options(timeout = sys_timeout)
  }
  path.expand(destfiles)
}

#' @noRd
ask_permission <- function(msg, default = TRUE) {
  ok <- utils::askYesNo(msg, default = default)
  isTRUE(ok)
}
