#' @importFrom sf st_read
#' @noRd
.read_gb <- function(path, quiet = TRUE) {
  if (length(path) >= 2) {
      l <- lapply(seq_along(path), function(i)
        st_read(path[i], quiet = quiet))
      res <- do.call(rbind, l)
  } else {
      res <- st_read(path, quiet = quiet)
      res <- sf::st_cast(res, "MULTIPOLYGON")
  }
  res
}

#' @noRd
#' @importFrom memoise memoise
read_gb <- memoise(.read_gb)

#' Get the administrative boundaries of selected countries
#'
#' Access country boundaries at a specified administrative level
#'
#' @importFrom tools file_path_as_absolute
#' @importFrom lifecycle deprecated deprecate_warn is_present
#'
#' @rdname geoboundaries
#' @param country characher; a vector of country names or country ISO3. If NULL all countries will be used
#'  for adm0, adm1, adm2 where the administrative level are available
#' @param adm_lvl character; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. adm0 being the country boundary. 0, 1, 2, 3, 4 or 5 can also be used.
#' @param type character; One of simplified and unsimplified.
#'  Determines the type of boundary to use. Default to unsimplified.
#' @param release_type character; This is one of gbOpen, gbHumanitarian, or gbAuthoritative.
#' For most users, we suggest using gbOpen, as it is CC-BY 4.0 compliant, and can be used for most
#' purposes so long as attribution is provided. gbHumanitarian files are mirrored from UN OCHA,
#' but may have less open licensure. gbAuthoritative files are mirrored from UN SALB, and cannot
#' be used for commerical purposes, but are verified through in-country processes.
#' Default to gbOpen
#' @param quiet logical; if TRUE no message while downloading and reading the data.
#' Default to FALSE
#' @param overwrite logical; if TRUE overwrite the files downloaded previously in the cache.
#' Default to FALSE.
#' @param version character; deprecated parameter.
#'
#' @details
#' Different types of boundaries are available:
#'
#' * unsimplified - The premier geoBoundaries release, representing the highest precision files
#'   available for every country in the world. No standardization is performed on these
#'   files, so (for example) two countries may overlap in the case of contested boundaries.
#'
#' * simplified - A simplified version of every file
#'   available for every country in the world. No standardization is performed on these files,
#'   so (for example) two countries may overlap in the case of contested boundaries.
#'
#' The following wrappers are also available:
#'
#' * `gb_adm0` returns the country boundaries
#' * `gb_adm1` if available, returns the country first administrative level boundaries
#' * `gb_adm2` if available, returns the country second administrative level boundaries
#' * `gb_adm3` if avaiable, returns the country third administrative level boundaries
#' * `gb_adm4` if available, returns the country fourth administrative level boundaries
#' * `gb_adm5` if available, returns the country first administrative level boundaries
#'
#' @references
#' Runfola D, Anderson A, Baier H, Crittenden M, Dowker E, Fuhrig S, et al. (2020)
#' geoBoundaries: A global database of political administrative boundaries.
#' PLoS ONE 15(4): e0231866. https://doi.org/10.1371/journal.pone.0231866
#'
#' @return a `sf` object
#'
#' @export
geoboundaries <- function(country = NULL,
                          adm_lvl = "adm0",
                          type = c("unsimplified", "simplified",
                                   "UNSIMPLIFIED", "SIMPLIFIED",
                                   "HPSCU", "HPSCGS", "SSCGS", "SSCU", "CGAZ",
                                   "hpscu", "hpscgs", "sscgs", "sscu", "cgaz"),
                          release_type = c("gbOpen", "gbHumanitarian", "gbAuthoritative"),
                          quiet = TRUE,
                          overwrite = FALSE,
                          version = deprecated()) {

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("0.5",
                              "rgeoboundaries::geoboundaries(version = )")
  }

  type <- match.arg(type)
  type <- tolower(type)

  type <- switch(type,
                 hpscu = "unsimplified",
                 hpscgs = "unsimplified",
                 sscu = "simplified",
                 sscgs = "simplified",
                 cgaz = "cgaz",
                 simplified = "simplified",
                 unsimplified = "unsimplified")

  if (type == "cgaz" & !is.null(country)) {
    warning("'cgaz' type not needed, just use `geoboundaries` or `gb_adm` without `country`",
            call. = FALSE)
    country <- NULL
  }

  release_type <- match.arg(release_type)

  if (is.null(country)) {
    path <- get_cgaz_shp_link(adm_lvl,
                              quiet = quiet,
                              force = overwrite)
    path <- file.path("/vsizip", path)
    res <- read_gb(path,
                   quiet = quiet)
  } else {
    df <- get_adm_shp_link(country = country,
                           adm_lvl = adm_lvl,
                           type = type,
                           release_type = release_type,
                           force = overwrite)
    path <- file.path("/vsizip", df$path)
    res <- read_gb(path, quiet = quiet)
    res <- merge(res,
                 df[,c("shapeGroup", "shapeType", "shapeCanonical")],
                 by = c("shapeGroup", "shapeType"))
  }
  res
}

#' @rdname geoboundaries
#' @export
gb_adm0 <- function(country = NULL, type = NULL, release_type = NULL,
                    quiet = TRUE,
                    overwrite = FALSE,
                    version = deprecated()) {

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("0.5",
                              "rgeoboundaries::gb_adm0(version = )")
  }

  geoboundaries(country = country,
                adm_lvl = "adm0",
                type = type,
                release_type = release_type,
                quiet = quiet,
                overwrite = overwrite)
}

#' @rdname geoboundaries
#' @export
gb_adm1 <- function(country = NULL, type = NULL, release_type = NULL,
                    quiet = TRUE,
                    overwrite = FALSE,
                    version = deprecated()) {

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("0.5",
                              "rgeoboundaries::gb_adm1(version = )")
  }

  geoboundaries(country = country,
                adm_lvl = "adm1",
                type = type,
                release_type = release_type,
                quiet = quiet,
                overwrite = overwrite)
}

#' @rdname geoboundaries
#' @export
gb_adm2 <- function(country = NULL, type = NULL,
                    release_type = NULL,
                    quiet = TRUE,
                    version = deprecated()) {

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("0.5",
                              "rgeoboundaries::gb_adm2(version = )")
  }

  geoboundaries(country = country,
                adm_lvl = "adm2",
                type = type,
                release_type = release_type,
                quiet = quiet)
}

#' @rdname geoboundaries
#' @export
gb_adm3 <- function(country = NULL, type = NULL, release_type = NULL,
                    quiet = TRUE,
                    overwrite = FALSE,
                    version = deprecated()) {

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("0.5",
                              "rgeoboundaries::gb_adm3(version = )")
  }

  geoboundaries(country = country,
                adm_lvl = "adm3",
                type = type,
                release_type = release_type,
                quiet = quiet,
                overwrite = overwrite)
}

#' @rdname geoboundaries
#' @export
gb_adm4 <- function(country = NULL, type = NULL, release_type = NULL,
                    quiet = TRUE,
                    overwrite = FALSE,
                    version = deprecated()) {

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("0.5",
                              "rgeoboundaries::gb_adm4(version = )")
  }

  geoboundaries(country = country,
                adm_lvl = "adm4",
                type = type,
                release_type = release_type,
                quiet = quiet,
                overwrite = overwrite)
}

#' @rdname geoboundaries
#' @export
gb_adm5 <- function(country = NULL, type = NULL, release_type = NULL,
                    quiet = TRUE,
                    overwrite = FALSE,
                    version = deprecated()) {

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("0.5",
                              "rgeoboundaries::gb_adm5(version = )")
  }

  geoboundaries(country = country,
                adm_lvl = "adm5",
                type = type,
                release_type = release_type,
                quiet = quiet,
                overwrite = overwrite)
}
