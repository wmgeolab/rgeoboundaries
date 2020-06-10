#' Get the administrative boundaries of selected countries
#'
#' Access country boundaries at a specified administrative level
#'
#' @importFrom sf st_read
#' @rdname geoboundaries
#' @param country characher; a vector of country names
#' @param adm_lvl characher; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. admO being the country.
#' @param type character; defaults to HPSCU. One of HPSCU, HPSCGS, SSCGS, or SSCU. Determines the type of boundary link you receive. More on details
#' @param version character; defaults to the most recent version of geoBoundaries available. The geoboundaries version requested, with underscores.
#' For example, 3_0_0 would return data from version 3.0.0 of geoBoundaries.
#' @param quiet logical; if TRUE download are quiet. Default to FALSE
#'
#' @details
#' Different types to of boundaries available:
#'
#' * HPSCU - High Precision Single Country Unstadardized. The premier geoBoundaries release,
#'   representing the highest precision files available for every country in the world.
#'   No standardization is performed on these files, so (for example) two countries may overlap in the case of contested boundaries.
#'
#' * HPSCGS - High Precision Single Country Globally Standardized. A version of geoBoundaries
#'   high precision data that has been clipped to the U.S. Department of State boundary file,
#'   ensuring no contested boundaries or overlap in the dataset. This globally standardized
#'   product may have gaps between countries. If you need a product with no gaps,
#'   we recommend our simplified global product.
#'
#' * SSCU - Simplified Single Country Unstandardized. A simplified version of every file
#'   available for every country in the world. No standardization is performed on these files,
#'   so (for example) two countries may overlap in the case of contested boundaries.
#'
#' * SSCGS - Simplified Single Country Globally Standardized. A version of geoBoundaries
#'   simplified data that has been clipped to the U.S. Department of State boundary file,
#'   ensuring no contested boundaries or overlap in the dataset.
#'   This globally standardized product may have gaps between countries.
#'
#' The following wrappers are available:
#'
#' * `gb_adm0` returns the country boundaries
#' * `gb_adm1` if available, returns the country first administrative level boundaries
#' * `gb_adm2` if available, returns the country second administrative level boundaries
#' * `gb_adm3` if available, returns the country third administrative level boundaries
#' * `gb_adm4` if available, returns the country fourth administrative level boundaries
#' * `gb_adm5` if available, returns the country first administrative level boundaries
#'
#'
#' @return a `sf` object
#'
#' @export
geoboundaries <- function(country, adm_lvl = "adm0", type = NULL, version = NULL, quiet = TRUE) {
  links <- get_download_links(country = country,
                              adm_lvl = adm_lvl,
                              type = type,
                              version = version)
  shps <- get_shp_from_links(links)
  path <- paste0("/vsizip/", shps)
  if (length(country) >= 2) {
    l <- lapply(seq_along(links), function(i)
      sf::st_read(path[i], quiet = quiet))
    res <- do.call(rbind, l)
  } else {
    res <- sf::st_read(path, quiet = quiet)
  }
  res
}

#' @rdname geoboundaries
#' @export
gb_adm0 <- function(country, type = NULL, version = NULL,  quiet = TRUE)
  geoboundaries(country = country,
                adm_lvl = "adm0",
                type = type,
                version = version,
                quiet = quiet)

#' @rdname geoboundaries
#' @export
gb_adm1 <- function(country, type = NULL, version = NULL, quiet = TRUE)
  geoboundaries(country = country,
                adm_lvl = "adm1",
                type = type,
                version = version,
                quiet = quiet)

#' @rdname geoboundaries
#' @export
gb_adm2 <- function(country, type = NULL, version = NULL, quiet = TRUE)
  geoboundaries(country = country,
                adm_lvl = "adm2",
                type = type,
                version = version,
                quiet = quiet)

#' @rdname geoboundaries
#' @export
gb_adm3 <- function(country, type = NULL, version = NULL, quiet = TRUE)
  geoboundaries(country = country,
                adm_lvl = "adm3",
                type = type,
                version = version,
                quiet = quiet)

#' @rdname geoboundaries
#' @export
gb_adm4 <- function(country, type = NULL, version = NULL, quiet = TRUE)
  geoboundaries(country = country,
                adm_lvl = "adm4",
                type = type,
                version = version,
                quiet = quiet)

#' @rdname geoboundaries
#' @export
gb_adm5 <- function(country, type = NULL, version = NULL, quiet = TRUE)
  geoboundaries(country = country,
                adm_lvl = "adm5",
                type = type,
                version = version,
                quiet = quiet)
