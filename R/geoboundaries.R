#' Get the administrative boundaries of selected countries
#'
#' Access country boundaries at a specified administrative level
#'
#' @importFrom sf st_read
#' @rdname geoboundaries
#' @param country characher; a vector of country names
#' @param adm_lvl characher; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. admO being the country.
#' @param quiet logical; if TRUE download are quiet. Default to FALSE
#'
#' @details
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
geoboundaries <- function(country, adm_lvl = "adm0", quiet = TRUE) {
  links <- get_download_link(country, adm_lvl)
  shps <- get_shp_from_link(links)
  if (length(country) >= 2) {
    l <- lapply(seq_along(links), function(i)
      sf::st_read(file.path("/vsizip/vsizip/vsicurl", links[i], shps[i]), quiet = quiet))
    res <- do.call(rbind, l)
  } else {
    res <- sf::st_read(file.path("/vsizip/vsizip/vsicurl", links, shps), quiet = quiet)
  }
  res
}

#' @rdname geoboundaries
#' @export
gb_adm0 <- function(country, quiet = TRUE)
  geoboundaries(country, adm_lvl = "adm0")

#' @rdname geoboundaries
#' @export
gb_adm1 <- function(country, quiet = TRUE)
  geoboundaries(country, adm_lvl = "adm1")

#' @rdname geoboundaries
#' @export
gb_adm2 <- function(country, quiet = TRUE)
  geoboundaries(country, adm_lvl = "adm2")

#' @rdname geoboundaries
#' @export
gb_adm3 <- function(country, quiet = TRUE)
  geoboundaries(country, adm_lvl = "adm3")

#' @rdname geoboundaries
#' @export
gb_adm4 <- function(country, quiet = TRUE)
  geoboundaries(country, adm_lvl = "adm4")

#' @rdname geoboundaries
#' @export
gb_adm5 <- function(country, quiet = TRUE)
  geoboundaries(country, adm_lvl = "adm5")
