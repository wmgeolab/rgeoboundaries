#' Get the administrative boundaries of selected countries
#'
#' @param country_iso3 vector of country iso3
#' @param adm_level a character with desired adminadm0, adm1, adm2 or adm3
#' @param quiet_download a logical value, if TRUE download are quiet. Default to FALSE
#' @return a sf object
#' @export
geoboundaries <- function(country_iso3, adm_level, quiet_download = FALSE) {
  if (length(country_iso3) >= 2) {
    l <- lapply(country_iso3, function(x) get_geoboundaries(x, adm_level, quiet_download = quiet_download))
    res <- do.call(rbind, l)
  } else {
    res <- get_geoboundaries(country_iso3, adm_level, quiet_download = quiet_download)
  }
  res
}
