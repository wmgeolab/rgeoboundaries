% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{gb_metadata}
\alias{gb_metadata}
\title{Get metadata for a country, administrative level, type of data and version}
\usage{
gb_metadata(
  country = NULL,
  adm_lvl = "all",
  release_type = c("gbOpen", "gbHumanitarian", "gbAuthoritative")
)
}
\arguments{
\item{country}{characher; a vector of country names or iso 3 country codes}

\item{adm_lvl}{characher or integer; administrative level, adm0, adm1, adm2, adm3, adm4 or adm5. adm0 being the country. 0, 1, 2, 3, 4 or 5 can also be used.}

\item{release_type}{character; This is one of gbOpen, gbHumanitarian, or gbAuthoritative. For most users, we suggest using gbOpen, as it is CC-BY 4.0 compliant, and can be used for most purposes so long as attribution is provided. gbHumanitarian files are mirrored from UN OCHA, but may have less open licensure. gbAuthoritative files are mirrored from UN SALB, and cannot be used for commerical purposes, but are verified through in-country processes. Default to gbOpen.}
}
\description{
Get metadata for a country and an administrative level, type of data and version
}
