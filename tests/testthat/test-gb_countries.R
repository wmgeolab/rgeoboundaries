context("gb_countries: test types and errors")
library(sf)
p <- gb_adm0(country = "Mali")

test_that("type of object returned is as expected", {
  expect_is(p, "sf")
  expect_true(st_geometry_type(p) %in% c("MULTIPOLYGON", "POLYGON"))
})