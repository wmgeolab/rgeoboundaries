context("gb_countries: test types and errors")
library(sf)

test_that("type of object returned is as expected", {
  skip_on_cran()
  p <- gb_adm0(country = "Mali")
  expect_is(p, "sf")
  expect_true(st_geometry_type(p) %in% c("MULTIPOLYGON", "POLYGON"))
})

test_that("simplified boundaries take less size on memory", {
  skip_on_cran()
  p1 <- gb_adm0(country = "Cabo Verde", type = "hpscu")
  p2 <- gb_adm0(country = "Cabo Verde", type = "sscu")

  expect_gt(object.size(p1),
            object.size(p2))
})