context("geoboundaries: test types and errors")
library(sf)

test_that("type of object returned is as expected", {
  p <- gb_adm0(country = "Mali")
  expect_is(p, "sf")
  expect_true(st_geometry_type(p) %in% c("MULTIPOLYGON", "POLYGON"))
})

test_that("simplified boundaries take less size on memory", {
  p1 <- gb_adm0(country = "Cabo Verde", type = "unsimplified")
  p2 <- gb_adm0(country = "Cabo Verde", type = "simplified")

  expect_gt(object.size(p1),
            object.size(p2))
})

test_that("Downloaded data are cached", {
  gb_clear_cache()
  expect_equal(length(gb_list_cache()), 0)
  p <- gb_adm0(country = "Mali")
  expect_gte(length(gb_list_cache()), 1)
})

test_that("ISO3 also works!", {
  p1 <- gb_adm0(country = "algeria")
  p2 <- gb_adm0(country = "dza")
  expect_equal(p1, p2)
})

test_that("adm_lvl can be any of the characters '0', ..., '5'", {
  p1 <- geoboundaries("benin", adm_lvl = "1")
  p2 <- geoboundaries("benin", adm_lvl = "adm1")
  expect_equal(p1, p2)
})

test_that("adm_lvl can be an integer between 0 and 5", {
  p1 <- geoboundaries("benin", adm_lvl = 0)
  p2 <- geoboundaries("benin", adm_lvl = "adm0")
  expect_equal(p1, p2)
})
