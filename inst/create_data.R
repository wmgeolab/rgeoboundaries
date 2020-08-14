library(tidyverse)
library(janitor)

base_url <- "https://www.geoboundaries.org/data/"
dat_v3 <- read_csv(file.path(base_url, "geoBoundaries-3_0_0", "geoBoundaries-3_0_0.csv")) %>%
  mutate(version = "3_0_0") %>%
  clean_names()

dat_v2 <- read_csv(file.path(base_url, "geoBoundaries-2_0_1", "geoBoundaries-2_0_1.csv")) %>%
  mutate(version = "2_0_1") %>%
  clean_names()

geoboundaries_meta <- bind_rows(dat_v3, dat_v2)
glimpse(geoboundaries_meta)

usethis::use_data()

geoboundaries_meta %>%
  filter(boundary_iso == 'CIV') %>%
  select(boundary_type, download_url) %>%
  as.data.frame()
