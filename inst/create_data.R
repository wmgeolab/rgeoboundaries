library(tidyverse)
library(janitor)

base_url <- "https://www.geoboundaries.org/data/"
dat_v3 <- read_csv(file.path(base_url, "geoBoundaries-3_0_0", "geoBoundaries-3_0_0.csv")) %>%
  mutate(boundaryVersion = "3_0_0")

dat_v2 <- read_csv(file.path(base_url, "geoBoundaries-2_0_1", "geoBoundaries-2_0_1.csv")) %>%
  mutate(boundaryVersion = "2_0_1")

geoboundaries_meta <- bind_rows(dat_v3, dat_v2) %>%
  as.data.frame()

usethis::use_data(geoboundaries_meta, internal = TRUE, overwrite = TRUE)
