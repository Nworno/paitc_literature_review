library(readr)
library(dplyr)

dir_data <- "data"

files_BZ <- list.files(file.path(dir_data, "extraction/adjudicated/second_part/BZ"), full.names = TRUE)
second_part_BZ <- bind_rows(lapply(X = files_BZ,
                                   FUN = read_csv2,
                                   col_types = c("ccccccccccccc")))
second_part_BZ %>% write_csv(file.path(dir_data, "extraction/adjudicated/second_part/BZ_raw.csv"))

files_DH <- list.files(file.path(dir_data, "extraction/adjudicated/second_part/DH"),
                       full.names = TRUE,
                       recursive = TRUE,
                       pattern = ".csv")
second_part_DH <- lapply(X = files_DH,
                         FUN = read_csv2,
                         col_types = c("ccccccccccccc")) %>%
  bind_rows()

second_part_DH %>% write_csv(file.path(dir_data, "extraction/adjudicated/second_part/DH_raw.csv"))
