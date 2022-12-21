library(tidyverse)

# convert individual rds files into a single combined DF containing all results and key metrics

all_years <- 1950:2022

year <- all_years[[1]]


all_data <- map_dfr(all_years,
        ~{
          year <- .x
          
          year_data_list <- read_rds(paste0("year_results_",
                                            year,
                                            ".rds"))
          
          map_dfr(year_data_list,
                  ~{
                    .x %>%
                      mutate(Pos = as.character(Pos)) %>%
                      mutate(Driver=str_remove_all(Driver,"\n"),
                             Driver=str_trim(Driver),
                             Driver=str_squish(Driver))
                  })
        })

write_csv(all_data,
          "all_f1_race_results_21_12_22.csv")
