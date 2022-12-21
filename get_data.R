library(tidyverse)
library(rvest)


all_years <- 1950:2022
all_years <- 2022 # update 2022 only

walk(all_years,
     ~{
       Sys.sleep(1)
       print(paste0("Year = ", .x))
       
       year <- .x
       
       # url of all races in a year
       year_url <- paste0("https://www.formula1.com/en/results.html/",
                          year,
                          "/races.html")
       
       html <- read_html(year_url)
       
       # on each year page, there's a table of links to details on the race
       # extract the URLs of each individual race
       html %>%
         html_element("table") %>%
         html_elements("a") %>%
         html_attr("href") -> year_races_urls
       
       # get name of each race so can label outputs to make easier later:
       html %>%
         html_table() %>%
         `[[`(1) %>%
         pull(`Grand Prix`) -> year_race_names
       
       # now visit each race page and get results
       # Keep as list due to some inconsistent use of character vs numeric in source
       # Deal with all of these later distinct from scraping task
       map2(year_races_urls,
            year_race_names,
            ~{
              Sys.sleep(0.5)
              race_url <- .x 
              
              html <- read_html(paste0("https://www.formula1.com/",race_url))
              
              html %>%
                html_table() %>%
                `[[`(1) -> df
              
              # Not all html table cols have a name
              # This doesn't work well with tidyverse
              # so create placeholder names and rename before making a tibble:
              n_cols <- ncol(df)
              place_holder_names <- paste0("col_",1:n_cols)
              
              current_names <- names(df)
              current_names <- if_else(current_names=="",place_holder_names,current_names)
              
              names(df) <- current_names
              
              df %>%
                as_tibble(.names_repair="universal") %>%
                mutate(race=.y) %>%
                mutate(year=.env[["year"]]) %>%
                select(year,race,everything())
              
            }) -> year_results
       
            # Save list of results by year to disk
            # Avoids issues if errors in individual years
       
            write_rds(year_results,paste0("year_results_",year,".rds"))
       
       
     })





# now visit each qualifying page and get results

# then follow links from table - e.g. https://www.formula1.com/en/results.html/2022/races/1124/bahrain/race-result.html
# https://www.formula1.com/en/results.html/2022/races/1124/bahrain/qualifying.html

