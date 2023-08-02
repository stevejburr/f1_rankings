# Libraries ---------------------------------------------------------------
library(tidyverse)
library(brms)
library(emmeans)
library(ggrepel)
library(marginaleffects)

windowsFonts(Raleway="Raleway")
# Data Read ---------------------------------------------------------------

# Read in CSV of all results data
all_data <- read_csv("all_f1_race_results_21_12_22.csv")



# Read in preferred model -
model_4 <- read_rds("model_4")


# Identify year of drivers career -

all_data %>%
  group_by(Driver) %>%
  summarise(first_year=min(year)) -> first_years

all_data %>%
  inner_join(first_years,by="Driver") %>%
  mutate(career_year=1+(year-first_year)) -> all_data

# Peak Performance - Top Drivers ------------------------------------------

all_data %>%
  group_by(Driver) %>%
  summarise(races=n()) %>%
  filter(races>=20) %>%
  pull(Driver) -> drivers_twenty_races

all_data %>%
  filter(Driver %in% drivers_twenty_races) %>%
  group_by(Driver) %>%
  distinct(career_year) %>%
  crossing(Car=NA,year_chr=NA) -> new_data



preds <- posterior_predict(model_4,newdata = new_data,allow_new_levels=TRUE)

new_data$predicted_points <- (preds %>%colMeans())
new_data$predicted_points_upr <- apply(preds,
                                              MARGIN=2,
                                              FUN=function(x){quantile(x,0.95)})

new_data$predicted_points_lwr <- apply(preds,
                                              MARGIN=2,
                                              FUN=function(x){quantile(x,0.05)})


new_data %>%
  group_by(Driver) %>%
  summarise(peak_points=max(predicted_points),
            years_raced=max(career_year)) %>%
  arrange(-peak_points) %>%
  head(20) -> peak_performance_data 

peak_performance_data %>%
  write_csv("peak_performance_data.csv")

peak_performance_data %>%
  mutate(Driver=as.factor(Driver),
         Driver=fct_reorder(Driver,peak_points)) %>%
  ggplot(aes(x=peak_points,
             y=Driver,
             group=Driver))+
  theme_minimal(base_family="Raleway")+
  geom_point(colour="#9b0d0d") +
  labs(x="Predicted mean points scored per race in average car - best season",
       y="",
       title="") 



# Average Performance Career ----------------------------------------------

new_data %>%
  group_by(Driver) %>%
  summarise(average_points_per_race=mean(predicted_points),
            best_season_points_per_race=max(predicted_points),
            worst_season_points_per_race=min(predicted_points)) %>%
  arrange(-average_points_per_race) %>%
  head(20) -> mean_performance_data

mean_performance_data %>%
  write_csv("mean_performance_data.csv")

mean_performance_data %>%
  mutate(Driver=as.factor(Driver),
         Driver=fct_reorder(Driver,average_points_per_race)) %>%
  ggplot(aes(y=Driver,
             x=average_points_per_race,
             group=Driver))+
  theme_minimal(base_family="Raleway")+
  geom_pointrange(aes(xmin=worst_season_points_per_race,
                      xmax=best_season_points_per_race),
                  colour="#9b0d0d")+
  labs(x="Predicted mean points scored per race in average car",
       y="",
       subtitle="Point is average season, range is best and worst season") 


new_data %>%
  filter(Driver %in% c("Jim Clark CLA",
                       "Alain Prost PRO",
                       "Juan Manuel Fangio FAN",
                       "Lewis Hamilton HAM",
                       "Fernando Alonso ALO",
                       #"Max Verstappen VER",
                       "Michael Schumacher MSC",
                       #"Ayrton Senna SEN"
                       NULL)) -> key_driver_trends

key_driver_trends %>%
  group_by(Driver) %>%
  filter(career_year==max(career_year)) %>%
  select(Driver,career_year,predicted_points) -> key_driver_trend_labels


key_driver_trends %>%
  ggplot(aes(y=predicted_points,
             x=career_year,
             colour=Driver,
             group=Driver))+
  geom_line(show.legend=FALSE) +
  geom_text_repel(data=key_driver_trend_labels,
                  aes(label=Driver),
                  show.legend = FALSE) +
  theme_minimal(base_family="Raleway") +
  theme(panel.grid = element_blank()) +
  labs(x="Year of Career",
       y= "Average predicted points per race in average car")



# Best 5 years of career --------------------------------------------------


new_data %>%
  group_by(Driver) %>%
  arrange(Driver,desc(predicted_points)) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=5) %>%
  group_by(Driver) %>%
  summarise(average_points_per_race=mean(predicted_points),
            best_season_points_per_race=max(predicted_points),
            worst_season_points_per_race=min(predicted_points)) %>%
  arrange(-average_points_per_race) %>%
  head(20) -> mean_performance_data_best_5




mean_performance_data_best_5 %>%
  write_csv("mean_performance_data_best_5.csv")

mean_performance_data_best_5 %>%
  mutate(Driver=as.factor(Driver),
         Driver=fct_reorder(Driver,average_points_per_race)) %>%
  ggplot(aes(y=Driver,
             x=average_points_per_race,
             group=Driver))+
  theme_minimal(base_family="Raleway")+
  geom_pointrange(aes(xmin=worst_season_points_per_race,
                      xmax=best_season_points_per_race),
                  colour="#9b0d0d")+
  labs(x="Predicted mean points scored per race in average car",
       y="",
       subtitle="Point is average season, range is best and worst season across best 5 years") 




# Overlapping drivers -----------------------------------------------------


overlap_drivers <- c("Carlos Sainz SAI",
  "Lando Norris NOR",
"Charles Leclerc LEC",
"Sebastian Vettel VET",
"Max Verstappen VER",
"Daniel Ricciardo RIC"
)

all_data %>%
  filter(Driver %in% overlap_drivers) %>%
  distinct(Driver,career_year,year) -> overlap_driver_years

new_data %>%
  filter(Driver %in% overlap_drivers) -> overlap_driver_trends

overlap_driver_trends %>%
  left_join(overlap_driver_years) -> overlap_driver_trends

write_csv(overlap_driver_trends,"overlap_driver_trends.csv")


overlap_driver_trends %>%
  group_by(Driver) %>%
  filter(career_year==max(career_year)) %>%
  select(Driver,career_year,predicted_points) -> overlap_driver_trends_label

overlap_driver_trends %>%
  group_by(Driver) %>%
  filter(year==max(year)) %>%
  select(Driver,year,predicted_points) -> overlap_driver_trends_label_years

overlap_driver_trends %>%
  ggplot(aes(y=predicted_points,
             x=career_year,
             colour=Driver,
             group=Driver))+
  geom_line(show.legend=FALSE) +
  geom_text_repel(data=overlap_driver_trends_label,
                  aes(label=Driver),
                  show.legend = FALSE) +
  theme_minimal(base_family="Raleway") +
  theme(panel.grid = element_blank()) +
  labs(x="Year of Career",
       y= "Average predicted points per race in average car")

overlap_driver_trends %>%
  ggplot(aes(y=predicted_points,
             x=year,
             colour=Driver,
             group=Driver))+
  geom_line(show.legend=FALSE) +
  geom_text_repel(data=overlap_driver_trends_label_years,
                  aes(label=Driver),
                  show.legend = FALSE,
                  direction="x") +
  theme_minimal(base_family="Raleway") +
  theme(panel.grid = element_blank()) +
  labs(x="Year of Career",
       y= "Average predicted points per race in average car") +
  xlim(c(NA,2025))



# do in career_year and do in actual years  
