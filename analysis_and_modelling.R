# Libraries ---------------------------------------------------------------
library(tidyverse)
library(brms)
library(emmeans)
library(ggrepel)
library(marginaleffects)

# Data Read ---------------------------------------------------------------

# Read in CSV of all results data
all_data <- read_csv("all_f1_race_results_21_12_22.csv")


# Quick Analysis ----------------------------------------------------------

# Quick sense check looking at wins and appearances vs known figures:
all_data %>%
  filter(Pos=="1") %>%
  group_by(Driver) %>%
  summarise(wins=n()) %>%
  arrange(-wins)

# ~ roughly matches wikipedia list (some differences on what GP/F1 race includes etc)


all_data %>%
  group_by(Driver) %>%
  summarise(races=n()) %>%
  arrange(-races)

# ~ roughly matches wikipedia list (some differences on what GP/F1 race includes etc)

# Assume compelte results gathered


# Define Points -----------------------------------------------------------

# Want credit for finishes (for team scoring)
# Find all `Time/Retired` values which count as finishing

unique(all_data$`Time/Retired`) %>%
  keep(~!is.na(.x)) %>%
  keep(~!str_detect(.x,"[0-9]+"))

# OK 
all_data %>%
  filter(`Time/Retired`=="SHC")

all_data %>%
  mutate(finished=if_else(`Time/Retired` %in% c("DNF","DSQ","DNC","DNS") |
                            `Pos`=="NC",0,1)) %>%
  mutate(pos_points=as.numeric(Pos),
         pos_points=if_else(pos_points<=10,11-pos_points,0),
         pos_points=replace_na(pos_points,0),
         total_points=finished+pos_points) -> all_data

all_data %>%
  mutate(year_chr=as.character(year)) -> all_data

# Identify year of drivers career -

all_data %>%
  group_by(Driver) %>%
  summarise(first_year=min(year)) -> first_years

all_data %>%
  inner_join(first_years,by="Driver") %>%
  mutate(career_year=1+(year-first_year)) -> all_data

# compare driver total PTS (real points) vs "total_points"

all_data %>%
  group_by(Driver) %>%
  summarise(PTS=sum(PTS),
            pos_points=sum(pos_points),
            total_points=sum(total_points)) %>%
  arrange(-pos_points)

all_data %>%
  group_by(Driver) %>%
  summarise(PTS=sum(PTS),
            pos_points=sum(pos_points),
            total_points=sum(total_points),
            races=n()) %>%
  mutate(total_points_per_race=total_points/races) %>%
  arrange(-total_points_per_race) %>%
  print(n=30)


all_data %>%
  group_by(Driver, career_year) %>%
  summarise(total_points=sum(total_points),
            races=n()) %>%
  mutate(total_points_per_race=total_points/races) %>%
  arrange(-total_points_per_race) %>%
  print(n=50)

all_data %>%
  group_by(Driver, career_year) %>%
  summarise(total_points=sum(total_points),
            races=n()) %>%
  mutate(total_points_per_race=total_points/races) %>%
  filter(Driver %in% c("Lewis Hamilton HAM",
                       "Sebastian Vettel VET",
                       "Michael Schumacher MSC")) %>%
  arrange(Driver,career_year) %>%
  ggplot(aes(x=career_year,y=total_points_per_race,colour=Driver)) +
  geom_point()+
  geom_smooth()+
  theme_minimal()


# Fit Models -------------------------------------------------------------

# First, fit a poisson model by driver

model_1 <- brm(total_points ~ 1 + (1|Driver),
               data=all_data,
               chains=4,
               cores=4,
               refresh=100,
               backend="cmdstanr",
               family="poisson")

#saveRDS(model_1,"model_1.rds")
model_1 <- read_rds("model_1.rds")

coef_model_1 <- coef(model_1)

as.data.frame(coef_model_1$Driver) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  arrange(-Estimate.Intercept) %>%
  print(n=100)
# driver only model - hamilton the best -


# Driver / team*year
model_2 <- brm(total_points ~ 1 + (1|Driver) + (1 | Car:year_chr),
               data=all_data,
               chains=4,
               cores=4,
               refresh=100,
               backend="cmdstanr",
               family="poisson")
#saveRDS(model_2,"model_2.rds")

model_2 <- read_rds("model_2.rds")

coef_model_2 <- coef(model_2)

as.data.frame(coef_model_2$Driver) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  arrange(-Estimate.Intercept) %>%
  print(n=100)

# worst drivers...
as.data.frame(coef_model_2$Driver) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  arrange(Estimate.Intercept) %>%
  print(n=100)

# best cars
as.data.frame(coef_model_2$`Car:year_chr`) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  arrange(-Estimate.Intercept) %>%
  print(n=100)

# controlling for car/year and driver, hamilton much lower...

# Driver * career_year / team*year

model_3 <- brm(total_points ~ 1 + s(career_year, bs="bs") +
                 (1 |Driver) +
                 (1 | Car:year_chr),
               data=all_data,
               chains=4,
               cores=4,
               refresh=100,
               backend="cmdstanr",
               family="poisson")
#saveRDS(model_3,"model_3.rds")
model_3 <- read_rds("model_3.rds")

m3_coefs <- coef(model_3)
#saveRDS(m3_coefs,"m3_coefs.rds")

m3_coefs <- read_rds("m3_coefs.rds")

as.data.frame(m3_coefs$`Car:year_chr`) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  arrange(-Estimate.Intercept) %>%
  print(n=50)

as.data.frame(m3_coefs$Driver) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  arrange(-Estimate.Intercept) %>%
  print(n=100)

# m3_fixed <- fixed(model_3)
# saveRDS(m3_fixed,"m3_fixed.rds")

plot(conditional_smooths(model_3), rug = TRUE, ask = FALSE)


all_data %>%
  group_by(Driver) %>%
  summarise(races=n()) %>%
  filter(races>=25) -> drivers_25

as.data.frame(m3_coefs$Driver) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  inner_join(drivers_25,by=c("rowname"="Driver")) %>%
  arrange(-Estimate.Intercept) %>%
  head(25) %>%
  mutate(rowname=factor(rowname),
         rowname=fct_reorder(rowname,Estimate.Intercept)) %>%
  ggplot(aes(y=rowname,
             xmin=Q2.5.Intercept,
             xmax=Q97.5.Intercept,
             x=Estimate.Intercept)) +
  geom_pointrange()+
  theme_minimal()
  
# squared driver career_year at a total and driver level

model_4 <- brm(total_points ~ 1 + poly(career_year,2) +
                 (1 + poly(career_year,2)|Driver) +
                 (1 | Car:year_chr),
               data=all_data,
               chains=4,
               cores=4,
               refresh=100,
               backend="cmdstanr",
               family="poisson")
saveRDS(model_4,"model_4.rds")
#model_4 <- read_rds("model_4.rds")

model_4_coefs <- coef(model_4)

as.data.frame(model_4_coefs$Driver) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  inner_join(drivers_25,by=c("rowname"="Driver")) %>%
  arrange(-Estimate.Intercept) %>%
  head(35) %>%
  mutate(rowname=factor(rowname),
         rowname=fct_reorder(rowname,Estimate.Intercept)) %>%
  ggplot(aes(y=rowname,
             xmin=Q2.5.Intercept,
             xmax=Q97.5.Intercept,
             x=Estimate.Intercept)) +
  geom_pointrange()+
  theme_minimal()


#model 4 looks good?
# prediction curves by driver (for key current ones)
summary(model_4)

# probably need to show / compare "average level" over a n-year career
# vs intercept by driver

# cubic driver career_year at a total and driver level

model_5 <- brm(total_points ~ 1 + poly(career_year,3) +
                 (1 + poly(career_year,3)|Driver) +
                 (1 | Car:year_chr),
               data=all_data,
               chains=4,
               cores=4,
               refresh=100,
               backend="cmdstanr",
               family="poisson")
saveRDS(model_5,"model_4.rds")
#model_5 <- read_rds("model_4.rds")



# Model Analysis ----------------------------------------------------------


model_4 <- read_rds("model_4.rds")

unique(all_data$Driver[str_detect(all_data$Driver,"Prost")])
  
"Lewis Hamilton HAM"
"Michael Schumacher MSC"
"Lando Norris NOR"
"Charles Leclerc LEC"
"Sebastian Vettel VET"
"Max Verstappen VER"
"Nikita Mazepin MAZ"
"Pastor Maldonado MAL"
"Nicholas Latifi LAT"
"Fernando Alonso ALO"
"Ayrton Senna SEN"
"Alain Prost PRO"

model_4$formula

new_data <- crossing(Driver=c("Lewis Hamilton HAM",
                              "Michael Schumacher MSC",
                              "Lando Norris NOR",
                              "Charles Leclerc LEC",
                              "Sebastian Vettel VET",
                              "Max Verstappen VER",
                              # "Nikita Mazepin MAZ",
                              # "Pastor Maldonado MAL",
                              # "Nicholas Latifi LAT",
                              "Fernando Alonso ALO",
                              "Ayrton Senna SEN",
                              "Alain Prost PRO"),
                   Car=NA,
                   year_chr=NA,
                   career_year = 1:10)

preds <- posterior_predict(model_4,newdata = new_data,allow_new_levels=TRUE)

new_data$predicted_points <- (preds %>%colMeans())

# Validate how this works - find all car/year values lewis hamilton could have driven
# predict these for him
all_data %>%
  filter(Driver=="Lewis Hamilton HAM") %>%
  distinct(year_chr) %>%
  inner_join(all_data) %>%
  distinct(Car,year_chr) %>%
  crossing(Driver="Lewis Hamilton HAM",
           career_year = 1:10) -> ham_preds

ham_preds_check <- posterior_predict(model_4,newdata = ham_preds,allow_new_levels=TRUE)
?posterior_epred()
ham_preds$predicted_points <- (ham_preds_check %>%colMeans())

ham_preds %>%
  group_by(Driver, career_year) %>%
  summarise(predicted_points_check=mean(predicted_points)) %>%
  left_join({
    new_data %>%
      filter(Driver=="Lewis Hamilton HAM")
  })




new_data %>%
  filter(career_year==10) -> driver_labels

all_data %>%
  filter(Driver %in% c("Lewis Hamilton HAM",
                       "Michael Schumacher MSC",
                       "Lando Norris NOR",
                       "Charles Leclerc LEC",
                       "Sebastian Vettel VET",
                       "Max Verstappen VER",
                       # "Nikita Mazepin MAZ",
                       # "Pastor Maldonado MAL",
                       # "Nicholas Latifi LAT",
                       "Fernando Alonso ALO",
                       "Ayrton Senna SEN",
                       "Alain Prost PRO")) %>%
  group_by(year_chr,Car,Driver,career_year) %>%
  summarise(total_points=mean(total_points)) -> observed_points

preds_observed <- posterior_predict(model_4,newdata = observed_points)

observed_points$predicted_points <- (preds_observed %>%colMeans())
  
observed_points$predicted_points_upr <- apply(preds_observed,
      MARGIN=2,
       FUN=function(x){quantile(x,0.95)})

observed_points$predicted_points_lwr <- apply(preds_observed,
                                              MARGIN=2,
                                              FUN=function(x){quantile(x,0.05)})

plot_driver_real_vs_predicted <- function(driver){
  observed_points %>%
    filter(Driver==driver) %>%
    ggplot(aes(x=career_year))+
    geom_point(aes(y=total_points),
               shape=21,
               colour="red")+
    geom_pointrange(aes(ymin=predicted_points_lwr,
                        y=predicted_points,
                        ymax=predicted_points_upr)) +
    theme_minimal() +
    labs(title="Predicted vs actual points per race",
         subtitle=driver)
}

plot_driver_real_vs_predicted("Lewis Hamilton HAM")
plot_driver_real_vs_predicted("Michael Schumacher MSC")
plot_driver_real_vs_predicted("Fernando Alonso ALO")



plot_drivers_real_vs_predicted <- function(drivers){
  observed_points %>%
    filter(Driver %in% drivers) %>%
    ggplot(aes(x=career_year))+
    facet_wrap(Driver ~.) +
    geom_point(aes(y=total_points),
               shape=21,
               colour="red")+
    geom_pointrange(aes(ymin=predicted_points_lwr,
                        y=predicted_points,
                        ymax=predicted_points_upr)) +
    theme_minimal() +
    labs(title="Predicted vs actual points per race")
}

plot_drivers_real_vs_predicted(c("Lewis Hamilton HAM",
                                 "Michael Schumacher MSC",
                                 "Sebastian Vettel VET",
                                 "Ayrton Senna SEN",
                                 "Alain Prost PRO",
                                 "Fernando Alonso ALO"))


new_data %>%
  ggplot() +
  geom_line(aes(x=career_year,y=predicted_points,colour=Driver),
            show.legend = FALSE) +
  geom_text_repel(data=driver_labels,
            aes(x=career_year,
                y=predicted_points,
                colour=Driver,
                label=Driver),
            hjust="left",
            direction="y",
            show.legend = FALSE)+
  coord_cartesian(xlim=c(0,15))+
  theme_minimal()



# Repeat this with no extrapolation -

all_data %>%
  group_by(Driver) %>%
  summarise(max_career = max(career_year)) %>%
  inner_join(new_data) %>%
  filter(career_year <= max_career) %>%
  ggplot() +
  geom_line(aes(x=career_year,y=predicted_points,colour=Driver),
            show.legend = FALSE) +
  geom_text_repel(data=driver_labels,
                  aes(x=career_year,
                      y=predicted_points,
                      colour=Driver,
                      label=Driver),
                  hjust="left",
                  direction="y",
                  show.legend = FALSE)+
  coord_cartesian(xlim=c(0,15))+
  theme_minimal()
  

# understand car distributions by year (points)

data_for_cars <- all_data %>%
  distinct(Car, year_chr) %>%
  crossing(career_year=5,
           Driver=NA)

preds_cars <- posterior_predict(model_4,
                           newdata = data_for_cars,
                           re_formula= ~ (1 | Car:year_chr),
                           allow_new_levels=TRUE,
                           ncores=4)

data_for_cars$av_points <- colMeans(preds_cars)

data_for_cars %>%
  ggplot()+
  geom_histogram(aes(x=av_points))+
  theme_minimal() +
  labs(title="All time distribution of team expected points")


data_for_cars %>%
  filter(year_chr=="2022") %>%
  arrange(-av_points)

all_data %>%
  filter(year_chr=="2022") %>%
  group_by(Car) %>%
  summarise(real_points=sum(PTS),
            total_points=sum(total_points)) %>%
  arrange(-real_points)

# why does Alpine look so good in 2022?


data_for_cars %>%
  filter(year_chr=="2021") %>%
  arrange(-av_points)

all_data %>%
  filter(year_chr=="2021") %>%
  group_by(Car) %>%
  summarise(real_points=sum(PTS),
            total_points=sum(total_points)) %>%
  arrange(-real_points)



data_for_cars %>%
  filter(year_chr=="2020") %>%
  arrange(-av_points)

data_for_cars %>%
  arrange(-av_points) %>%
  print(n=30)

data_for_cars %>%
  filter(year_chr=="2012") %>%
  arrange(-av_points)

# understand driver distributions by year (points)


data_for_drivers <- all_data %>%
  distinct(Driver, year_chr, career_year) %>%
  crossing(Car=NA)

preds_drivers <- posterior_predict(model_4,
                                newdata = data_for_drivers,
                                re_formula= ~ (1 + poly(career_year, 2) | Driver),
                                allow_new_levels=TRUE,
                                ncores=4)

data_for_drivers$av_points <- colMeans(preds_drivers)

data_for_drivers %>%
  filter(year_chr=="2022") %>%
  arrange(-av_points) %>%
  print(n=25)

# Alonso curve probably too extreme down?
# Hence have to make Alpine v. good to compensate...
new_data_alonso <- crossing(Driver=c("Fernando Alonso ALO"),
                     Car=NA,
                     year_chr=NA,
                     career_year = 1:22)

preds_alonso <- posterior_predict(model_4,
                                  newdata = new_data_alonso,
                                  allow_new_levels=TRUE,
                                  ncores=4)

new_data_alonso$predicted_points <- (preds_alonso %>%colMeans())

new_data_alonso %>%
  ggplot()+
  geom_line(aes(x=career_year,
                y=predicted_points)) +
  theme_minimal()



# understand average driver trends and interesting variations
# average curve and/or spaghetti plot of all drivers
# do these in "average car" - use emmeans for that?!

data_for_drivers %>%
  group_by(career_year) %>%
  summarise(av_points=mean(av_points)) %>%
  mutate(Driver="Average") -> average_drive_traj

data_for_drivers %>%
  group_by(Driver) %>%
  mutate(total_years=n()) %>%
  filter(total_years>=5) %>%
  ggplot()+
  geom_line(aes(x=career_year,
                y=av_points,
                group=Driver),
            alpha=0.2) +
  geom_line(data=average_drive_traj,
            aes(x=career_year,
                y=av_points,
                group=Driver),
            colour="red")+
  theme_minimal()

# Index averages vs year 1

data_for_drivers %>%
  group_by(Driver) %>%
  mutate(total_years=n()) %>%
  filter(total_years>=5) %>%
  mutate(first_year=if_else(career_year==1,
                            av_points,
                            -Inf),
         first_year=max(first_year),
         av_points_idx = av_points/first_year) -> driver_idx

driver_idx  %>%
  group_by(career_year) %>%
  summarise(av_points_idx=mean(av_points_idx)) %>%
  mutate(Driver="Average") -> average_drive_idx_traj

driver_idx %>%
  ggplot()+
  geom_line(aes(x=career_year,
                y=av_points_idx,
                group=Driver),
            alpha=0.2) +
  geom_line(data=average_drive_idx_traj,
            aes(x=career_year,
                y=av_points_idx,
                group=Driver),
            colour="red")+
  theme_minimal()



# take 2019/2020/2021/2022
# Look at different drivers in different cars...
# which car / driver was best in 2021?

# to compare by year
# average driver in this car
# this driver in the average car (for this year)
# overall prediction for this driver in this year

# for all drivers and actual seasons
# get "average car points" - across career summarise min/max/med values
# rank and display

# for each season
# calculate expected "average car" driver points
# sum "total_points" "predicted_points" and "actual points"
# compare number of career championships and differences

# Alonso 2012

# Compare by real year, the driver ratings of
# RIC/VES/SAI/LEC/VET as all have overlapped (and show teams?)


# Driver career contributions vs Driver car contributions to points
# Who has benefitted most from their car?


# Create Visuals / Tables -------------------------------------------------
# compare predicted (epred/emmeans (see andrew blog)
# curves by driver vs empirical ones
# Best drivers
# filter people out
# explain/build contribution to points in a season using CAR+DRIVER
# show over/under performance vs expectation vs observed!

# Best teams

# Average variation by driver and career_year

# Individual career_year for recent/current/historic drivers:
# HAM/VET/ALO/RIK/RIC/MSC/SEN


# Simulation of recent seasons
# VET/HAM - HAm/VES - HAM/ROS - 2022
