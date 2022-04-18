#on line 518 add created by Estelle Ou
wd <- "D:/Estelle/Rscripts/covid_monitoring"

library(tidyverse)
library(lubridate)
library(zoo)
library(testit)
library(logr)

source("D:/Estelle/Rscripts/estelle_theme.R")
tmp <- file.path("data_cleaning.log")

#raw data ---------------------------------------------------------------------

#cases and vaccines
covid_data <- 
  read_csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))

#mobility data
urlfile="https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_countries.csv"
mobility<-read_csv(url(urlfile))
save(mobility, file = "cleaned_data/mobility.rda")

lf <- log_open(tmp)

#data cleaning ---------------------------------------------------------------

#Mobility data cleaning 
cntry_cleaned_mobility <-
  #creating country-level mobility data
  mobility %>%
  #taking the country total numbers and getting rid of county/city-level data
  filter(region == "Total") %>%
  #this is your index calculation
  mutate(index = (`retail and recreation` + `transit stations` + workplaces - residential)/4) %>%
  group_by(country) %>%
  mutate(roll_index = rollmean(index, k=7, align = "right", fill = NA)) %>% 
  ungroup()

save(cntry_cleaned_mobility, file = "cleaned_data/cntry_cleaned_mobility.rda")

#COVID data cleaning 
cntry_cleaned_covid <-
  covid_data %>% 
  select(date, location, total_cases, new_cases, total_deaths, new_deaths, icu_patients, new_cases_per_million, new_deaths_per_million,
         hosp_patients, weekly_hosp_admissions, weekly_icu_admissions, total_tests,
         new_tests, total_vaccinations, people_vaccinated, new_vaccinations, people_fully_vaccinated,
         stringency_index, population) %>% 
  rename(country = location) %>% 
  #smoothing out by creating 7-day rolling average
  group_by(country) %>% 
  fill(new_cases, .direction = "down") %>%
  fill(new_deaths, .direction = "down") %>%
  fill(new_vaccinations, .direction = "down") %>%
  fill(people_fully_vaccinated, .direction = "down") %>% 
  mutate(new_cases_avg = rollmean(new_cases, k= 7, align = "right", fill = NA),
         new_deaths_avg =rollmean(new_deaths, k = 7, align = "right", fill = NA),
         weekly_hosp_admin_avg = rollmean(weekly_hosp_admissions, k = 7, align = "right", fill = NA),
         weekly_icu_admin_avg = rollmean(weekly_icu_admissions, k = 7, align = "right", fill = NA),
         new_vac_avg = rollmean(new_vaccinations , k = 7, align = "right", fill = NA)) %>% 
  select(-new_cases, -new_deaths, -weekly_hosp_admissions, -weekly_icu_admissions, -new_vaccinations) %>%
  mutate( total_cases_per_pop = (total_cases/population),
          total_vax_per_pop = (total_vaccinations/population)*100,
          fully_vaxed_per_pop = (people_fully_vaccinated/population)*100,
          new_vax_avg_per_pop = ((new_vac_avg/population)*100),
          population = population/1000000,
          new_deaths_avg_per_pop = (new_deaths_avg/population),
          new_cases_avg_per_pop = (new_cases_avg/population)) %>% 
  mutate(new_cases_avg_per_pop = ifelse(new_cases_avg_per_pop <0, 0, new_cases_avg_per_pop),
         new_deaths_avg_per_pop = ifelse(new_deaths_avg_per_pop <0, 0, new_deaths_avg_per_pop)) %>% 
  ungroup()

save(cntry_cleaned_covid, file = "cleaned_data/cntry_cleaned_covid.rda")

#data set of speed of cases and deaths --------------------------------------
increases_in_deaths_and_cases_within_this_week <- 
  cntry_cleaned_covid %>% 
  group_by(country) %>% 
  mutate(change_7day_avg_new_cases_per_pop = new_cases_avg_per_pop-lag(new_cases_avg_per_pop,1),
         change_7day_avg_new_deaths_per_pop = new_deaths_avg_per_pop-lag(new_deaths_avg_per_pop,1),
         avg_chg_weeklycases = rollmean(change_7day_avg_new_cases_per_pop, k = 14, align = "right", fill = NA),
         avg_chg_weeklydeaths = rollmean(change_7day_avg_new_deaths_per_pop, k = 14, align = "right", fill = NA),) %>% 
  ungroup() 

save(increases_in_deaths_and_cases_within_this_week, file = "cleaned_data/increases_in_deaths_and_cases_within_this_week.rda")


#regional covid cases data set ------------------------------------------------
  
  region_cases <-
  covid_data %>% 
  select(date, location, continent,new_cases, new_deaths) %>% 
  rename(country = location) %>%
  #smoothing out by creating 7-day rolling average
  group_by(country) %>% 
  fill(new_cases, .direction = "down") %>%
  fill(new_deaths, .direction = "down") %>%
  ungroup() %>% 
  select(-country) %>% 
  filter(date >= "2020-02-24") %>% 
  unique() %>% 
  group_by(date, continent) %>%
  mutate(new_cases = sum(new_cases, na.rm = T)) %>% 
  mutate(new_deaths = sum(new_deaths, na.rm = T)) %>% 
  unique() %>% 
  arrange(date) %>% 
  ungroup() %>% 
  group_by(continent) %>% 
  mutate(new_cases_avg = rollmean(new_cases, k= 7, align = "right", fill = NA)) %>% 
  mutate(new_deaths_avg = rollmean(new_deaths, k= 7, align = "right", fill = NA)) 

#regional population data ----------------------------------------------------

region_pop <-
  covid_data %>% 
  select(date, continent,population) %>% 
  arrange(date)  %>% 
  #taking data from two days ago because sometimes data isn't update for every country on time
  filter(date == today()-2) %>% 
  group_by(date, continent) %>% 
  mutate(population = sum(population, na.rm = T), 
         population = population/1000000) %>% 
  unique() %>%
  ungroup() %>% 
  filter(continent %in% c("North America", "South America", 
                          "Europe", "Asia", "Africa")) %>% 
  #getting rid of date variable to not mess up the merge with case data later
  select(-date)


#checking that there's data for each continent 
log_print( 
  assert("Population data not calculating correctly", length(region_pop$continent) == 5 && 
           !is.na(region_pop$population))
)

#combining regional covid and population data to get per capita dataset --------
region_covid_data <-
  region_cases %>% 
  left_join(region_pop, by = "continent") %>% 
  mutate(new_cases_avg_per_pop = (new_cases_avg/population)) %>% 
  mutate(new_deaths_avg_per_pop = (new_deaths_avg/population)) %>% 
  mutate(new_cases_avg_per_pop = ifelse(is.na(new_cases_avg_per_pop),lag(new_cases_avg_per_pop), new_cases_avg_per_pop)) %>% 
  mutate(new_deaths_avg_per_pop = ifelse(is.na(new_deaths_avg_per_pop),lag(new_deaths_avg_per_pop), new_deaths_avg_per_pop)) 

save(region_covid_data, file = "cleaned_data/region_covid_data.rda")

#us state covid data -----------------------------------------------

state_population <-
  read_csv(url("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv")) %>% 
  select(state_name, population) %>% 
  mutate(population = population/1000000) %>% 
  rename(state = state_name)

state_covid_data <-
  read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>% 
  left_join(state_population, by = "state") 

save(state_covid_data, file = "cleaned_data/state_covid_data.rda")

state_map_case_data <- 
  state_cleaned_case() %>% 
  select(region, date, row_avg_weekly_new_cases) %>% 
  mutate(region = str_to_lower(region)) %>%
  filter(date == last(date)) %>% 
  left_join(us_map)

save(state_map_case_data, file = "cleaned_data/state_map_case_data.rda")

state_map_mobility_data <- 
state_cleaned_mobility("United States") %>% 
  select(region, date, roll_index) %>% 
  mutate(change_7day_roll_index = roll_index-lag(roll_index,1),
         avg_chg_weeklymobility = rollmean(change_7day_roll_index , 
                                           k = 7, align = "right", fill = NA)) %>% 
  mutate(region = str_to_lower(region)) %>%
  filter(date == last(date)) %>% 
  left_join(us_map)

save(state_map_mobility_data, file = "cleaned_data/state_map_mobility_data.rda")


log_close()
writeLines(readLines(lf))

