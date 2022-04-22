library(tidyverse)
library(lubridate)
library(zoo)
library(testit)
library(logr)
library(googlesheets4)

# # designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# # trigger auth on purpose to store a token in the specified cache
# # a broswer will be opened
# googlesheets4::gs4_create()
# # see your token file in the cache, if you like
# list.files(".secrets/")

gs4_auth(cache = ".secrets", email = "estelles.bot.assistant@gmail.com")


tmp <- file.path("cleaned_data/data_cleaning.log")

#raw data ---------------------------------------------------------------------

#cases and vaccines
covid_data <- 
  read_csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))

#mobility data
urlfile="https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_countries.csv"
mobility<-read_csv(url(urlfile))

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
  ungroup() %>% 
  select(-index, -region, -parks, `grocery and pharmacy`)

#data loaded into initial app publication
# write_csv(cntry_cleaned_mobility, "cleaned_data/cntry_cleaned_mobility.csv")

#creating separate/smaller data for uploading to website
for (i in seq(1:month(today()))) {
  
  cntry_cleaned_mobility_2022_onwards <- 
    cntry_cleaned_mobility %>% 
    filter(date > "2021-12-31") %>% 
    filter(date >= ymd(paste0("2022-", i,"-01"))) %>% 
    filter(date < ymd(paste0("2022-", i+1, "-01" )))
  
  sheet_write(cntry_cleaned_mobility_2022_onwards,  ss = "1IBoGqC30KEVqFM3z2N6gOqvxeZMkyWDfsOGLqjX-iSE",
              sheet = paste0("cntry_cleaned_mobility_",i))
  
}

#COVID data cleaning 
cntry_cleaned_covid <-
  covid_data %>% 
  select(date, location, total_cases, new_cases, total_deaths, new_deaths, population) %>% 
  rename(country = location) %>% 
  #smoothing out by creating 7-day rolling average
  group_by(country) %>% 
  fill(new_cases, .direction = "down") %>%
  fill(new_deaths, .direction = "down") %>%
  mutate(new_cases_avg = rollmean(new_cases, k= 7, align = "right", fill = NA),
         new_deaths_avg =rollmean(new_deaths, k = 7, align = "right", fill = NA)) %>% 
  select(-new_cases, -new_deaths) %>%
  mutate(population = population/1000000,
          new_deaths_avg_per_pop = (new_deaths_avg/population),
          new_cases_avg_per_pop = (new_cases_avg/population)) %>% 
  mutate(new_cases_avg_per_pop = ifelse(new_cases_avg_per_pop <0, 0, new_cases_avg_per_pop),
         new_deaths_avg_per_pop = ifelse(new_deaths_avg_per_pop <0, 0, new_deaths_avg_per_pop)) %>% 
  ungroup() %>% 
  select(date, country, new_cases_avg_per_pop, new_deaths_avg_per_pop)

#data loaded into initial app publication
# write_csv(cntry_cleaned_covid, "cleaned_data/cntry_cleaned_covid.csv")

#creating separate/smaller data for uploading to website
for (i in seq(1:month(today()))) {
  
  cntry_cleaned_covid_2022_onward <- 
  cntry_cleaned_covid %>% 
    filter(date > today()-368) %>% 
    filter(date >= ymd(paste0("2022-", i,"-01"))) %>% 
    filter(date < ymd(paste0("2022-", i+1, "-01" )))
  
  sheet_write(cntry_cleaned_covid_2022_onward, ss = "1TpumENzcZE1r60Y4uqn6UWjk5_92u0iK1ZIM_H0G9Pg", 
              sheet = paste0("cntry_cleaned_covid_",i))
  
}

#data set of speed of cases and deaths --------------------------------------
increases_in_deaths_and_cases_within_this_week <- 
  cntry_cleaned_covid %>% 
  group_by(country) %>% 
  mutate(change_7day_avg_new_cases_per_pop = new_cases_avg_per_pop-lag(new_cases_avg_per_pop,1),
         change_7day_avg_new_deaths_per_pop = new_deaths_avg_per_pop-lag(new_deaths_avg_per_pop,1),
         avg_chg_weeklycases = rollmean(change_7day_avg_new_cases_per_pop, k = 14, align = "right", fill = NA),
         avg_chg_weeklydeaths = rollmean(change_7day_avg_new_deaths_per_pop, k = 14, align = "right", fill = NA),) %>% 
  ungroup() %>% 
  filter(date > today() - 3) %>% 
  select(date, country, avg_chg_weeklycases, avg_chg_weeklydeaths)

write_sheet(data = increases_in_deaths_and_cases_within_this_week, ss = "1iRB35-thoroWHQzMv2Sbx4LxOKR7_8Gb8ExkpBtgGhU", 
            sheet = "increases_in_deaths_and_cases_within_this_week")


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
  mutate(new_deaths_avg_per_pop = ifelse(is.na(new_deaths_avg_per_pop),lag(new_deaths_avg_per_pop), new_deaths_avg_per_pop)) %>% 
  filter(!is.na(continent)) %>%
  filter(date > "2020-01-31") %>% 
  select(date, continent, new_cases_avg_per_pop, new_deaths_avg_per_pop)

sheet_write(region_covid_data , ss = "1haO-gWx9msdunNKIyfzSthJc5pCEbYurM4J2SXKX_BM",
            sheet = "region_covid_data")


log_close()
writeLines(readLines(lf))

