library(tidyverse)
library(lubridate)
library(maps)
library(countrycode)

# Date must be correctly formatted to correctly identify online file
day <- ifelse(day(today()-1) < 10, 
              paste0("0",day(today()-1)), 
              day(today()-1))
month <- ifelse(month(today()-1) < 10,
                paste0("0",month(today()-1)),
                month(today()-1))
today <- paste(month,day,year(today()-1),sep="-")

# Read in data
coronaCases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
coronaDeaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
daily_report <- read_csv(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
                                today,".csv"))
world_map <- map_data("world") %>% 
  mutate(iso3n = countrycode(region,
                             origin="country.name",
                             destination = "iso3n"))
usa_map <- map_data("county") %>% 
  mutate(Combined_Key = paste(str_to_title(subregion),str_to_title(region),
                              "US", sep=", "))

# Do some data handling
cases <- coronaCases %>% 
  gather(key = date, value = confirmed, 5:ncol(coronaCases)) %>%
  group_by(region = `Country/Region`,date) %>% 
  summarise(confirmed=sum(confirmed)) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y"),
         type = "cases")

deaths <- coronaDeaths %>% 
  gather(key = date, value = deaths, 5:ncol(coronaDeaths)) %>%
  group_by(region = `Country/Region`,date) %>% 
  summarise(confirmed=sum(deaths)) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y"),
         type = "deaths")

daily_report_global <- daily_report %>% 
  group_by(region = Country_Region) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Active = sum(Active)) %>%
  mutate(CODE = countrycode(region,
                            origin = "country.name",
                            destination = "iso3c"),
         hover = with(paste(region,"<br>",
                            "Confirmed:", Confirmed)))

daily_report_us <- daily_report %>% 
  filter(Country_Region == "US") %>% 
  group_by(Combined_Key) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Active = sum(Active)) %>% 
  filter(Combined_Key != "Recovered")

# Combined world maps
# world_map_combined <- left_join(world_map,daily_report_global,by="iso3n")
usa_map_combined <- left_join(usa_map,daily_report_us,by="Combined_Key")

# Combine cases and deaths into one dataset
time_series <- rbind(cases,deaths)

# Save the final data sets
save(time_series, file = "~/GitHub/coronavirus-visualizations/time_series.Rda")
save(daily_report_global, file = "~/GitHub/coronavirus-visualizations/world_map_combined.Rda")
save(usa_map_combined, file = "~/GitHub/coronavirus-visualizations/usa_map_combined.Rda")


