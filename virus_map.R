library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(countrycode)

# Change this directory to match where you would like to save the
# outputted maps on your computer
filePath_WhereToSaveGlobalMap <- "~/GitHub/coronavirus-visualizations"
filePath_WhereToSaveUSAMap <- "~/GitHub/coronavirus-visualizations"

# Date must be correctly formatted to correctly identify online file
day <- ifelse(day(today()-1) < 10, 
              paste0("0",day(today()-1)), 
              day(today()-1))
month <- ifelse(month(today()-1) < 10,
                paste0("0",month(today()-1)),
                month(today()-1))
today <- paste(month,day,year(today()-1),sep="-")

# Read in data
daily_report <- read_csv(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
                                today,".csv"))
coronaCases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
coronaDeaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
world_map <- map_data("world")

# Create clone variable so we have a name to correctly reference.
# This is necessary because the column with the most recent data
# changes its name every day
# Ex. Now we can refer to "recent" rather than today's date as a string
# time <- now()
# coronaCases$recent <- pull(coronaCases, ifelse(hour(time)<19, ncol(coronaCases), ncol(coronaCases)-1)) # If after 7pm, add "-1" to the end of "ncol(corona)"

# Standardizes country names into unique numbers
coronaCases$iso3 <- countrycode(coronaCases$`Country/Region`,
                    origin = "country.name",
                    destination = "iso3n")
world_map$iso3 <- countrycode(world_map$region,
                              origin="country.name",
                              destination = "iso3n")

# groups cases from all regions into their respective countries
# Makes running the plot a lot faster
corona_grouped <- coronaCases %>% 
  group_by(iso3) %>% 
  summarise(recent=sum(recent))

# Combines map data with coronavirus data
world_map_corona <- left_join(world_map, corona_grouped, by=c("iso3"="iso3"))

#############################
# World Map
#############################
options(scipen = 999) # Prevent scale from converting to scientific notation
ggplot(data=world_map_corona)+
  geom_polygon(aes(x=long, y=lat, group=group, fill=recent))+
  theme_map()+
  scale_fill_gradient(low = "gray70", high = "darkred", 
                      na.value = "gray70",
                      name="No. Confirmed",
                      trans="log",
                      breaks=c(10,100,1000,10000,100000,1000000))+
  labs(title="Coronavirus Spread",
       subtitle = today()-1,
       caption = "Data source: https://github.com/CSSEGISandData/COVID-19")+
  theme(plot.title = element_text(hjust = 0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(.05,.15))+
  coord_quickmap()+
  ggsave("virus_map.png",width = 200, height=167,units="mm",
         path = filePath_WhereToSaveGlobalMap)

#############################
# USA map with counties
#############################

# Do some String handling
daily_report <- daily_report %>% 
  filter(Country_Region == "US") %>% 
  rename("county"=Admin2) %>% 
  mutate(Combined_Key=str_to_lower(str_sub(Combined_Key,1, -5)))

# Read in County AND State USA maps
USA_map <- map_data("county") %>% 
  mutate(location_full = paste(subregion,region,sep=", "))
USA_states <- map_data("state")

# Combine county map and COVID-19 daily report
USA_map_corona <- left_join(USA_map, daily_report, by=c("location_full"="Combined_Key"))

# Create map, use state map to draw state borders
ggplot(data=USA_map_corona)+
  geom_polygon(aes(x=long, y=lat, group=group, fill=Confirmed))+
  geom_path(aes(x=long,y=lat,group=group), data=USA_states, color="gray50", size=.1)+
  theme_map()+
  scale_fill_gradient(low = "gray70", high = "darkred", 
                      na.value = "gray70",
                      name="No. Confirmed",
                      trans="log",
                      breaks=c(10,100,1000,10000,100000))+
  labs(title="Coronavirus Spread",
       subtitle = today()-1,
       caption = "Data source: https://github.com/CSSEGISandData/COVID-19")+
  theme(legend.position = c(0.85,0.05),
        plot.title = element_text(hjust = 0.5,size=20),
        plot.subtitle = element_text(hjust = 0.5))+
  coord_quickmap()+
  ggsave("virus_map_USA.png", width = 200, height=167,units="mm",
         path = filePath_WhereToSaveUSAMap)
