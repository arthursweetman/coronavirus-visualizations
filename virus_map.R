library(tidyverse)
library(maps)
library(ggthemes)
library(countrycode)

# Change these file paths to match the appropriate paths on your computer
filePath_TimeSeriesConfirmed <- "GitHub/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
filePath_TimeSeriesDeaths <- "GitHub/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
filePath_DailyReports <- "GitHub/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"
filePath_WhereToSaveTheMap <- "~/GitHub/coronavirus-visualizations"

coronaCases <- read_csv(filePath_TimeSeriesConfirmed)
coronaDeaths <- read_csv(filePath_TimeSeriesDeaths)
world_map <- map_data("world")

# Create clone variable so we have a name to correctly reference.
# This is necessary because the column with the most recent data
# changes its name every day
time <- now()
coronaCases$recent <- pull(coronaCases, ifelse(hour(time)<19, ncol(coronaCases), ncol(coronaCases)-1)) # If after 7pm, add "-1" to the end of "ncol(corona)"

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

# Map with a logarithmic scale
options(scipen = 999) # Prevent scale from converting to scientific notation
ggplot(data=world_map_corona)+
  geom_polygon(aes(x=long, y=lat, group=group, fill=recent))+
  theme_map()+
  scale_fill_gradient(low = "gray70", high = "darkred", 
                      na.value = "gray70",
                      name="No. Confirmed",
                      trans="log",
                      breaks=c(10,100,1000,10000,100000))+
  labs(title="Coronavirus Spread",
       subtitle = today()-1,
       caption = "Data source: https://github.com/CSSEGISandData/COVID-19")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  coord_quickmap()+
  ggsave("virus_map.png",
         path = filePath_WhereToSaveTheMap)

# USA map with counties
if(month(today()) < 10){
  month <- paste0("0",month(today()-1))
  today <- paste(month,day(today()-1),year(today()-1),sep="-")
} else {
  today <- paste(month(today()-1),day(today()-1),year(today()-1),sep="-")
}

# Read in Daily Report
coronaCases1 <- read_csv(paste0(
  filePath_DailyReports,today,".csv"))

# Do some String handling
corona1_grouped <- coronaCases1 %>% 
  filter(Country_Region == "US") %>% 
  rename("county"=Admin2) %>% 
  mutate(Combined_Key=str_to_lower(str_sub(Combined_Key,1, -5)))

# Read in County AND State USA maps
USA_map <- map_data("county") %>% 
  mutate(location_full = paste(subregion,region,sep=", "))
USA_states <- map_data("state")

# Combine county map and COVID-19 daily report
USA_map_corona <- left_join(USA_map, corona1_grouped, by=c("location_full"="Combined_Key"))

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
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  coord_quickmap()+
  ggsave("virus_map_USA.png",
         path = filePath_WhereToSaveTheMap)
