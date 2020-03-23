library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(countrycode)

# Change these file paths to match the appropriate paths on your computer
filePath_TimeSeriesConfirmed <- "~/GitHub/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
filePath_TimeSeriesDeaths <- "~/GitHub/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
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
ggplot(data=world_map_corona)+
  geom_polygon(aes(x=long, y=lat, group=group, fill=recent))+
  theme_map()+
  scale_fill_gradient(low = "gray70", high = "darkred", 
                      na.value = "gray70",
                      name="No. Confirmed",
                      trans="log",
                      breaks=c(8,80,800,8000,80000))+
  labs(title="Coronavirus Spread",
       subtitle = today()-1,
       caption = "Data source: https://github.com/CSSEGISandData/COVID-19")+
  theme(legend.position = c(0.05,0.15),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ggsave("virus_map.png",
         path = filePath_WhereToSaveTheMap)+
  coord_quickmap()
