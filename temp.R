library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(countrycode)
library(plotly)

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
world_map <- map_data("world") %>% 
  mutate(iso3n = countrycode(region,
                             origin="country.name",
                             destination = "iso3n"))
usa_map <- map_data("county") %>% 
  mutate(Combined_Key = paste(str_to_title(subregion),str_to_title(region),
                               "US", sep=", "))
state_map <- map_data("state")

# Data manipulation

daily_report_global <- daily_report %>% 
  group_by(region = Country_Region) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Active = sum(Active)) %>% 
  mutate(iso3n = countrycode(region,
                              origin = "country.name",
                              destination = "iso3n"))

daily_report_us <- daily_report %>% 
  filter(Country_Region == "US") %>% 
  group_by(Combined_Key) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Active = sum(Active)) %>% 
  filter(Combined_Key != "Recovered")

# Combined world map
world_map_combined <- left_join(world_map,daily_report_global,by="iso3n")
usa_map_combined <- left_join(usa_map,daily_report_us,by="Combined_Key")

# Create maps
ggplotly(
  ggplot(data=world_map_combined)+
    geom_polygon(aes(x=long, y=lat, group=group, fill=Confirmed))+
    theme_map()+
    scale_fill_gradient(low = "gray70", high = "darkred", 
                        na.value = "gray70",
                        name="No. Confirmed",
                        trans="log",
                        breaks=c(10,100,1000,10000,100000,1000000))+
    labs(title="Coronavirus Spread",
         subtitle = today()-1,
         caption = "Data source: https://github.com/CSSEGISandData/COVID-19")+
    theme(legend.position = c(0.85,0.05),
          plot.title = element_text(hjust = 0.5,size=20),
          plot.subtitle = element_text(hjust = 0.5))+
    coord_quickmap()
)

ggplotly(
  ggplot(data=usa_map_combined)+
    geom_path(aes(x=long,y=lat,group=group), data=state_map, color="gray50", size=.1)+
    geom_polygon(aes(x=long, y=lat, group=group, fill=Confirmed))+
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
    coord_quickmap()
)
