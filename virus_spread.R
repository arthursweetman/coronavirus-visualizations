library(tidyverse)
library(lubridate)

# Change these file paths to match the appropriate paths on your computer
filePath_TimeSeriesConfirmed <- "~/GitHub/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
filePath_TimeSeriesDeaths <- "~/GitHub/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
filePath_WhereToSaveCasesPlot <- "~/GitHub/coronavirus-visualizations"
filePath_WhereToSaveDeathsPlot <- "~/GitHub/coronavirus-visualizations"

coronaCases <- read_csv(filePath_TimeSeriesConfirmed)
coronaDeaths <- read_csv(filePath_TimeSeriesDeaths)

cases <- corona %>% 
  gather(key = date, value = confirmed, 5:ncol(coronaCases)) %>%
  group_by(date) %>% 
  summarise(confirmed=sum(confirmed)) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y"))

deaths <- coronaDeaths %>% 
  gather(key = date, value = deaths, 5:ncol(corona)) %>%
  group_by(date) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y"))

# Plot of the number of confirmed cases and confirmed deaths since Jan 21, 2020.
ggplot(data=cases)+
  geom_point(aes(x=date, y=confirmed), color="red")+
  geom_line(aes(x=date, y=confirmed), color="red")+
  geom_point(aes(x=date, y=deaths), data=deaths)+
  geom_line(aes(x=date, y=deaths), data=deaths)+
  labs(x="Date", y="Cumulate Number of Confirmed Cases")+
  theme_bw()+
  scale_x_date(date_breaks = "4 days")+
  theme(axis.text.x = element_text(angle=90))+
  scale_y_continuous(breaks = seq(0,2000000,10000))+
  ggsave("virus_cases.png",
         path = filePath_WhereToSaveCasesPlot) 

# Just Deaths
ggplot(data=deaths)+
  geom_point(aes(x=date, y=deaths))+
  geom_line(aes(x=date, y=deaths))+
  labs(x="Date", y="Cumulate Number of Deaths")+
  theme_bw()+
  scale_x_date(date_breaks = "4 days")+
  theme(axis.text.x = element_text(angle=90))+
  scale_y_continuous(breaks = seq(0,2000000,1000))+
  ggsave("virus_deaths.png",
         path = filePath_WhereToSaveDeathsPlot) 
