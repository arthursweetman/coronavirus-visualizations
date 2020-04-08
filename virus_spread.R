library(tidyverse)
library(lubridate)

# Change these directories to match where you would like to save the
# outputted plots on your computer
filePath_WhereToSaveCasesPlot <- "~/GitHub/coronavirus-visualizations"
filePath_WhereToSaveDeathsPlot <- "~/GitHub/coronavirus-visualizations"

# Read in data
coronaCases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
coronaDeaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# Do some data handling
cases <- coronaCases %>% 
  gather(key = date, value = confirmed, 5:ncol(coronaCases)) %>%
  group_by(date) %>% 
  summarise(confirmed=sum(confirmed)) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y"))

deaths <- coronaDeaths %>% 
  gather(key = date, value = deaths, 5:ncol(coronaDeaths)) %>%
  group_by(date) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y"))

# Plot of the number of confirmed cases since Jan 21, 2020.
ggplot(data=cases)+
  geom_point(aes(x=date, y=confirmed), color="red")+
  geom_line(aes(x=date, y=confirmed), color="red")+
  labs(x="Date", y="Cumulate Number of Confirmed Cases",
       title="Cases of COVID-19")+
  theme_bw()+
  scale_x_date(date_breaks = "4 days")+
  theme(axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = .5))+
  scale_y_continuous(breaks = seq(0,10000000,100000))+
  ggsave("virus_cases.png", width=200,height=167,units="mm",
         path = filePath_WhereToSaveCasesPlot) 

# Plot of the number of confirmed deaths since Jan 21, 2020.
ggplot(data=deaths)+
  geom_point(aes(x=date, y=deaths))+
  geom_line(aes(x=date, y=deaths))+
  labs(x="Date", y="Cumulate Number of Deaths",
       title = "Deaths From COVID-19")+
  theme_bw()+
  scale_x_date(date_breaks = "4 days")+
  theme(axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = .5))+
  scale_y_continuous(breaks = seq(0,1000000,10000))+
  ggsave("virus_deaths.png", width=200,height=167,units="mm",
         path = filePath_WhereToSaveDeathsPlot) 
