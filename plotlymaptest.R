library(plotly)
library(countrycode)
library(tidyverse)
library(lubridate)

####################################################################

day <- ifelse(day(today()-1) < 10, 
              paste0("0",day(today()-1)), 
              day(today()-1))
month <- ifelse(month(today()-1) < 10,
                paste0("0",month(today()-1)),
                month(today()-1))
today <- paste(month,day,year(today()-1),sep="-")

daily_report <- read_csv(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
                                today,".csv"))

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


plot_geo(daily_report_global) %>% 
  add_trace(
    z=~log10(Confirmed), locations=~CODE,
    color=~log10(Confirmed), colors=colorRampPalette(colors=c("gray90","darkred"))(nrow(daily_report_global)),
    text=~hover,
    hoverinfo="text"
  ) %>% 
  colorbar(tickvals=c(1,2,3,4,5,6),
           ticktext=c(10,100,1000,10000,100000,1000000),
           title="No. Confirmed") %>% 
  layout(
    title="COVID-19 Spread",
    geo=list(projection=list(type="miller"))
  )




