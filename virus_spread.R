library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)

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
  mutate(date=as.Date(date, format="%m/%d/%y"),
         type = "cases")

deaths <- coronaDeaths %>% 
  gather(key = date, value = deaths, 5:ncol(coronaDeaths)) %>%
  group_by(date) %>% 
  summarise(confirmed=sum(deaths)) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y"),
         type = "deaths")

# Combine cases and deaths into one dataset

all <- rbind.data.frame(cases,deaths)

# Plot of the number of confirmed cases since Jan 21, 2020.
ggplotly(
  ggplot(data=cases)+
    geom_point(aes(x=date, y=confirmed), color="red")+
    geom_line(aes(x=date, y=confirmed), color="red")+
    labs(x="Date", y="Cumulate Number of Confirmed Cases",
         title="Cases of COVID-19")+
    theme_bw()+
    scale_x_date(date_breaks = "4 days")+
    theme(axis.text.x = element_text(angle=90),
          plot.title = element_text(hjust = .5))+
    scale_y_continuous(breaks = seq(0,10000000,200000))+
    ggsave("virus_cases.png", width=200,height=167,units="mm",
           path = filePath_WhereToSaveCasesPlot)
)

# Plot of the number of confirmed deaths since Jan 21, 2020.
ggplotly(
  ggplot(data=deaths)+
    geom_point(aes(x=date, y=confirmed))+
    geom_line(aes(x=date, y=confirmed))+
    labs(x="Date", y="Cumulate Number of Deaths",
         title = "Deaths From COVID-19")+
    theme_bw()+
    scale_x_date(date_breaks = "4 days")+
    theme(axis.text.x = element_text(angle=90),
          plot.title = element_text(hjust = .5))+
    scale_y_continuous(breaks = seq(0,1000000,10000))+
    ggsave("virus_deaths.png", width=200,height=167,units="mm",
           path = filePath_WhereToSaveDeathsPlot)
)

###################### Shiny App ####################################
titles = c("Confirmed Cases" = "cases",
            "Deaths" = "deaths")

ui <- fluidPage(
  titlePanel("COVID-19 Spread"),
  
  selectInput(inputId = "type",
              label = "Select data of interest",
              choices = titles),
  plotlyOutput(outputId = "c")
)

server <- function(input,output){
  output$c <- renderPlotly({
    ggplotly(
      ggplot(data=all %>% filter(type == input$type))+
        # geom_point(aes(x=date, y=confirmed), color=ifelse(input$type == "cases","red","black"))+
        geom_line(aes(x=date, y=confirmed), color=ifelse(input$type == "cases","red","black"))+
        labs(x="Date", y=paste("Cumulate Number of",names(titles)[titles == input$type]),
             title=paste("COVID-19",names(titles)[titles == input$type]))+
        theme_bw()+
        theme(axis.text.x = element_text(angle=90),
              plot.title = element_text(hjust = .5)),
      dynamicTicks = TRUE
    )
  })
    
}

shinyApp(ui = ui, server = server)

