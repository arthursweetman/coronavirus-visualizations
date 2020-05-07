library(tidyverse)
library(plotly)
library(shiny)
library(ggthemes)
library(maps)

load("~/GitHub/coronavirus-visualizations/time_series.Rda")
load("~/GitHub/coronavirus-visualizations/world_map_combined.Rda")
load("~/GitHub/coronavirus-visualizations/usa_map_combined.Rda")
state_map <- map_data("state")

titles = c("Confirmed Cases" = "cases",
           "Deaths" = "deaths")

# Vector of regions in descending order by number of confirmed cases
temp <- time_series %>% 
  filter(type == "cases") %>% 
  arrange(-confirmed)
regions <- c("all",unique(temp$region))

ui <- fluidPage(
  titlePanel("COVID-19 Spread"),
  plotlyOutput(outputId = "worldmap"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "region",
                  label = "Select region of interest",
                  choices = regions),
      selectInput(inputId = "type",
              label = "Select data of interest",
              choices = titles),
    ),
    mainPanel(
      plotlyOutput(outputId = "timeseries")
    )
  ),
  
  # plotlyOutput(outputId = "usamap")
)

server <- function(input,output){
  
  # Time series plot
  output$timeseries <- renderPlotly({
    
    if(input$region == "all"){
      filtered <- time_series %>%
        filter(type == input$type) %>% 
        group_by(date) %>% 
        summarise(confirmed = sum(confirmed)) %>% 
        arrange(date)
    } else {
      filtered <- time_series %>% 
        filter(type == input$type,
               region == input$region)
    }
    
    ggplotly(
      ggplot(data=filtered)+
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
  
  # World map
  output$worldmap <- renderPlotly({
    # ggplotly(
    #   ggplot(data=world_map_combined)+
    #     geom_polygon(aes(x=long, y=lat, group=group, fill=Confirmed))+
    #     theme_map()+
    #     scale_fill_gradient(low = "gray70", high = "darkred", 
    #                         na.value = "gray70",
    #                         name="No. Confirmed",
    #                         trans="log",
    #                         breaks=c(10,100,1000,10000,100000,1000000))+
    #     labs(title="Coronavirus Spread",
    #          subtitle = today()-1,
    #          caption = "Data source: https://github.com/CSSEGISandData/COVID-19")+
    #     theme(legend.position = c(0.85,0.05),
    #           plot.title = element_text(hjust = 0.5,size=20),
    #           plot.subtitle = element_text(hjust = 0.5))+
    #     coord_quickmap()
    # ) 
    
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
    
  })
  
  # USA map
  output$usamap <- renderPlotly({
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
  })
  
}

shinyApp(ui = ui, server = server)
