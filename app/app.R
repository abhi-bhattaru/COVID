#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("COVID-19 Analysis"),
    p("By: Abhi Bhattaru"),
    textOutput("date"),
    
    
    br(),
    h2("Date retrieval"),
    p("The EEDC publishes data on COVID19 daily. Files were imported from EEDC.", a(href = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")),
    br(),
    
    h3("Plot of cummulative cases per day"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            strong("Choose countries"),
            checkboxGroupInput("countries", "",
                               choices = c("China", 
                                           "United_States_of_America",
                                           "Germany", 
                                           "Italy",
                                           "Japan",
                                           "India")),
            strong("Choose date range"),
            dateInput("date", "", 
                      min=ymd("2020-01-01"),
                      max=ymd(today()))
            
            
        ),
        mainPanel = mainPanel(
            plotOutput("plotout")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    COVID19 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
    cumalativetable<-COVID19 %>%
        select(dateRep, cases, geoId, countriesAndTerritories, popData2018)%>%
        pivot_wider(names_from = dateRep, values_from = cases, values_fill = list(cases=0))%>% #note the list
        pivot_longer(cols=-c(countriesAndTerritories, geoId, popData2018), names_to="dateRep", values_to='cases')%>%
        mutate(date=dmy(dateRep))%>%
        group_by(geoId)%>%
        arrange(date)%>%
        mutate(cum_sum = cumsum(cases))%>%
        mutate(cum_sumperpop = cum_sum*1000000/popData2018)%>%
        ungroup()
   
    output$date <- renderText ({paste('',today())})
    output$plotout <- renderPlot({
        plottable<- filter(cumalativetable, countriesAndTerritories %in% input$countries)
        plot <- ggplot(data=plottable, mapping = aes(date,cum_sum, col=countriesAndTerritories))+
            geom_line()+
            geom_point()+
            geom_hline(yintercept = 10000)
        plot
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
