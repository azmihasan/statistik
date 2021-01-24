#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(graphics)
library(shiny)
library(ggplot2)
library(plotly)
library(ggthemes)
library(stats)
library(base)

#Data extraction
covidData <- read.csv("RKI_COVID19-20201102.csv", header = TRUE, encoding = "UTF-8")

covidData$Meldedatum <- as.Date(covidData$Meldedatum, format = "%Y/%m/%d")

# Definieren der Benutzeroberfläche für die Anwendung
ui <- fluidPage(
    
    # Applikationstitel
    titlePanel("Visualisierung für Covid Data"),
    
    # Begleittext
    p("Dieses Shiny-App wurde erstellt, 
      um Daten nach Covid 19 im Jahr 2020 in Deutschland zu visualisieren."),
    
    br(),
    
    # Änderung der Visualisierung
    sidebarLayout(
        sidebarPanel(
            
            fluidRow(
                
                column(10,wellPanel(
                    
                    h4(strong("Visualisierung nach Merkmalen")),
                    
                    p("Hier kann man Merkmale der Daten ändern"),
                    
                    selectInput("kat",
                                "Merkmale",
                                c("Altersgruppe",
                                  "Geschlecht",
                                  "Bundesländer")
                    )
                    
                )),
                
                column(12, wellPanel(
                    
                    dateRangeInput("date","Zeitraum", 
                                   start = as.Date("2020-01-21"), 
                                   end = as.Date("2020-11-01"),
                                   min = as.Date("2020-01-21"),
                                   max = as.Date("2020-11-01"), 
                                   language = "de")
                ))
                
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            fluidRow(
                
                h5(strong("Visualisierung der im bestimmten Zeitraum angemeldeten Anzahl der Infizierten nach Merkmalen")),
                
                column(12, wellPanel(
                    
                    plotlyOutput("timedata")
                    
                )),
                
                h5(strong("Visualisierung der gesamten Anzahl der Infizierten nach Merkmalen")),
                
                p("Zeitraum von 21. Januar 2020 bis 1. November 2020"),
                
                column(12, wellPanel(
                    
                    plotOutput("data")
                    
                )
                
                )
                
                
            )
            
        )
        
    )
    
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$timedata <- renderPlotly({
        
        switch(input$kat,
               
               "Altersgruppe" = 
                   
                   covidData %>%
                   
                   ggplot(aes(y = AnzahlFall, x = Meldedatum, color = Altersgruppe)) + 
                   
                   geom_line() + 
                   
                   theme(axis.title = element_text())+
                   
                   scale_x_date(limit=c(input$date[1],input$date[2])) + 
                   
                   labs( title = "Anzahl der Fälle über Zeitreihe in Deutschland",
                         x = "Altersgruppe",
                         y = "Anzahl der Fälle"
                   )
               
               
               ,
               
               "Geschlecht" = 
                   
                   covidData %>%
                   
                   ggplot(aes(y = AnzahlFall, x = Meldedatum, color = Geschlecht)) + 
                   
                   geom_line() + 
                   
                   theme(axis.title = element_text())+
                   
                   scale_x_date(limit=c(input$date[1],input$date[2])) + 
                   
                   labs( title = "Anzahl der Fälle über Zeitreihe in Deutschland",
                         x = "Geschlecht",
                         y = "Anzahl der Fälle"
                   )
               
               
               ,
               
               "Bundesländer" = 
                   
                   covidData %>%
                   
                   ggplot(aes(y = AnzahlFall, x = Meldedatum, color = Bundesland)) + 
                   
                   geom_line() +
                   
                   theme(axis.title = element_text()) +
                   
                   scale_x_date(limit=c(input$date[1],input$date[2])) + 
                   
                   labs( title = "Anzahl der Fälle über Zeitreihe in Deutschland",
                         x = "Bundesländer",
                         y = "Anzahl der Fälle"
                   )
               
               
        )
        
    })
    
    
    output$data <- renderPlot({
        
        switch(input$kat,
               "Altersgruppe" = 
                   
                   covidData %>%
                   
                   ggplot(aes(y = AnzahlFall, x = Bundesland, color = Bundesland)) + 
                   
                   geom_bar(stat = "identity") +
                   
                   theme(axis.text.x = element_text(angle = 60, hjust = 1, size = (8))) +
                   
                   theme_fivethirtyeight() +
                   
                   theme(axis.title = element_text())+
                   
                   labs( title = "Anzahl der Fälle in Deutschland",
                         x = "Bundesländer",
                         y = "Anzahl der Fälle"
                   )
               
               ,
               
               "Geschlecht" = 
                   
                   covidData %>%
                   
                   ggplot(aes(y = AnzahlFall, x = Geschlecht, color = Geschlecht)) + 
                   
                   geom_bar(stat = "identity") + 
                   
                   theme_fivethirtyeight() +
                   
                   theme(axis.title = element_text())+
                   
                   labs( title = "Anzahl der Fälle in Deutschland",
                         x = thema,
                         y = "Anzahl der Fälle"
                   )
               
               ,
               
               "Bundesländer" = 
                   
                   covidData %>%
                   
                   ggplot(aes(y = AnzahlFall, x = Bundesland, color = Bundesland)) + 
                   
                   geom_bar(stat = "identity") +
                   
                   theme(axis.text.x = element_text(angle = 60, hjust = 1, size = (8))) +
                   
                   theme_fivethirtyeight() +
                   
                   theme(axis.title = element_text())+
                   
                   labs( title = "Anzahl der Fälle in Deutschland",
                         x = "Bundesländer",
                         y = "Anzahl der Fälle"
                   )
               
               
        )
        
    }) 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
