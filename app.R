library(shiny)

library(shinydashboard)
library(tidyverse)
library(leaflet)


ui <- dashboardPage(
  dashboardHeader(title = "Exploring this Dataset"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map of the United States", tabName = "tab_1"),
      menuItem("City Profile", tabName = "tab_2")
    )#tab names that will show up, defining them for R as "tab_1" etc.
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab_1",
              fluidRow(
                box(leafletOutput("us_map", height = 300, width = 700)),
                box(title = "Victim Characteristics", 
                    checkboxGroupInput("race", "Race", 
                                choices = c("Black", "White", "Latino" 
                                            )), 
                    checkboxGroupInput("gender", "Gender",
                                choices = c("Male", "Female", "Non-conforming")),
                    sliderInput("age", "Age:", min = 6, max = 87, 
                                value = c(6, 87)), 
                    checkboxGroupInput("armed", "Armed", 
                                       choices = c("Not armed", "Firearm", 
                                                              "Knife", "Unknown", "Other",
                                                              "Non-lethal firearm")),
                    checkboxGroupInput("manner", "Manner of Death",
                                       choices = c("Death in Custody", "Gunshot",
                                                   "Other", "Struck by Vehicle", 
                                                   "Taser"))),
                
                box(title = "Situation Characteristics",
                    checkboxGroupInput("year", "Year", choices = c("2015", "2016")),
                    sliderInput("month", "Month of the Year", min = 1,
                                max = 12, value = c(1,12)),
                    checkboxGroupInput("state", "State", choices = c("CA", "AZ")), 
                    checkboxGroupInput("city", "City", choices = c("Santa Barbara, CA",
                                                            "Phoenix, AZ"))
                  )
              )),
      
      tabItem(tabName = "tab_2",
              fluidRow(
                box(plotOutput("my_graph2", height = 500)), 
                box(title = "Choose Color:", 
                    radioButtons("color2", "Choose Color:",
                                 choices = c("red", "yellow", "gray")))
              )) 
    )
  )
) #everything the audience is going to interact with.


server <- function(input, output){
  output$my_graph1 <- renderPlot({
    histogram(faithful$waiting, col = input$color1)
  })
  
  output$my_graph2 <- renderPlot({
    ggplot(faithful, aes(x = waiting, y = eruptions)) + 
      geom_point(color = input$color2)
  })
}
shinyApp(ui = ui, server = server)

