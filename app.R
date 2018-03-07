library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

# Data Read in and Wrangling
source("Data Wrangling R.R")

#UI

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
              titlePanel("Select Characteristics"),
              
              fluidRow(
                box(title = "Victim Characteristics", width = 9,
                    column(width = 3,
                    checkboxGroupInput("race", "Race/Ethnicity", 
                                choices = unique(Death15_16$raceethnicity) , 
                                selected = unique(Death15_16$raceethnicity))
                    ), 
                    column(width = 3,
                    checkboxGroupInput("gender", "Gender",
                                choices = unique(Death15_16$gender),
                                selected = unique(Death15_16$gender)),
                    sliderInput("age", "Age:", min = 6, max = 87, 
                                value = c(6, 87))
                    ), 
                    column(width = 3,
                    checkboxGroupInput("armed", "Armed", 
                                       choices = unique(Death15_16$armed),
                                       selected = unique(Death15_16$armed))
                    ),
                    column(width = 3,
                    checkboxGroupInput("manner", "Manner of Death",
                                       choices = unique(Death15_16$mannerofdeath),
                                       selected = unique(Death15_16$mannerofdeath))
                    )
                    ),
                
                box(title = "Situation Characteristics", width =3,
                    checkboxGroupInput("year", "Year", choices = unique(Death15_16$year), 
                                       selected = unique(Death15_16$year)),
                    checkboxGroupInput("season", "Season",
                                       choices = unique(Death15_16$season),
                                       selected = unique(Death15_16$season))
                   
                  )),
              
              titlePanel("Map (Based on Characteristics Selected)"),
              fluidRow(box(title = "Map", leafletOutput("us_map"), width = 12)),
            
              fluidRow(box(width = 12, align = "center",
                           h3(textOutput("count"))),
                       
              titlePanel("Demographics (Based on Characteristics Selected)"),
              fluidRow(
                 column(6, 
                    box(title = "Gender", 
                    plotlyOutput("demGender", height = 200),
                    verbatimTextOutput("event")
                    ),
                  box(title = "Race/Ethnicity",
                      plotlyOutput("demRace", height = 200)
                      )),
                 column(6,
                  box(title = "Armed",
                      plotlyOutput("demArmed", height = 200)),
                  box(title = "Manner of Death",
                      plotlyOutput("demManner", height = 200))
                  )),
              
              # US DEMOGRAPHIC DATA
              titlePanel("US Demographics for Reference"),
              fluidRow(
                box(title = "Gender", height = 290,
                    plotlyOutput("demGenderUS", height = 200)),
                box(title = "Race/Ethnicity", height = 290,
                plotlyOutput("demRaceUS", height = 200),
                p("Hispanic/Latino (of any Race): 17.8%")))
                )),
      
      
      
      tabItem(tabName = "tab_2",
              titlePanel("Comparisons by City and/or State"),
              
              fluidRow(
                box(title = "Choose Cities to Compare:",
                    checkboxGroupInput("cityChoice", "City",
                                       choices = head(Death15_16$city))
                    ),
                box(title = "Choose Comparison Variable for Cities:",
                    selectInput("variableSelectCity", "Choose Variable:",
                                choices = c(
                                  "Race" = 1, 
                                  "Gender" = 2, 
                                  "Armed" = 3, 
                                  "Manner of Death" = 4, 
                                  "Year" = 5, 
                                  "Season" = 6)
                                )
                    )
              ),
              
              fluidRow(
                box(title = "City Plot",
                  plotOutput("my_graph2", height = 500)), 
            
                box(title = "City Table",
                  tableOutput("compTablecity"))
              ),
              
              fluidRow(
                box(title = "Choose States to Compare:",
                    checkboxGroupInput("stateChoice", "State",
                                       choices = head(Death15_16$state))),
                box(title = "Choose Comparison Variable for State:",
                    selectInput("variableSelectCity", "Choose Variable:",
                                choices = c(
                                  "Race" = 1, 
                                  "Gender" = 2, 
                                  "Armed" = 3, 
                                  "Manner of Death" = 4, 
                                  "Year" = 5, 
                                  "Season" = 6)
                              )
                    )
                ),
              
              fluidRow(
                box(title = "State Plot",
                       plotOutput("my_graph3",height = 500)),
                box(title = "State Table",
                    plotOutput("compTableState"))
              ) 
    
  )
) ) )



server <- function(input, output){
  output$us_map <- renderLeaflet({
    
    DeathMap_df <- Death15_16 %>%
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(year %in% input$year) %>% 
      filter(season %in% input$season)
    
    pal <- colorFactor(c("brown", "red", "orange", "green", "yellow", "purple", "purple", "blue"), domain = c("Arab-American","Asian/Pacific Islander","Black", "Hispanic/Latino","Native American","Other","Unknown","White"))
    
    leaflet(DeathMap_df) %>% #change Death15_16 here to the newdf
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      setView (-98.35, 39.50, zoom = 3) %>% 
      addCircles(~longitude, ~latitude, 
                 color = ~pal(raceethnicity), 
                 popup = ~paste(name, ",", raceethnicity, ",", age, ",", gender)) %>% 
      addLegend("bottomright", pal = pal, values = ~raceethnicity,
                title = "Race/Ethnicity",
                opacity = 1)
    
  })
  
  output$count <- renderText ({
    DeathMap_count <- Death15_16 %>%
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(year %in% input$year) %>% 
      filter(season %in% input$season)
    
     paste("Total Deaths is", nrow(DeathMap_count)) 
    
  })
  
  output$demGender <- renderPlotly({
    
    m <- list(l = 10, r = 0, t = 0, b = 5)
    
    DeathGender <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(year %in% input$year) %>% 
      filter(season %in% input$season) %>% 
      count(gender) %>%            # 
      mutate(prop = prop.table(n)) 
    as.data.frame(DeathGender) 
    
    plot_ly(DeathGender, labels = ~gender, values = ~prop, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(gender, round(prop*100), "%"))    %>%
    layout(autosize = T,margin = m,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
    config(displayModeBar = F)
  })
  
  output$demRace <- renderPlotly({
    
    DeathRace <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(year %in% input$year) %>% 
      filter(season %in% input$season) %>% 
      count(raceethnicity) %>%            # 
      mutate(prop = prop.table(n)) 
    
    as.data.frame(DeathRace) 
    
    m <- list(l = 10, r = 0, t = 0, b = 5)
    
    plot_ly(DeathRace, values = ~prop, type = 'pie', labels = ~raceethnicity,
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(raceethnicity, round(prop*100), "%")
            ) %>%
           
      layout(autosize = T, margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% 
      config(displayModeBar = F)
  })
  
  output$demArmed <- renderPlotly({
    
    DeathArmed <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(year %in% input$year) %>% 
      filter(season %in% input$season)%>% 
      count(armed) %>%            # 
      mutate(prop = prop.table(n)) 
    as.data.frame(DeathArmed) 
    
    m <- list(l = 10, r = 0, t = 0, b = 5)
    
    plot_ly(DeathArmed, labels = ~armed, values = ~prop, type = 'pie',
      textposition = 'inside',
    textinfo = 'percent',
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = 'text',
    text = ~paste(armed, round(prop*100), "%"))    %>%
 
      layout(autosize = T, margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% 
      config(displayModeBar = F)
  })
  
  output$demManner <- renderPlotly({
    
    DeathManner <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(year %in% input$year) %>% 
      filter(season %in% input$season) %>% 
      count(mannerofdeath) %>%            
      mutate(prop = prop.table(n)) 
    
    as.data.frame(DeathManner) 
    
    m <- list(l = 10, r = 0, t = 0, b = 5)
    
    plot_ly(DeathManner, labels = ~mannerofdeath, values = ~prop, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(mannerofdeath, round(prop*100), "%"))  %>%
      layout(autosize = T, margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% 
      config(displayModeBar = F)
  })
  
  output$demRaceUS <- renderPlotly({
    
    m <- list(l = 10, r = 0, t = 0, b = 5)
    
    plot_ly(USRace, labels = ~Race, values = ~Prop, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Race, round(Prop*100), "%"))  %>%
      layout(autosize = T, margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      config(displayModeBar = F)
  })
  
  

output$demGenderUS <- renderPlotly({
    
    m <- list(l = 10, r = 0, t = 0, b = 5)
    
    plot_ly(USGender, labels = ~Gender, values = ~Prop, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Gender, round(Prop*100), "%"))  %>%
      layout(autosize = T, margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      config(displayModeBar = F)
  })
  
  
  output$my_graph2 <- renderPlot({
   ggplot(Death15_16, alpha = 0.2,
          aes(x = input$variableSelectCity, 
              group = input$variableSelectCity,
              fill= input$variableSelectCity)) + 
     geom_bar(position = "fill")
 })
}
shinyApp(ui = ui, server = server)


