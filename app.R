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
      menuItem("Compare Cities and States", tabName = "tab_2"), 
      menuItem("About this Dataset", tabName = "tab_3")
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
              titlePanel("Comparisons by City and State"),
              
              fluidRow(
                box(title = "Choose Cities to Compare:",
                    checkboxGroupInput("cityChoice", "City",
                                       choices = unique(citysort10$city), 
                                       select = "Los Angeles")
                    ),
                box(title = "Choose Comparison Variable for Cities:",
                    selectInput("variableSelectCity", "Choose Variable:",
                                choices = c("Race" = "raceethnicity",
                                            "Gender" = "gender",
                                            "Manner of Death" = "mannerofdeath",
                                            "Armed" = "armed", 
                                            "Year" = "year", 
                                            "Season" = "season")
                                ),
                    hr()
                    )
              ),
              
              fluidRow(
                box(title = "City Plot",
                  plotOutput("my_graph2")) 
                , 
            
                box(title = "City Table",
                tableOutput("my_table2"))
              ),
              
              
              fluidRow(
                box(title = "Choose States to Compare:",
                    checkboxGroupInput("stateChoice", "State",
                                       choices = unique(statesort10$state), 
                                       select = "CA")
                ),
                box(title = "Choose Comparison Variable for States:",
                    selectInput("variableSelectState", "Choose Variable:",
                                choices = c("Race" = "raceethnicity",
                                            "Gender" = "gender",
                                            "Manner of Death" = "mannerofdeath",
                                            "Armed" = "armed", 
                                            "Year" = "year", 
                                            "Season" = "season")
                    ),
                    hr()
                )
              ),
              
              
              fluidRow(
                box(title = "State Plot",
                       plotOutput("my_graph3")),
                box(title = "State Table",
                    plotOutput("my_table3"))
              )), 
    
  
  tabItem(tabName = "tab_3",
          titlePanel(""),
          fluidRow(includeMarkdown("include.Rmd")
            
          ))
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
    DF_cc <- Death15_16 %>% filter(city %in% input$cityChoice)
    
    variable <- switch(input$variableSelectCity, 
                       armed = DF_cc$armed,
                       raceethnicity = DF_cc$raceethnicity, 
                       gender = DF_cc$gender, 
                       season = DF_cc$season, 
                       year = DF_cc$year, 
                       mannerofdeath = DF_cc$mannerofdeath) 
    
    
    ggplot(DF_cc, alpha = 0.2,
          aes(x = city, 
             group = variable,
            fill = variable)) + 
     geom_bar(position = "fill") +
     theme_classic()+
      scale_y_continuous(expand = c(0,0)) +
      xlab("City") +
      ylab("Proportion of City Total")
 })
  
  output$my_graph3 <- renderPlot({
    DF_sc <- Death15_16 %>% filter(state %in% input$stateChoice)
    
    variable <- switch(input$variableSelectState, 
                       armed = DF_sc$armed,
                       raceethnicity = DF_sc$raceethnicity, 
                       gender = DF_sc$gender, 
                       season = DF_sc$season, 
                       year = DF_sc$year, 
                       mannerofdeath = DF_sc$mannerofdeath) 
    
    
    ggplot(DF_sc, alpha = 0.2,
           aes(x = state, 
               group = variable,
               fill = variable)) + 
      geom_bar(position = "fill") +
      theme_classic()+
      scale_y_continuous(expand = c(0,0)) +
      xlab("State") +
      ylab("Proportion of State Total")
  })
  
  #output$my_table2 <- renderText ({
   # DF_tb <- Death15_16 %>%
    #  filter(city %in% input$cityChoice) 
    
    #variabletb2 <- switch(input$variableSelectCity, 
     #                     armed = DF_tb$armed,
      #                    raceethnicity = DF_tb$raceethnicity, 
       #                   gender = DF_tb$gender, 
        ##                 year = DF_tb$year, 
          #                mannerofdeath = DF_tb$mannerofdeath)
    
 #   paste("you chose", variabletb2)
    
#  })
  
  
  #output$my_table2 <- renderTable({
  # DF_tb <- Death15_16 %>% filter(city %in% input$cityChoice)
    
    
    
    #A <- count(DF_tb, variabletb2, state)
    #A2 <- arrange(A, state)

   
 # })
}
shinyApp(ui = ui, server = server)


