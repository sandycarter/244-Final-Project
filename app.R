library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)


#Data Read in
Death15_16 <- read_csv("2015_2016.csv")

# Data Wrangling
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
                    sliderInput("month", "Month of the Year", min = 1,
                                max = 12, value = c(1,12))
                   
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
              fluidRow(
                box(plotOutput("my_graph2", height = 500)), 
                box(title = "Choose Color:", 
                    radioButtons("color2", "Choose Color:",
                                 choices = c("red", "yellow", "gray")))
              )) 
    
  )
) )



server <- function(input, output){
  output$us_map <- renderLeaflet({
    
    DeathMap_df <- Death15_16 %>%
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2])
      # filter(month > input$month[1] & month < input$month[2])
    
    leaflet(DeathMap_df) %>% #change Death15_16 here to the newdf
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      setView (-98.35, 39.50, zoom = 3) %>% 
      addCircles(~longitude, ~latitude)
    
  })
  
  output$count <- renderText ({
    DeathMap_count <- Death15_16 %>%
      filter(raceethnicity %in% input$race) %>%
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2])
    #month
    
     paste("Total Deaths is", nrow(DeathMap_count)) 
    
  })
  
 # output$demGender <- renderPlot({
  #  DeathGender <- Death15_16 %>%
   #   filter(gender %in% input$gender) %>% 
    #  count(gender) %>%            # 
     # mutate(prop = prop.table(n)) 
    #as.data.frame(DeathGender) 
    
  #  arrange(DeathGender, prop)
   # DeathGender$label <- paste0(DeathGender$gender, " ", round(DeathGender$prop *100), "%")
    
  #  ggplot(DeathGender, aes(x="", y = prop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  #   scale_fill_brewer("Blues") +
   #   coord_polar("y", start = 0) + 
    #  theme_void() +
     # geom_text(aes(x = 1, y = cumsum(prop)-prop/2, label = label)) 
    #ggplot(DeathGender, aes(gender)) + 
     # stat_count()
    
    
#  })
  output$demGender <- renderPlotly({
    
    m <- list(l = 10, r = 0, t = 0, b = 5)
    
    DeathGender <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(gender %in% input$gender) %>% 
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
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
           )
  })
  
  output$demRace <- renderPlotly({
    
    DeathRace <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(raceethnicity %in% input$race) %>% 
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
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$demArmed <- renderPlotly({
    
    DeathArmed <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(armed %in% input$armed) %>% 
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
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$demManner <- renderPlotly({
    
    DeathManner <- Death15_16 %>%
      filter(age > input$age[1] & age < input$age[2]) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      count(mannerofdeath) %>%            # 
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
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
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
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
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
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             displayModeBar=FALSE
             )
  })
  
  
  output$my_graph2 <- renderPlot({
   ggplot(faithful, aes(x = waiting, y = eruptions)) + 
     geom_point(color = input$color2)
 })
}
shinyApp(ui = ui, server = server)


