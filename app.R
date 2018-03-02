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
              titlePanel("Title"),
              fluidRow(box(title = "Map", leafletOutput("us_map"), width = 12)),
              fluidRow(
                box(title = "Victim Characteristics", width = 9,
                    column(width = 3,
                    checkboxGroupInput("race", "Race", 
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
                                max = 12, value = c(1,12)),
                    checkboxGroupInput("state", "State", choices = c("CA", "AZ")), 
                    checkboxGroupInput("city", "City", choices = c("Santa Barbara, CA",
                                                                   "Phoenix, AZ"))
                  )),
              
                titlePanel("Demographics (Based on Inputs Made Above)"),
               fluidRow(
                  box(title = "Gender",
                    plotlyOutput("demGender", height = 250),
                    verbatimTextOutput("event")
                    ),
                  box(title = "Race",
                      plotlyOutput("demRace", height = 250)
                      ),
                  box(title = "Armed",
                      plotlyOutput("demArmed", height = 250)),
                  box(title = "Manner of Death",
                      plotlyOutput("demManner", height = 250))
                  ),
              
              # US DEMOGRAPHIC DATA
              titlePanel("US Demograpics (For reference)")
              
                ),
      
      tabItem(tabName = "tab_2",
              fluidRow(
                box(plotOutput("my_graph2", height = 500)), 
                box(title = "Choose Color:", 
                    radioButtons("color2", "Choose Color:",
                                 choices = c("red", "yellow", "gray")))
              )) 
    )
  )
) 



server <- function(input, output){
  output$us_map <- renderLeaflet({
    
    DeathMap_df <- Death15_16 %>%
      filter(raceethnicity %in% input$race) %>% 
      # age  filter(age %in% input$age) %>% 
      filter(gender %in% input$gender) %>% 
      filter(armed %in% input$armed) %>% 
      filter(mannerofdeath %in% input$manner) %>% 
      filter(age > input$age[1] & age < input$age[2])
      # filter(month > input$month[1] & month < input$month[2])
    # month of year (Slider)
    # state?
    # city?
    
    leaflet(DeathMap_df) %>% #change Death15_16 here to the newdf
      addTiles() %>%
      setView (-84.5555, 42.7325, zoom = 7) %>% 
      addCircles(~longitude, ~latitude)
    
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
    
    DeathGender <- Death15_16 %>%
      filter(gender %in% input$gender) %>% 
      count(gender) %>%            # 
      mutate(prop = prop.table(n)) 
    as.data.frame(DeathGender) 
    
    plot_ly(DeathGender, labels = ~gender, values = ~prop, type = 'pie',
            textposition = 'auto',
            textinfo='label+percent',
            insidetextfont = list(color = '#FFFFFF')) %>%
    layout(title = "Gender", autosize=TRUE)
  })
  
  output$demRace <- renderPlotly({
    
    DeathRace <- Death15_16 %>%
      filter(raceethnicity %in% input$race) %>% 
      count(raceethnicity) %>%            # 
      mutate(prop = prop.table(n)) 
    as.data.frame(DeathRace) 
    
    plot_ly(DeathRace, labels = ~raceethnicity, values = ~prop, type = 'pie',
            textposition = 'inside',
            textinfo='label+percent',
            insidetextfont = list(color = '#FFFFFF')) %>%
      layout(title = "Race", autosize=TRUE)
  })
  
  output$demArmed <- renderPlotly({
    
    DeathArmed <- Death15_16 %>%
      filter(armed %in% input$armed) %>% 
      count(armed) %>%            # 
      mutate(prop = prop.table(n)) 
    as.data.frame(DeathArmed) 
    
    plot_ly(DeathArmed, labels = ~armed, values = ~prop, type = 'pie',
            textposition = 'auto',
            textinfo='label+percent',
            insidetextfont = list(color = '#FFFFFF')) %>%
      layout(title = "Armed", autosize=TRUE)
  })
  
  output$demManner <- renderPlotly({
    
    DeathManner <- Death15_16 %>%
      filter(mannerofdeath %in% input$manner) %>% 
      count(mannerofdeath) %>%            # 
      mutate(prop = prop.table(n)) 
    as.data.frame(DeathManner) 
    
    plot_ly(DeathManner, labels = ~mannerofdeath, values = ~prop, type = 'pie',
            textposition = 'auto',
            textinfo='label+percent',
            insidetextfont = list(color = '#FFFFFF')) %>%
      layout(title = "Armed", autosize=TRUE)
  })
  
  
  output$my_graph2 <- renderPlot({
   ggplot(faithful, aes(x = waiting, y = eruptions)) + 
     geom_point(color = input$color2)
 })
}
shinyApp(ui = ui, server = server)


