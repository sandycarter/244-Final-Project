library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)


#Data Read in
Death15_16 <- read_csv("2015_2016.csv")

# Data Wrangling

source("Data Wrangling R.R")
#Death15_16$raceethnicity <- as.factor(Death15_16$raceethnicity)
#Death15_16$gender <- as.factor(Death15_16$gender)
#Death15_16$armed <- as.factor(Death15_16$armed)
#Death15_16$age <- as.numeric(Death15_16$age)
#Death15_16$month <- as.factor(Death15_16$month)
#Death15_16$state <- as.factor(Death15_16$state)
#Death15_16$month <- as.factor(Death15_16$month)
#Death15_16$mannerofdeath <- as.factor(Death15_16$mannerofdeath)

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
                                selected = unique(Death15_16$gender))
                    
                    ,
                    sliderInput("age", "Age:", min = 6, max = 87, 
                                value = c(6, 87))
                    )
                    , 
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
                    )
                ,
                
                box(title = "Situation Characteristics", width =3,
                    checkboxGroupInput("year", "Year", choices = unique(Death15_16$year), 
                                       selected = unique(Death15_16$year)),
                    sliderInput("month", "Month of the Year", min = 1,
                                max = 12, value = c(1,12)),
                    checkboxGroupInput("state", "State", choices = c("CA", "AZ")), 
                    checkboxGroupInput("city", "City", choices = c("Santa Barbara, CA",
                                                                   "Phoenix, AZ"))
                  ),
                titlePanel("title 2")),
              fluidRow(
                box(plotOutput("my_graph2"))
                )
                
              # DEMOGRAPHIC DATA
              # US DEMOGRAPHIC DATA
              # DEMOGRAPHIC DATA SPECIFIC TO THE INPUTS. 
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
#everything the audience is going to interact with.


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
 #output$demGender <- renderPlot({
  #gender_prop_test <- Death15_16 %>% 
   # filter(gender %in% input$gender) %>% 
    #select(gender) %>% 
    #count(gender) %>% 
    #mutate(prop = prop.table(n))
  
 # ggplot(gender_prop_test, aes(x="", y=prop, fill =gender)) +
  #  geom_bar(width = 1, stat = "identity")   
  
  #  input$gender == 
   # DeathGender_df <- Death15_16 %>%
    #  filter(gender %in% input$gender)
      #Deathgender15 <-data.frame(table(DeathGender_df$gender)/length(DeathGender_df$gender)*100)
     # colnames(DeathGender_df) <- c("Gender", "Percent")
    
   # ggplot(DeathGender_df, aes(x="", y = (table(DeathGender_df$gender)/length(DeathGender_df$gender)), fill=Gender)) +
    #  geom_bar(width=1, stat = "identity") +
     # coord_polar("y", start = 0) + theme_void()
 #})
 
 # output$my_graph2 <- renderPlot({
  #ggplot(faithful, aes(x = waiting, y = eruptions)) + 
   #geom_point()
  #})
}
shinyApp(ui = ui, server = server)


