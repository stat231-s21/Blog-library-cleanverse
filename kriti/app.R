library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)

###############
# import data #
###############
kv_data <- read_csv("/Users/kritiverma/Documents/Amherst Junior Year/Spring 2021/KV Data Science/git/Blog-library-cleanverse/kriti/us-daily-covid-vaccine-doses-administered_final2.csv") 


#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)

states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
            "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
            "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
            "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming")

#creating object                      
kv_states_shiny <- kv_data %>%
  filter(Entity %in% states) %>%
  mutate(state = tolower(Entity)
         , Day = as.Date(Day, format="%m/%d/%y")) %>%
  select(Day, state, daily_vaccinations, state_population) %>%
  group_by(state) %>%
  summarise(Day, state_population, daily_vaccinations, cumulative_vaccinations = cumsum(daily_vaccinations)) %>%
  mutate(percentpopcumulative = cumulative_vaccinations/state_population, percentdaily = daily_vaccinations/state_population, 
         state = str_to_title(state))

#creating choices for user to pick variables and states to include
line_choice_values <- c("cumulative_vaccinations", "percentpopcumulative", "daily_vaccinations", "percentdaily")

line_choice_names <- c("Cumulative Vaccinations", "Cumulative Vaccines Delivered as a Percent of Population", "Vaccinations Administered per Day", 
                       "Vaccinations Administered per Day as a Percent of Population")

names(line_choice_values) <- line_choice_names

state_choices <- unique(kv_states_shiny$state)


############
#    ui    #
############
ui <- navbarPage(
  
  title = "USA State Vaccinations",
  
  tabPanel(
    title = "Line Graphs",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "linevar"
                    , label = "Choose a variable of interest to plot:"
                    , choices = line_choice_values
                    , selected = "Cumulative Vaccinations"),
        selectizeInput(inputId = "statevar"
                           , label = "Include states:"
                           , choices = state_choices
                           , selected = c("Massachusetts", "New Jersey", "California")
                           , multiple = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "line")
      )
    )
  )
)



############
# server   #
############
server <- function(input,output){
  
  #creating reactive dataset
  data_for_state <- reactive({
    data <- filter(kv_states_shiny, state %in% input$statevar)

  })

  #creating plot
  output$line <- renderPlot({
    ggplot() +
      geom_line(data = data_for_state(), aes_string(x = "Day", y = input$linevar, color = "state")) +
   
       labs(color = "State",
           y = line_choice_names[line_choice_values == input$linevar])
  })
}

shinyApp(ui = ui, server = server)
