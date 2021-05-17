library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)

###############
# import data #
###############
kv_data <- read_csv("us-daily-covid-vaccine-doses-administeredpopstate.csv") 


#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)

# for TAB 1 (line) widgets: 
# for selectInput, 'choices' object should be a NAMED LIST


states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
            "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
            "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
            "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming")
                        
kv_states_shiny <- kv_data %>%
  filter(Entity %in% states) %>%
  mutate(state = tolower(Entity)
         , Day = as.Date(Day, format="%m/%d/%y")) %>%
  select(Day, state, daily_vaccinations, state_population) %>%
  group_by(state) %>%
  summarise(Day, state_population, daily_vaccinations, cumulative_vaccinations = cumsum(daily_vaccinations)) %>%
  mutate(percentpopcumulative = cumulative_vaccinations/state_population, percentdaily = daily_vaccinations/state_population, 
         state = str_to_title(state))

line_choice_values <- c("cumulative_vaccinations", "percentpopcumulative", "daily_vaccinations", "percentdaily")

line_choice_names <- c("Cumulative Vaccinations", "Cumulative Vaccines Delivered as a Percent of Population", "Vaccinations Administered per Day", 
                       "Vaccinations Administered per Day as a Percent of Population")
names(line_choice_values) <- line_choice_names

#for checkboxGroupInput
# state_choice_values <- c("alabama",
#                          "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "florida",
#                          "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana",
#                          "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri",
#                          "montana", "nebraska", "nevada", `new hampshire`, `new jersey`, `new mexico`, `new york`, `north carolina`, `north dakota`, "ohio",
#                          "oklahoma", "oregon", "pennsylvania", `rhode island`, `south carolina`, `south dakota`, "tennessee", "texas", "utah", "vermont", "virginia", "washington", `west virginia`, "wisconsin", "wyoming")
# state_choice_names <- c("Alabama",
#                               "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
#                               "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
#                               "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
#                               "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
#                               "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

# state_choice_values <- c("alabama",
#                          "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "florida",
#                          "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana",
#                          "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri",
#                          "montana", "nebraska", "nevada", "ohio",
#                          "oklahoma", "oregon", "pennsylvania", "tennessee", "texas", "utah", "vermont", "virginia", "washington",  "wisconsin", "wyoming")
# state_choice_names <- c("Alabama",
#                         "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
#                         "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
#                         "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
#                         "Montana", "Nebraska", "Nevada", "Ohio",
#                         "Oklahoma", "Oregon", "Pennsylvania", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",  "Wisconsin", "Wyoming")
# 
# names(state_choice_values) <- state_choice_names

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
  
  # TAB 1: LINE
  # data_for_line <- reactive({
  #   data <- select(kv_states_shiny, input$linevar)
  # })
  # 
  data_for_state <- reactive({
    data <- filter(kv_states_shiny, state %in% input$statevar)

  })
  # 
  output$line <- renderPlot({
    ggplot() +
      geom_line(data = data_for_state(), aes_string(x = "Day", y = input$linevar, color = "state")) +
   
       labs(color = "State",
         #x = line_choice_names[line_choice_values == input$linevar]
           y = line_choice_names[line_choice_values == input$linevar])
  })
}

shinyApp(ui = ui, server = server)
