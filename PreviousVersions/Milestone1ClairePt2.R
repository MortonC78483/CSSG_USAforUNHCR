## SETUP
library(readr)
library(reactable)
library(shiny)
library(dplyr)
library(tidyr)
data <- read_csv("/Users/clairemorton/Documents/_CS+SG/mailUniverseBack.csv") # change this to whatever filepath you have

## BACKGROUND
# Code will show how to subset our data based on join_channel_grouped. Next steps from here are 
# implementing the selection with other variables (e.g. MRC amounts) and combining this code
# with the existing code that changes the table layouts so that we have code that changes the 
# data going into the table and the layout of the table.

# make sure R will be interpreting the things we want to be factors as factors
data$join_channel_grouped <- as.factor(data$join_channel_grouped)

## APP
ui <- fluidPage(
  # probably want to put this inside of a dropdown!
  checkboxGroupInput("join_channel_grouped_selected", "Select Join Channel Types", 
                     choices = levels(data$join_channel_grouped), # the levels() command gets all of the levels of the factor
                     selected = levels(data$join_channel_grouped)), # sets everything to be selected to start
  
  reactableOutput("table") # This table is just to show that the checkbox does change the data. 
  # You'll want to make this the table from my/Vicky's code so that you're integrating your
  # code with the previously written code.
)

server <- function(input, output) {
  filtered <- reactive({
    data %>%
      # below command filters the join_channel_grouped column so that it only has values
      # that are included in the list of selected values from the input. This selection element
      # will end up being different depending on what you're selecting -- ex. for MRC amount
      # you'll want to filter so that all of the amounts are above the low end of the values selected
      # and below the high end of the values selected.
      filter(join_channel_grouped %in% input$join_channel_grouped_selected)
  })
  
  output$table <- renderReactable({
    # note the use of filtered() here -- you'll want to make a reactive expression like filtered()
    # and use it in the table creation portion of my/Vicky's code instead of "data"
    reactable(spread(count(filtered(), join_channel_grouped), join_channel_grouped, n))
  })
}

shinyApp(ui, server)
