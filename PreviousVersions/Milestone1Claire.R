## SETUP
library(readr)
library(reactable)
library(shiny)
library(dplyr)
library(tidyr)
data <- read_csv("/Users/clairemorton/Documents/_CS+SG/mailUniverseBack.csv") # change this to whatever filepath you have

## BACKGROUND
# Variables that are categorical:
# - first_gift_date_range
# - first_gift_amt_range
# - join_channel_grouped
# - mrc_date_range
# - mrc_amt_range
# - hpc_date_range
# - hpc_amt_range
# - gift_frequency
# - loyalty
# - multi-channel
# - channel_behavior
# - sustainer_behavior

# I will be attempting to make a table with 4 variable elements that can be changed to 
#   first_gift_date_range, first_gift_amt_range, mrc_date_range, and mrc_amt_range. First,
#   I will be writing code for making a table with first_gift_date_range, than an app with 
#   one category that you can change from first_gift_date_range to first_gift_amt_range, 
#   then the final app.

# make sure R will be interpreting the things we want to be factors as factors
data$first_gift_date_range <- as.factor(data$first_gift_date_range)
data$first_gift_amt_range <- as.factor(data$first_gift_amt_range)
data$mrc_date_range <- as.factor(data$mrc_date_range)
data$mrc_amt_range <- as.factor(data$mrc_amt_range)

## TABLE FORMATTING
# how to get the counts we'll need for tables
count(data, first_gift_date_range)
count(data, first_gift_date_range, first_gift_amt_range)

# how to format tables
table1 <- count(data, first_gift_date_range)
spread(table1, first_gift_date_range, n)
# or, in one line:
spread(count(data, first_gift_date_range), first_gift_date_range, n)

table2 <- count(data, first_gift_date_range, first_gift_amt_range)
spread(table2, first_gift_date_range, n)
# or, in one line:
spread(count(data, first_gift_date_range, first_gift_amt_range), first_gift_date_range, n)

## APP V1
# select from two variables for 1-variable table
ui <- fluidPage(
  selectInput("var1", "Select variable to see in table", 
              choices = c("first_gift_date_range", "first_gift_amt_range")),
  
  reactableOutput("table")
)

server <- function(input, output) {
  output$table <- renderReactable({
    reactable(spread(count(data, data[input$var1]), input$var1, n))
  })
}

shinyApp(ui, server)


## APP V2
# select from two variables for 2-variable table
ui <- fluidPage(
  selectInput("var1", "Select variable 1 to see in table", 
              choices = c("first_gift_date_range", "first_gift_amt_range")),
  selectInput("var2", "Select variable 2 to see in table", 
              choices = c("first_gift_date_range", "first_gift_amt_range")),
  reactableOutput("table")
)

server <- function(input, output) {
  output$table <- renderReactable({
    reactable(spread(count(data, data[input$var1], data[input$var2]),
                     input$var1, n))
  })
}

shinyApp(ui, server)

## APP V3
# select from four variables for 4-variable table
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
        # set up all of the drop-down menus
        selectInput("var1", "Select variable 1 to see in table", 
                    choices = c("first_gift_date_range", "first_gift_amt_range",
                                "mrc_date_range", "mrc_amt_range")),
        selectInput("var2", "Select variable 2 to see in table", 
                    choices = c("first_gift_date_range", "first_gift_amt_range",
                                "mrc_date_range", "mrc_amt_range")),
        selectInput("var3", "Select variable 3 to see in table", 
                    choices = c("first_gift_date_range", "first_gift_amt_range",
                                "mrc_date_range", "mrc_amt_range")),
        selectInput("var4", "Select variable 4 to see in table", 
                    choices = c("first_gift_date_range", "first_gift_amt_range",
                                "mrc_date_range", "mrc_amt_range")),
        br()), # inserts line break, end sidebarPanel
    mainPanel(
        # call the table
        reactableOutput("table")
    ) # end mainPanel
  ) #end sidebarLayout
)

server <- function(input, output) {
  # create the table
  output$table <- renderReactable({
    reactable(spread(count(data, data[input$var4], data[input$var3], data[input$var2], data[input$var1]),
                     input$var1, n),
              bordered = TRUE)
  })
}

shinyApp(ui, server)



