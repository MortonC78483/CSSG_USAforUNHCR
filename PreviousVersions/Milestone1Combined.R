## SETUP
library(readr)
library(reactable)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
data <- read_csv("/Users/clairemorton/Documents/_CS+SG/mailUniverseBack.csv") # change this to whatever filepath you have

data <- data %>%
  select(-c(roi_family_id, roi_id,
            first_gift_soft, first_gift_source_code, first_gift_source_description, first_gift_source_type, 
            mrc_soft, mrc_source_code, mrc_source_description, mrc_source_type,
            hpc_soft, hpc_source_code, hpc_source_description, hpc_source_type,
            otg_gifts, otg_amount, pledge_gifts, pledge_amount, overall_gifts, ever_soft,
            address_status, address_contact_status, do_not_contact, deceased_date, membership, deceased
            )) %>%
  mutate_if (is.character, as.factor) %>% # mutate everything that is a character (not numeric or date) to be factor
  mutate(direct_marketing = as.factor(direct_marketing)) %>%
  mutate(face_to_face = as.factor(face_to_face)) %>%
  mutate(digital_marketing = as.factor(digital_marketing)) %>%
  mutate(drtv = as.factor(drtv)) %>%
  mutate(other = as.factor(other))

data <- data %>% rename(
  "First gift date range" = first_gift_date_range, 
  "First gift amount range" = first_gift_amt_range, 
  "MRC date range" = mrc_date_range, 
  "MRC amount range" =  mrc_amt_range, 
  
  "Membership type" = membership_type, 
  "Membership status" = membership_status, 
  "Account class" = account_class, 
  "Join channel" = join_channel, 
  "Join channel grouped" = join_channel_grouped, 
  
  "HPC date range" = hpc_date_range, 
  "HPC amount range" = hpc_amt_range,
  
  "First gift source contact type" = first_gift_source_contact_type, 
  "MRC source contact type" = mrc_source_contact_type,
  "MRC promotion code" = mrc_promotion_code, 
  "HPC source contact type" = hpc_source_contact_type, 
  "HPC promotion code" = hpc_promotion_code, 
  "HPC date range" = hpc_date_range, 
  
  "Gift frequency" = gift_frequency, 
  "Current year donor" = current_year_donor, 
  "Loyalty" = loyalty,
  "Multi channel" = multi_channel, 
  "Channel behavior" = channel_behavior, 
  
  "Direct marketing" =  direct_marketing, 
  "Face to face" = face_to_face,
  "Digital marketing" = digital_marketing, 
  "DRTV" = drtv, 
  
  "Telemarketing responsive" = telemarketing_responsive,
  "Direct mail responsive" = directmail_responsive, 
  "Face to face responsive" = facetoface_responsive, 
  "Digital responsive" = digital_responsive, 
  "DRTV responsive" = drtv_responsive, 
  "Sustainer behavior" = sustainer_behavior
) 

variable_choices <-  c(
  "First gift date range", "First gift amount range",
  "MRC date range", "MRC amount range", 
  "Membership type" , "Membership status", "Account class", "Join channel", "Join channel grouped", 
  "HPC date range", "HPC amount range", 
  "First gift source contact type", "MRC source contact type", "MRC promotion code", "HPC source contact type", "HPC promotion code", "HPC date range", 
  "Gift frequency", "Current year donor" , "Loyalty", "Multi channel", "Channel behavior",  
  "Direct marketing", "Face to face", "Digital marketing", "DRTV",
  "Telemarketing responsive", "Direct mail responsive", "Face to face responsive", "DRTV responsive", "Sustainer behavior"
)

## APP
ui <- fluidPage(
  titlePanel("Milestone 1"),
  fluidRow(
    column(1),
    column(2,
      dropdown(label = "Join Channel Types",
        checkboxGroupInput("join_channel_grouped_selected", NULL,
                           choices = levels(data$"Join channel grouped"), 
                           selected = levels(data$"Join channel grouped")))
    ),
    
    column(2,
      dropdown(label = "Membership Loyalty",
        checkboxGroupInput("loyalty", NULL,
                           choices = levels(data$"Loyalty"),
                           selected = levels(data$"Loyalty")))
    ),
    
    column(2,
    dropdown(label = "Gift Frequency",
      checkboxGroupInput("gift_frequency", NULL,
                         choices = levels(data$"Gift frequency"),
                         selected = levels(data$"Gift frequency")))
    ),
    
    column(2,
    dropdown(label = "First Gift",
      dateRangeInput("first_gift_date", "Date Range",
                     start = min(data$first_gift_date),
                     end = max(data$first_gift_date),
                     min = min(data$first_gift_date),
                     max = max(data$first_gift_date)),
      numericInput("first_gift_min", "Minimum",
                   value = min(data$first_gift_amount)),
      numericInput("first_gift_max", "Maximum",
                   value = max(data$first_gift_amount)))
    ),
    
    column(2,
    dropdown(label = "Most Recent Contribution",
      dateRangeInput("mrc_date", "Date Range",
                     start = min(data$mrc_date),
                     end = max(data$mrc_date),
                     min = min(data$mrc_date),
                     max = max(data$mrc_date)),
      numericInput("mrc_min", "Minimum",
                   value = min(data$mrc_amount)),
      numericInput("mrc_max", "Maximum",
                   value = max(data$mrc_amount)))
    )
  ), # end fluidRow
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      # set up all of the drop-down menus
      selectInput("var1", "Select variable 1 to see in table", 
                  choices = variable_choices),
      selectInput("var2", "Select variable 2 to see in table", 
                  choices = variable_choices),
      selectInput("var3", "Select variable 3 to see in table", 
                  choices = variable_choices),
      selectInput("var4", "Select variable 4 to see in table", 
                  choices = variable_choices),
      br()), # end sidebarPanel
    
    mainPanel(
      # call the table
      reactableOutput("table")
    ) # end mainPanel
  ) #end sidebarLayout
)

server <- function(input, output) {
  # create the table
  filtered <- reactive({
    data %>%
      filter(data$"Join channel grouped" %in% input$join_channel_grouped_selected) %>%
      filter(data$"Loyalty" %in% input$loyalty)%>%
      filter(data$"Gift frequency" %in% input$gift_frequency)%>%
      
      filter(first_gift_date >= input$first_gift_date[1])%>%
      filter(first_gift_date <= input$first_gift_date[2])%>%
      filter(first_gift_amount >= input$first_gift_min) %>%
      filter(first_gift_amount <= input$first_gift_max)%>%
      
      filter(mrc_date >= input$mrc_date[1])%>%
      filter(mrc_date <= input$mrc_date[2])%>%
      filter(mrc_amount >= input$mrc_min) %>%
      filter(mrc_amount <= input$mrc_max)
  })
  
  output$table <- renderReactable({
    reactable(spread(count(filtered(), filtered()[input$var4], filtered()[input$var3], filtered()[input$var2], filtered()[input$var1]),
                     input$var1, n),
              bordered = TRUE)
  })
}

shinyApp(ui, server)