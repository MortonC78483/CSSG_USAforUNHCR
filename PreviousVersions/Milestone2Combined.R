## SETUP
library(readr)
library(reactable)
library(shiny)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(RColorBrewer)
data <- read_csv("/Users/clairemorton/Documents/_CS+SG/App/mailUniverseBack.csv") # change this to whatever filepath you have

data <- data %>%
  dplyr::select(-c(roi_family_id, roi_id,
                   account_class, 
                   first_gift_soft, first_gift_source_code, first_gift_source_description, first_gift_source_type,
                   first_gift_source_contact_type, join_channel, direct_marketing, face_to_face, digital_marketing,
                   drtv, other,
                   mrc_soft, mrc_source_code, mrc_source_description, mrc_source_type, mrc_source_contact_type,
                   hpc_soft, hpc_source_code, hpc_source_description, hpc_source_type, hpc_source_contact_type,
                   otg_gifts, otg_amount, pledge_gifts, pledge_amount, overall_gifts, ever_soft,
                   multi_channel, telemarketing_responsive, directmail_responsive, facetoface_responsive, digital_responsive, drtv_responsive,
                   address_status, address_contact_status, do_not_contact, deceased_date, membership, deceased
  )) %>%
  mutate_if (is.character, as.factor)# %>% # mutate everything that is a character (not numeric or date) to be factor
#mutate(direct_marketing = as.factor(direct_marketing)) %>%
#mutate(face_to_face = as.factor(face_to_face)) %>%
#mutate(digital_marketing = as.factor(digital_marketing)) %>%
#mutate(drtv = as.factor(drtv)) %>%
#mutate(other = as.factor(other))

data <- data %>% rename(
  "First gift date range" = first_gift_date_range,
  "First gift amount range" = first_gift_amt_range,
  "MRC date range" = mrc_date_range,
  "MRC amount range" =  mrc_amt_range,
  
  "Membership type" = membership_type,
  "Membership status" = membership_status,
  "Join channel grouped" = join_channel_grouped,
  
  "HPC date range" = hpc_date_range,
  "HPC amount range" = hpc_amt_range,
  
  "MRC promotion code" = mrc_promotion_code,
  "HPC promotion code" = hpc_promotion_code,
  
  "Gift frequency" = gift_frequency,
  "Current year donor" = current_year_donor,
  "Loyalty" = loyalty,
  "Channel behavior" = channel_behavior,
  
  "Sustainer behavior" = sustainer_behavior
)

variable_choices <-  c(
  "First gift date range", "First gift amount range", "MRC date range", "MRC amount range",
  "Membership type" , "Membership status", "Join channel grouped",
  "HPC date range", "HPC amount range",
  "MRC promotion code", "HPC promotion code",
  "Gift frequency", "Current year donor" , "Loyalty", "Channel behavior",
  "Sustainer behavior"
)

# color code from https://themockup.blog/posts/2020-05-13-qb-salaries-vs-playoff-appearances/,
# https://themockup.blog/posts/2020-05-29-client-side-interactivity-do-more-with-crosstalk/
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
scale_color <- make_color_pal(c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

## APP
ui <- fluidPage(
  titlePanel("Milestone 2"),
  fluidRow(
    splitLayout(tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                           overflow: visible;
                                           }
                                           "))), #makes dropdowns show up
                 cellWidths = c("4%","14%","14%","12%","10%","20%"),
                #cellArgs = list(style = "padding: 20px"), #adds padding
               
                #column(2,
                       dropdown(label = "Channel Behavior",
                                checkboxGroupInput("channel_behavior_grouped_selected", NULL,
                                                   choices = levels(data$"Channel behavior"),
                                                   selected = levels(data$"Channel behavior"))),
                #),
                
                #column(2,
                       dropdown(label = "Membership Loyalty",
                                checkboxGroupInput("loyalty", NULL,
                                                   choices = levels(data$"Loyalty"),
                                                   selected = levels(data$"Loyalty"))),
                #),
                
                #column(2,
                       dropdown(label = "Gift Frequency",
                                checkboxGroupInput("gift_frequency", NULL,
                                                   choices = levels(data$"Gift frequency"),
                                                   selected = levels(data$"Gift frequency"))),
                #),
                
                #column(2,
                       dropdown(label = "First Gift",
                                dateRangeInput("first_gift_date", "Date Range",
                                               start = min(data$first_gift_date),
                                               end = max(data$first_gift_date),
                                               min = min(data$first_gift_date),
                                               max = max(data$first_gift_date)),
                                numericInput("first_gift_min", "Minimum",
                                             value = min(data$first_gift_amount)),
                                numericInput("first_gift_max", "Maximum",
                                             value = max(data$first_gift_amount))),
                #),
                
                #column(2,
                       dropdown(label = "Most Recent Contribution",
                                dateRangeInput("mrc_date", "Date Range",
                                               start = min(data$mrc_date),
                                               end = max(data$mrc_date),
                                               min = min(data$mrc_date),
                                               max = max(data$mrc_date)),
                                numericInput("mrc_min", "Minimum",
                                             value = min(data$mrc_amount)),
                                numericInput("mrc_max", "Maximum",
                                             value = max(data$mrc_amount))),
                #),
                
                #column(2,
                       dropdown(label = "Highest Past Contribution",
                                dateRangeInput("hpc_date", "Date Range",
                                               start = min(data$hpc_date),
                                               end = max(data$hpc_date),
                                               min = min(data$hpc_date),
                                               max = max(data$hpc_date)),
                                numericInput("hpc_min", "Minimum",
                                             value = min(data$hpc_amount)),
                                numericInput("hpc_max", "Maximum",
                                             value = max(data$hpc_amount)))
                #)
    )# end splitlayout
  ), # end fluidRow
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      # set up all of the drop-down menus
      selectizeInput("cols", "Select variables to see in table",
                     choices = variable_choices, multiple = TRUE,
                     options = list(maxItems = 4)),
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
      filter(data$"Channel behavior" %in% input$channel_behavior_grouped_selected &
               data$"Loyalty" %in% input$loyalty &
               data$"Gift frequency" %in% input$gift_frequency)%>%
      
      filter(first_gift_date >= input$first_gift_date[1])%>%
      filter(first_gift_date <= input$first_gift_date[2])%>%
      filter(first_gift_amount >= input$first_gift_min) %>%
      filter(first_gift_amount <= input$first_gift_max)%>%
      
      filter(mrc_date >= input$mrc_date[1])%>%
      filter(mrc_date <= input$mrc_date[2])%>%
      filter(mrc_amount >= input$mrc_min) %>%
      filter(mrc_amount <= input$mrc_max) %>%
      
      filter(hpc_date >= input$hpc_date[1])%>%
      filter(hpc_date <= input$hpc_date[2])%>%
      filter(hpc_amount >= input$hpc_min) %>%
      filter(hpc_amount <= input$hpc_max)
  })
  
  table_1 <- reactive({
    spread(count(filtered(), filtered()[input$cols[1]]),
           input$cols[1], n) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum, na.rm=TRUE),
                          across(where(is.factor), ~"Total"))) %>%
      mutate_all(~replace(., is.na(.), 0))
  })
  
  table_2 <- reactive({
    spread(count(filtered(), filtered()[input$cols[2]], filtered()[input$cols[1]]),
           input$cols[1], n) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum, na.rm=TRUE),
                          across(where(is.factor), ~"Total"))) %>%
      mutate_all(~replace(., is.na(.), 0))
  })
  
  table_3 <- reactive({
    spread(count(filtered(), filtered()[input$cols[3]], filtered()[input$cols[2]], filtered()[input$cols[1]]),
           input$cols[1], n) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum, na.rm=TRUE),
                          across(where(is.factor), ~"Total"))) %>%
      mutate_all(~replace(., is.na(.), 0))
  })
  
  table_4 <- reactive ({
    spread(count(filtered(), filtered()[input$cols[4]], filtered()[input$cols[3]], filtered()[input$cols[2]], filtered()[input$cols[1]]),
           input$cols[1], n) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum, na.rm=TRUE),
                          across(where(is.factor), ~"Total"))) %>%
      mutate_all(~replace(., is.na(.), 0))
  })
  
  output$table <- renderReactable({
    if (length(input$cols) == 1) {
      reactable(table_1(),
                bordered = TRUE,
                pagination = FALSE, 
                compact = TRUE,
                defaultColDef = colDef(
                  style = function(value) {
                    if (!is.numeric(value)) return()
                    normalized <- value/nrow(data)
                    color <- scale_color(normalized)
                    list(background = color)}
                )
      )
    }
    else if (length(input$cols) == 2) {
      reactable(table_2(),
                bordered = TRUE,
                pagination = FALSE,
                compact = TRUE,
                defaultColDef = colDef(
                  style = function(value) {
                    if (!is.numeric(value)) return()
                    normalized <- value/nrow(data)
                    color <- scale_color(normalized)
                    list(background = color)}
                )
      )
    }
    else if (length(input$cols) == 3) {
      #print(as.name(input$cols[3]))
      #thing = as.name(input$cols[2])
      reactable(table_3(),
                bordered = TRUE,
                pagination = FALSE,
                compact = TRUE,
                #groupBy = c(input$cols[3]),
                defaultColDef = colDef(
                  style = function(value) {
                    if (!is.numeric(value)) return()
                    normalized <- value/nrow(data)
                    color <- scale_color(normalized)
                    list(background = color)}#,
                  #aggregate = "sum",
                  #na="0"
                )
                
                
                # columns = list(
                #   thing = colDef(
                #     aggregate = "unique"
                #   )
                # )
      )
    }
    else if (length(input$cols) == 4) {
      reactable(table_4(),
                bordered = TRUE,
                pagination = FALSE,
                compact = TRUE,
                #groupBy = c(input$cols[4], input$cols[3]),
                defaultColDef = colDef(
                  #aggregate = "sum",
                  style = function(value) {
                    if (!is.numeric(value)) return()
                    normalized <- value/nrow(data)
                    color <- scale_color(normalized)
                    list(background = color)}
                )
                
      )
    }
    
  })
}

shinyApp(ui, server)

