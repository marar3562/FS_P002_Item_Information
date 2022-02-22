#
# 

library(shiny)
library(tidyverse)
library(googlesheets4)
library(DT)

#gs4_auth(cache = ".secrets") #used to achieve the secrets file

gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) #use in shinyapps prod to connect to data
#gs4_auth(email = "")

##Initial load of data sets from Google Drive
#Farmer Produce tab
fp = range_read("1m45w0hQOkvUFvNpc5MwQcN4snGs0Lkrb6kJfOUJf6tw"
                ,sheet = 'farm_produce'
                ,skip = 1
                ,col_types = 'icccnnnnncD'
)

#Master Item List tab
mil = range_read("1m45w0hQOkvUFvNpc5MwQcN4snGs0Lkrb6kJfOUJf6tw"
                 ,sheet = 'master_item_list'
                 ,col_types = 'cccccccnccD'
)

#Share Rotation tab
sr = range_read("1m45w0hQOkvUFvNpc5MwQcN4snGs0Lkrb6kJfOUJf6tw"
                ,sheet = 'share_rotation'
                ,col_types = '----Dicic----------'
)

##Date checks plus fp_max used in date range filter
# finding max archive date
fp_max = fp %>%
  summarise(max = max(`Archive Date`)) %>%
  pull

# finding max snapshot date
mil_max = mil %>%
  summarise(max = max(`Snapshot Date`)) %>%
  pull

#making sure max dates match (if not, then archive process not successful)
stopifnot(fp_max == mil_max)

#only using the latest Master Item List data set
milc = mil %>% filter(`Snapshot Date` == fp_max)

## Data sets
#creating pivot table initial data set
fp_pivot_table = fp %>% 
  mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>% 
  filter(!is.na(availability)) %>% 
  left_join(sr %>% #obtain week date
              rename(Week = 'Week #') %>% 
              select(Week, Date) %>% 
              distinct()
            , by = c('Week')
  ) 

#creating data set for time series (this could get large over time so potential efficiency improvement)
ts_init <- sr %>% #obtain full week list
  rename(Week = 'Week #') %>% 
  filter(Date <= fp_max & Group_Id != "skip") %>% 
  select(Week, Date) %>% 
  distinct() %>% 
  full_join(fp %>% #cross join weeks with full Item list
              filter(!is.na(av_min) | !is.na(av_max)) %>% 
              select(Item) %>% 
              distinct() %>% 
              left_join(milc %>% #adding in Per value by Item
                          select(Item,Per = Preferred_Per_Value) %>% 
                          distinct()
                        , by = c('Item')
              ) %>% 
              arrange(Item, Per)
            , by = character()) %>% 
  left_join(fp %>% #bring in availability for each Item
              mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>% 
              filter(!is.na(availability)) %>% 
              group_by(Item, Week) %>% 
              summarise(availability = sum(availability)) %>%
              ungroup()
            , by = c('Week','Item')) %>% 
  mutate(availability = ifelse(is.na(availability), 0, availability)) #fill in NAs with 0

##Filter variables 
#obtaining item list for filter
item_list = fp %>% 
  filter(!is.na(av_min) | !is.na(av_max)) %>%
  select(Item) %>% 
  distinct() %>% 
  arrange(Item)

#determine min date value for date range filter
fp_min = fp_pivot_table %>%
  summarise(min = min(Date)) %>%
  pull

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Farmer Produce - Availability"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          dateRangeInput("daterange","Date Range",fp_min, fp_max),     #date range
          selectInput("item","Item",item_list$Item, multiple = TRUE),  #item list
          actionButton("update", "Update Table")                       #may want to replace with Submit button
        ),

        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput("farmer_item_table"),  #farmer availability pivot table
          plotOutput("item_timeseries")          #item availability time series plots
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #setting reactive variables
  values <- reactiveValues()
  values_orig <- reactiveValues()
  timeseries <- reactiveValues()
  timeseries_final <- reactiveValues()
  
  #putting initial data in dataframe variables
  values$df <- fp_pivot_table
  values_orig$df <- fp_pivot_table
  timeseries$df <- ts_init
  timeseries_final$df <- data.frame(Item = NA, Per = NA, Date = NA, availability = NA)

  #update data sets based on filters, once user clicks button
  newEntry <- observe({
    if(input$update > 0) {
      isolate(values$df <- values_orig$df %>% 
                filter(Item %in% input$item & Date >= format(input$daterange[1]) & Date <= format(input$daterange[2])) 
              )
    }
    if(input$update > 0 & !is.na(input$item)) {
      isolate(timeseries_final$df <- timeseries$df %>% 
                filter(Item %in% input$item)
      )
    }
    if(input$update > 0 & !is.na(timeseries_final$df %>% select(Item) %>% pull())) {
      isolate(timeseries_final$df <- timeseries_final$df %>% 
                filter(Date >= format(input$daterange[1]) & Date <= format(input$daterange[2]))
              )
    }
  })
  
  #farmer availability pivot table
  output$farmer_item_table <- renderDataTable({
    dat <- datatable(values$df %>% 
                       group_by(Farm, Item) %>% 
                       summarise(availability = sum(availability)) %>% 
                       ungroup() %>% 
                       pivot_wider(names_from = Farm, values_from = availability) %>% 
                       arrange(Item) %>% 
                       replace(is.na(.), 0)
                       , rownames = FALSE                      #remove row numbers
                       , class = 'cell-border stripe'          #add lines between rows/columns
                       , options = list(dom = 'litip')) %>%    #DT dropdown / filter / entry count settings
      formatStyle(
        names(values$df %>%
                group_by(Farm, Item) %>% 
                summarise(availability = sum(availability)) %>% 
                ungroup() %>% 
                pivot_wider(names_from = Farm, values_from = availability) %>%
                select(-Item))
        , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
        , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
        , textAlign = 'center'
        
        
      )
    return(dat)
    })
  
  #item availability time series plots
  output$item_timeseries <- renderPlot({
    timeseries_final$df %>% 
      ggplot(aes(x=Date, y=availability)) +
      geom_point(color = c('seagreen')) +
      geom_line(color = c('seagreen')) +
      facet_grid(Item + Per~.)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
