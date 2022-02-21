#
# 

library(shiny)
library(tidyverse)
library(googlesheets4)
library(DT)

#gs4_auth(cache = ".secrets") #used to achieve the secrets file

gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) #use in shinyapps prod to connect to data
#gs4_auth(email = "")

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

fp_max = fp %>%
  summarise(max = max(`Archive Date`)) %>%
  pull

mil_max = mil %>%
  summarise(max = max(`Snapshot Date`)) %>%
  pull

stopifnot(fp_max == mil_max)

milc = mil %>% filter(`Snapshot Date` == fp_max)

item_list = fp %>% 
  filter(!is.na(av_min) | !is.na(av_max)) %>%
  select(Item) %>% 
  distinct() %>% 
  arrange(Item)

fp_pivot_table = fp %>% 
  filter(!is.na(av_min) | !is.na(av_max)) %>% 
  group_by(Farm, Item) %>% 
  summarise(min_avail = sum(av_min)
            , max_avail = sum(av_max)
  ) %>% 
  mutate(availability = ifelse(is.na(max_avail), min_avail, max_avail)) %>% 
  ungroup() %>% 
  select(-min_avail, -max_avail) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Farmer Produce"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("item","Item",item_list$Item, multiple = TRUE),  #item_list
          actionButton("update", "Update Table")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput("farmer_item_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  values <- reactiveValues()
  values_orig <- reactiveValues()
  
  values$df <- fp_pivot_table
  values_orig$df <- fp_pivot_table
  
  newEntry <- observe({
    if(input$update > 0) {
      isolate(values$df <- values_orig$df %>% filter(Item %in% input$item & !is.na(availability)) )
    }
  })
  output$farmer_item_table <- renderDataTable({
    dat <- datatable(values$df %>% 
                       pivot_wider(names_from = Farm, values_from = availability) %>% 
                       arrange(Item) %>% 
                       replace(is.na(.), 0)
                       , options = list(dom = 'litip')) %>% 
      formatStyle(
        # 'Yoder',
        names(values$df %>%
                pivot_wider(names_from = Farm, values_from = availability) %>%
                select(-Item)),
        backgroundColor = styleInterval(c(1), c('azure2', 'seagreen')),
        textAlign = 'center'
      )
    return(dat)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
