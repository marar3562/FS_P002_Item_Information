#
# 

library(googlesheets4)
library(tidyverse)
library(DT)
library(scales)
library(bslib)
library(thematic)
library(showtext)

#gs4_auth(cache = ".secrets") #used to achieve the secrets file

gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) #use in shinyapps prod to connect to data
#gs4_auth(email = "")

##C3P0 Archive File
#Farmer Produce tab
sheet_id = "1xs8TAMrSsJuL_gou4y0DBH3IkaTH0eBn_pdboCGWFTI"
fp = range_read(sheet_id
                ,sheet = 'farm_produce'
                ,col_types = 'icccnnnnncDcD'
)

#Master Item List tab
mil = range_read(sheet_id
                 ,sheet = 'master_item_list'
                 ,col_types = 'cccccccnccDc'
)

#Share Rotation tab
sr = range_read(sheet_id
                ,sheet = 'share_rotation'
                ,col_types = 'Dicicc'
)

#Weekly Share Lists tab
wsl = range_read(sheet_id
                 ,sheet = 'weekly_share_lists'
                 ,col_types = 'iccicinDcD'
)

#Share Numbers tab
sn = range_read(sheet_id
                ,sheet = 'share_numbers'
                ,col_types = 'icccicDcD'
)

#Inventory Sales tab
is = range_read(sheet_id
                ,sheet = 'inv_sales'
                ,col_types = 'iccnnccDcD'
)

########## Delorean ##########
# Place holder
##############################

########## Present Day ##########
fp_max = fp %>%
  summarise(max = max(archive_date)) %>%
  pull

milc = mil %>% filter(snapshot_date == fp_max)
##############################

## Data sets
#creating pivot table initial data set
fp_pivot_table = fp %>% 
  mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>% 
  filter(!is.na(availability)) 

#creating data set for time series (this could get large over time so potential efficiency improvement)
ts_init <- sr %>% #obtain full week list
  filter(date <= fp_max & group_id != "skip") %>% 
  select(week, date) %>% 
  distinct() %>% 
  full_join(fp %>% #cross join weeks with full Item list
              filter(av_min > 0 | av_max > 0) %>% 
              select(item) %>% 
              distinct() %>% 
              left_join(milc %>% #adding in Per value by Item
                          select(item,per = preferred_per_value) %>% 
                          distinct()
                        , by = c('item')
              ) %>% 
              arrange(item, per)
            , by = character()) %>% 
  left_join(fp_pivot_table %>% #bring in availability for each Item
              group_by(item, week) %>% 
              summarise(availability = sum(availability)) %>%
              ungroup()
            , by = c('week','item')) %>% 
  mutate(availability = ifelse(is.na(availability), 0, availability)) #fill in NAs with 0

##Filter variables 
#obtaining item list for filter
item_list = fp %>% 
  filter(av_min > 0 | av_max > 0) %>%
  select(item) %>% 
  distinct() %>% 
  arrange(item)

#determine min date value for date range filter
fp_min = fp_pivot_table %>%
  summarise(min = min(date)) %>%
  pull

#obtaining master item list for filter (more inclusive, even if no availability in farmer produce)
mitem_list = milc %>% 
  filter(category %in% c('Produce - Vegetables','Produce - Fruits')) %>% 
  select(item) %>% 
  distinct() %>% 
  arrange(item)

##Theme Setup
# Setting Theme (https://shiny.rstudio.com/app-stories/weather-lookup-bslib.html)
my_theme <- bs_theme(bootswatch = "flatly",
                     base_font = font_google("Roboto"))
# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = my_theme,
  
  # Application title
  div(id = "page-top",
      fluidRow(img(src="logo.png", height="5%", width="5%"),
               column(3, radioButtons("current_theme", "App Theme:", c("Light" = "flatly", "Dark" = "slate"), inline = TRUE))
      )
  ),
  div(
    id = "app-title",
    titlePanel(title="Fair Shares Item Information"
               ,tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"),
                         tags$title("FS Item Information"))
               )
  ),
  
  tabsetPanel(
    tabPanel("Farmer Produce - Availability",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("daterange","Date Range",fp_min, fp_max),     #date range
                 selectInput("item","Item",item_list$item, multiple = TRUE),  #item list
                 actionButton("update", "Click to Show Charts")               #allows data to update or not
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 dataTableOutput("farmer_item_table"),  #farmer availability pivot table
                 plotOutput("item_timeseries")          #item availability time series plots
               )
             )
    ),
    tabPanel("Produce Item Search",
             sidebarLayout(
               sidebarPanel(
                 selectInput("mitem","Item",mitem_list$item)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"), tableOutput("infotable"),tableOutput("costtable"))
               )
               ,fluidRow(dataTableOutput("farmigotable"),style = "height:175px; overflow-y: scroll;overflow-x: scroll;")
               ,fluidRow(dataTableOutput("grouptable"),style = "height:525px; overflow-y: scroll;overflow-x: scroll;")
               )
             )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ##############  Farmer Produce - Availability tab  ############## 
  
  fmatrix = reactiveValues(data = NULL)
  timeseries = reactiveValues(data = NULL)
  
  observeEvent(input$update, {
    if (is.null(input$item)) {
      fmatrix$data = fp_pivot_table %>%
        filter(date >= format(input$daterange[1]) & date <= format(input$daterange[2]))
      timeseries$data = NULL
    } else {
      fmatrix$data = fp_pivot_table %>%
        filter(item %in% input$item & date >= format(input$daterange[1]) & date <= format(input$daterange[2]))
      timeseries$data = ts_init %>%
        filter(item %in% input$item & date >= format(input$daterange[1]) & date <= format(input$daterange[2]))
      
    }
    
  })
  
  output$farmer_item_table <- renderDataTable({
    if (is.null(fmatrix$data)) {
      return()
    } else {
      df <- fmatrix$data %>%
        group_by(farm, item) %>% 
        summarise(availability = sum(availability)) %>% 
        ungroup() %>% 
        filter(availability > 0) %>% 
        pivot_wider(names_from = farm, values_from = availability) %>% 
        arrange(item) %>% 
        replace(is.na(.), 0)
      dat <- datatable(df
                       , rownames = FALSE                      #remove row numbers
                       , class = 'cell-border stripe'          #add lines between rows/columns
                       , options = list(dom = 'litip')) %>%    #DT dropdown / filter / entry count settings
        formatStyle(
          names(df)
          , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
          , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
          , textAlign = 'center'
        )
      
      return(dat)
    }
  })
  
  #item availability time series plots
  output$item_timeseries <- renderPlot({
    
    if (is.null(timeseries$data)) {
      return()
    } else {
      ts <- timeseries$data %>%
        filter(item %in% input$item & date >= format(input$daterange[1]) & date <= format(input$daterange[2])) %>%
        ggplot(aes(x=date, y=availability)) +
          geom_point(color = c('seagreen')) +
          geom_line(color = c('seagreen')) +
          facet_grid(item + per~.)
      
      return(ts)
    }
  })
  
  
  
  ##############  Produce Item Search tab  ############## 
  output$infotable <- renderTable({
    milc %>% 
      filter(item == input$mitem) %>% 
      select(item, category, per = preferred_per_value, Note = farmer_dashboard_notes) %>% 
      mutate(n = row_number(),
             Note = ifelse(is.na(Note), 'FILL ME IN PLEASE!!!', Note)) %>%
      left_join(fp %>% 
                  mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
                  filter(!is.na(availability) & !is.na(cost)) %>% 
                  group_by(item) %>% 
                  summarise('Average Cost' = mean(cost)
                            ,'75th % Cost' = quantile(cost, 0.75)
                            ,'50th % Cost' = median(cost)
                            ,'25th % Cost' = quantile(cost, 0.25)
                  ) %>% 
                  ungroup() %>% 
                  mutate_at(vars(-item), funs(. %>% round(2) %>% scales::dollar()))
                , by = c('item')) %>% 
      select(-item) %>% 
      pivot_longer(!n, names_to = "Names", values_to ="Variables") %>% 
      select(-n) %>% 
      replace(is.na(.), '')
  }) 
  
  output$costtable <- renderTable({
    fp %>% 
      mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
      filter(!is.na(availability) & !is.na(cost)) %>% 
      filter(item == input$mitem) %>% 
      group_by(item, cost) %>% 
      summarise(count = n()) %>% 
      arrange(item, desc(cost)) %>% 
      ungroup() %>% 
      left_join(fp %>% 
                  mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
                  filter(item == input$mitem) %>% 
                  filter(!is.na(availability) & !is.na(cost)) %>% 
                  group_by(item) %>% 
                  summarise(ttl_count = n()) %>% 
                  ungroup()
                , by = c('item')
      ) %>% 
      mutate('Percent Total' = count / ttl_count) %>% 
      select(-item,-ttl_count) %>% 
      mutate_at(vars(-count,-'Percent Total'), funs(. %>% round(2) %>% scales::dollar())) %>% 
      mutate_at(vars(-count,-cost), funs(. %>% round(2) %>% scales::percent())) %>% 
      rename(Cost = cost, Count = count)
  })
  
  output$farmigotable <- renderDataTable({
    dt <- milc %>% 
      filter(item == input$mitem) %>%
      select(item) %>% 
      distinct() %>% 
      full_join(sr %>% #obtain full week list
                  filter(date <= fp_max & group_id != "skip") %>% 
                  select(week) %>% 
                  distinct()
                , by = character()
      ) %>% 
      left_join(is %>%  
                  filter(sold != '') %>% 
                  filter(item == input$mitem) %>%
                  select(week, item, sold) %>% 
                  distinct()
                , by = c('week','item')
      ) %>% 
      arrange(desc(week)) %>% 
      mutate(item = '') %>% 
      rename('_________' = item) %>% 
      mutate(week = as.character(week),
             sold = as.character(sold)) 
    dt_ft = dt %>% 
      pivot_wider(names_from = week, values_from = sold) %>% 
      replace(is.na(.), '')
    
    dat <- datatable(dt_ft
                     , rownames = FALSE                      #remove row numbers
                     , class = 'cell-border stripe'          #add lines between rows/columns
                     , caption = htmltools::tags$caption(
                       style = 'caption-side: top; text-align: left;'
                       ,'Farmigo Sales'
                     )
                     , options = list(dom = 't')) %>%    
      formatStyle(
        names(dt %>% 
                mutate(sold = ifelse(sold != '', 1, 0)) %>% 
                pivot_wider(names_from = week, values_from = sold) %>% 
                replace(is.na(.), 0)
        )
        , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
        , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
      )
    return(dat)
  }) 
  
  output$grouptable <- renderDataTable({
    dt <- milc %>% 
      filter(item == input$mitem) %>%
      select(item) %>% 
      distinct() %>% 
      full_join(sr %>% #obtain full week list
                  filter(date <= fp_max & group_id != "skip") %>% 
                  select(week) %>% 
                  distinct()
                , by = character()
      ) %>% 
      inner_join(wsl %>% #obtain full group list
                   filter(!is.na(item)) %>% 
                   select(week, group_id) %>% 
                   distinct()
                 , by = c('week')
      ) %>% 
      left_join(wsl %>%  
                  filter(!is.na(item) & !is.na(amt_share) & !is.na(prcnt_amnt)) %>% 
                  filter(item == input$mitem) %>%
                  left_join(sn %>% 
                              group_by(week, group_id) %>% 
                              summarise(members = sum(members)) %>% 
                              ungroup()
                            , by = c('week','group_id')
                  ) %>% 
                  mutate(member_actual = ceiling(prcnt_amnt*members),
                         member_val = ifelse(member_actual != members, 1, 0)
                  ) %>% 
                  group_by(week, item, group_id) %>% 
                  summarise(member_actual = sum(member_actual),
                            member_val = sum(member_val)) %>% 
                  ungroup() 
                , by = c('week','item','group_id')
      ) 
    dt_gt <- dt %>% 
      mutate(member_val = ifelse(member_val > 0, paste0(member_actual, '_(%)'), member_actual)) %>% 
      select(week, item, group_id, member_val) %>% 
      arrange(desc(week)) %>% 
      select(-item) %>% 
      mutate(week = as.character(week),
             member_val = as.character(member_val)) %>% 
      pivot_wider(names_from = week, values_from = member_val) %>% 
      arrange(group_id) %>% 
      rename(Group_Id = group_id) %>% 
      replace(is.na(.), '')
    
    dat <- datatable(dt_gt
                     , rownames = FALSE                      #remove row numbers
                     , class = 'cell-border stripe'          #add lines between rows/columns
                     , caption = htmltools::tags$caption(
                       style = 'caption-side: top; text-align: left;'
                       ,'Share Quantity - (%) means only for percentage of members'
                     )
                     , options = list(dom = 't')) %>%    
      formatStyle(
        names(dt %>% 
                select(week, item, group_id, member_val = member_actual) %>% 
                arrange(desc(week)) %>% 
                select(-item) %>% 
                mutate(week = as.character(week),
                       member_val = as.integer(member_val)) %>% 
                pivot_wider(names_from = week, values_from = member_val) %>% 
                arrange(group_id) %>% 
                rename(Group_Id = group_id) %>% 
                replace(is.na(.), 0)
        )
        , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
        , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
      )
    return(dat)
  }) 
  
  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
