#
#

library(googlesheets4)
library(tidyverse)
library(DT)
library(scales)
library(bslib)
library(thematic)
library(showtext)
library(shinyBS)
library(lubridate)

# gs4_auth(cache = ".secrets") #used to achieve the secrets file

gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) #use in shinyapps prod to connect to data
#gs4_auth(email = "")

##C3P0 Archive File
#Farmer Produce tab
sheet_id = "1xs8TAMrSsJuL_gou4y0DBH3IkaTH0eBn_pdboCGWFTI"
fp = range_read(sheet_id
                ,sheet = 'farm_produce'
                ,col_types = 'icccnnnnncDcDc'
)

#Master Item List tab
mil = range_read(sheet_id
                 ,sheet = 'master_item_list'
                 ,col_types = 'cccccccnccnDc'
)

#Share Rotation tab
sr = range_read(sheet_id
                ,sheet = 'share_rotation'
                ,col_types = 'Diciccc'
)

#Weekly Share Lists tab
wsl = range_read(sheet_id
                 ,sheet = 'weekly_share_lists'
                 ,col_types = 'iccicinDcDc'
)

#Share Numbers tab
sn = range_read(sheet_id
                ,sheet = 'share_numbers'
                ,col_types = 'icccicDcDc'
)

#Inventory Sales tab
# is = range_read(sheet_id
#                 ,sheet = 'inv_sales'
#                 ,col_types = 'iccnnccDcD'
# )

#Sales Only tab
so = range_read(sheet_id
                ,sheet = 'sales_only'
                ,col_types = 'icccDcDc'
)

#Share List tab
sl = range_read(sheet_id
                ,sheet = 'share_list'
                ,col_types = 'ccniDc'
)

#Farmer List tab
fl = range_read(sheet_id
                ,sheet = 'farmer_list'
                ,col_types = 'ccccccDc'
)

########## Delorean ##########
# Place holder
##############################

########## Present Day ##########
fp_max = fp %>%
  summarise(max = max(archive_date)) %>%
  pull

mil_max = mil %>%
  summarise(max = max(snapshot_date)) %>%
  pull

sl_max = sl %>%
  summarise(max = max(snapshot_date)) %>%
  pull

fl_max = fl %>%
  summarise(max = max(snapshot_date)) %>%
  pull

stopifnot(fp_max == mil_max)
stopifnot(fp_max == sl_max)
stopifnot(fp_max == fl_max)

milc = mil %>% filter(snapshot_date == fp_max)
slc = sl %>% filter(snapshot_date == fp_max)
flc = fl %>% filter(snapshot_date == fp_max)
##############################

## Data sets
#creating pivot table initial data set
fp_pivot_table = fp %>% 
  mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>% 
  filter(!is.na(availability)) 

#creating data set for time series (this could get large over time so potential efficiency improvement)
ts_init <- sr %>% #obtain full week list
  filter(date <= fp_max & group_id != "skip") %>% 
  select(season_week, date) %>% 
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
              group_by(item, season_week) %>% 
              summarise(availability = sum(availability),
                        order = sum(order)
                        ) %>%
              ungroup()
            , by = c('season_week','item')) %>% 
  mutate(availability = ifelse(is.na(availability), 0, availability),
         order = ifelse(is.na(order), 0, order)
         ) #fill in NAs with 0

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

#Parameter filter list for Matrix
matrix_parameter = c('Available','Ordered','Ordered / Available')

# setting an example item field for the Item Detail tab
ex_item = 'Broccoli'

##Theme Setup
# Setting Theme (https://shiny.rstudio.com/app-stories/weather-lookup-bslib.html)
my_theme <- bs_theme(bootswatch = "slate",
                     base_font = font_google("Roboto"))
# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = my_theme,
  
  # Application title
  div(id = "page-top",
      fluidRow(tags$img(src="logo.png", style = "width: 100px"), # width = "5%", height = "5%" # old R version
               column(3, radioButtons("current_theme", "App Theme:", c("Dark" = "slate", "Light" = "flatly"), inline = TRUE)
                      ),
               column(3, bsButton("showpanel", "Show/hide sidebar", type = "toggle", value = TRUE, size = "small")
                      )
      )
  ),
  div(
    id = "app-title",
    titlePanel(title="Fair Shares Item Information"
               ,tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"),
                         tags$title("FS Item Information"))
               ),
    
  ),
  
  tabsetPanel(
    tabPanel("Farmer Produce - Availability",
             uiOutput('ui')
    ),
    tabPanel("Produce Item Search",
             uiOutput('ui2')

    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$ui <- renderUI({
    if (input$showpanel) {
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("daterange","Date Range",fp_min, fp_max),        #date range
          selectInput("mparameter","Matrix Parameter",matrix_parameter),  #matrix parameter
          selectInput("item","Item*",item_list$item, multiple = TRUE),    #item list
          radioButtons("chart_type", "Chart Type", c("line", "bar"), "line"), #allow pick the chart type 
          radioButtons("date_aggr", "Date Aggregaton", c("week", "month"), "week"), #pick granularity of aggregation
          checkboxInput("combine_data", "Combine Items?",FALSE),           #allow to combine all items in all charts 
          h6("* The Time Series chart will only appear if an Item(s) is selected in the filter above.
                    The Item List is based on the Farmer Produce List."),
          actionButton("update_av", "Click to Show Charts")               #allows data to update or not
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          h6("Farm Name Notes:  * = Primary,   ** = Primary - Specialty,   *** = Secondary,   **** = Secondary - Specialty"),
          dataTableOutput("farmer_item_table"),  #farmer availability pivot table
          plotOutput("item_timeseries")          #item availability time series plots
        )
      )
    } else {
      mainPanel(
        h6("Farm Name Notes:  * = Primary,   ** = Primary - Specialty,   *** = Secondary,   **** = Secondary - Specialty"),
        dataTableOutput("farmer_item_table"),  #farmer availability pivot table
        plotOutput("item_timeseries")          #item availability time series plots
      )
    }
  })
  
  output$ui2 <- renderUI({
    if (input$showpanel) {
      sidebarLayout(
        sidebarPanel(
          selectInput("mitem","Item*",mitem_list$item, multiple = TRUE, selected = ex_item),
          actionButton("update_it", "Click to Show Results"),               #allows data to update or not
          h6("* The Item List is based on Items from the Master List with a filter on Category."),
          h6("In the 'Farmigo Sales' and 'Share Quantity' charts the column headers are populated as (season #).(week #)")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(fluidRow(
          splitLayout(cellWidths = c("50%", "50%"),
                      div(tableOutput("infotable"), style = "height:290px;weight:300px")
                      , div(tableOutput("costtable"), style = "height:290px;weight:300px")
          )
          ,style = "height:310px;"
        )
        ,fluidRow(dataTableOutput("farmigotable"),style = "height:200px;overflow-x: scroll;")
        ,fluidRow(dataTableOutput("grouptable"),style = "height:550px;overflow-x: scroll;")
        # ,verbatimTextOutput("test")
        )
      )
    } else {
      mainPanel(fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    div(tableOutput("infotable"), style = "height:290px;weight:300px")
                    , div(tableOutput("costtable"), style = "height:290px;weight:300px")
        )
        ,style = "height:310px;"
      )
      ,fluidRow(dataTableOutput("farmigotable"),style = "height:200px;overflow-x: scroll;")
      ,fluidRow(dataTableOutput("grouptable"),style = "height:550px;overflow-x: scroll;")
      # ,verbatimTextOutput("test")
      )
    }
  })
  
  
  
  
  ##############  Farmer Produce - Availability tab  ############## 
  
  fmatrix = reactiveValues(data = NULL)
  timeseries = reactiveValues(data = NULL)
  
  observeEvent(input$update_av, {
    if (is.null(input$item)) { #only update matrix if item filter is blank
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
    if (is.null(fmatrix$data)) { #only update time series chart if item filter is populated
      return()
    } else {
      if (input$mparameter == 'Available') {
        df <- fmatrix$data %>%
          group_by(farm, item) %>% 
          summarise(availability = sum(availability)) %>% 
          ungroup() %>% 
          filter(availability > 0) %>% 
          left_join(flc %>% 
                      select(farm, detail) %>% 
                      distinct()
                    , by = c('farm')
          ) %>% 
          mutate(farm = ifelse(is.na(detail), farm,
                               ifelse(detail == 'Primary', paste0(farm,'*'),
                                      ifelse(detail == 'Primary - Specialty', paste0(farm,'**'),
                                             ifelse(detail == 'Secondary', paste0(farm,'***'),
                                                    ifelse(detail == 'Secondary - Specialty', paste0(farm,'****'),farm)))))
          ) %>% 
          select(-detail) %>% 
          pivot_wider(names_from = farm, values_from = availability) %>%
          arrange(item) %>%
          rename(Item = item) %>%
          replace(is.na(.), 0)
        
        ## attempt at combining the dataframe. Had lots of issues so needs further testing! 
        # if (input$combine_date == TRUE) {
        #   df <- df %>% 
        #     rbind(
        #       df %>% 
        #         group_by(farm) %>% 
        #         summarise(availability = sum(availability)) %>% 
        #         ungroup() %>% 
        #         mutate(item = '__Combine All__')
        #     )
        # }

        dat <- datatable(df
                         , rownames = FALSE                      #remove row numbers
                         , class = 'cell-border stripe'          #add lines between rows/columns
                         , options = list(dom = 'litip')         #DT dropdown / filter / entry count settings
                         , caption = htmltools::tags$caption(
                           style = 'caption-side: top; text-align: center;'
                           ,'Available Amount'
                         )
                        ) %>% 
          formatStyle(
            names(df)
            , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
            , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
            , textAlign = 'center'
          )
        
      } else if (input$mparameter == 'Ordered') {
        df <- fmatrix$data %>%
          group_by(farm, item) %>% 
          summarise(order = sum(order)) %>% 
          ungroup() %>% 
          filter(order > 0) %>% 
          left_join(flc %>% 
                      select(farm, detail) %>% 
                      distinct()
                    , by = c('farm')
          ) %>% 
          mutate(farm = ifelse(is.na(detail), farm,
                               ifelse(detail == 'Primary', paste0(farm,'*'),
                                      ifelse(detail == 'Primary - Specialty', paste0(farm,'**'),
                                             ifelse(detail == 'Secondary', paste0(farm,'***'),
                                                    ifelse(detail == 'Secondary - Specialty', paste0(farm,'****'),farm)))))
          ) %>% 
          select(-detail) %>% 
          pivot_wider(names_from = farm, values_from = order) %>% 
          arrange(item) %>% 
          rename(Item = item) %>% 
          replace(is.na(.), 0)
        
        dat <- datatable(df
                         , rownames = FALSE                      #remove row numbers
                         , class = 'cell-border stripe'          #add lines between rows/columns
                         , options = list(dom = 'litip')         #DT dropdown / filter / entry count settings
                         , caption = htmltools::tags$caption(
                           style = 'caption-side: top; text-align: center;'
                           ,'Order Amount'
                         )
                         ) %>% 
          formatStyle(
            names(df)
            , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
            , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
            , textAlign = 'center'
          )
        
      } else if (input$mparameter == 'Ordered / Available') {
        df <- fmatrix$data %>%
          group_by(farm, item) %>% 
          summarise(order = sum(order),
                    availability = sum(availability)) %>% 
          mutate(oa_percent = round(ifelse((is.na(availability)| availability==0) & order > 0, 1,
                                     ifelse((is.na(availability)| availability==0) , 0, order / availability)),2) *100
                    ) %>%
          ungroup() %>% 
          filter(oa_percent != 0) %>% 
          left_join(flc %>% 
                      select(farm, detail) %>% 
                      distinct()
                    , by = c('farm')
          ) %>% 
          mutate(farm = ifelse(is.na(detail), farm,
                               ifelse(detail == 'Primary', paste0(farm,'*'),
                                    ifelse(detail == 'Primary - Specialty', paste0(farm,'**'),
                                        ifelse(detail == 'Secondary', paste0(farm,'***'),
                                             ifelse(detail == 'Secondary - Specialty', paste0(farm,'****'),farm)))))
          ) %>% 
          select(-detail) %>%
          select(-order, -availability) %>% 
          pivot_wider(names_from = farm, values_from = oa_percent) %>% 
          arrange(item) %>% 
          rename(Item = item) %>% 
          replace(is.na(.), 0) 
        
        dat <- datatable(df #%>% 
                           #mutate_at(vars(-Item), funs(. %>% round(2) %>% scales::percent())) #formatstyle not working correctly when scales::percent added
                         , rownames = FALSE                      #remove row numbers
                         , class = 'cell-border stripe'          #add lines between rows/columns
                         , options = list(dom = 'litip')         #DT dropdown / filter / entry count settings
                         , caption = htmltools::tags$caption(
                           style = 'caption-side: top; text-align: center;'
                           ,'Order / Available (%)'
                         )
                         ) %>%    
          formatStyle(
            names(df
                  )
            , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
            , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
            , textAlign = 'center'
          )

      } else {
        return()
      }
      
      return(dat)
    }
  })
  
  #item availability time series plots
  output$item_timeseries <- renderPlot({
    
    if (is.null(timeseries$data)) {
      return()
    } else {
      
      if (input$date_aggr == "month") {
        ts_date_aggr =  timeseries$data |> 
          mutate(date = floor_date(date, 'month'))
      } else {
        ts_date_aggr =  timeseries$data
      }
      
      if (input$combine_data == TRUE) {
        if (input$mparameter == 'Available') {
          ts_init <- ts_date_aggr |> 
            group_by(date) |> 
            summarise(value = sum(availability)) |> 
            ungroup()
          
        } else if (input$mparameter == 'Ordered') {
          ts_init <- ts_date_aggr |> 
            group_by(date) |> 
            summarise(value = sum(order)) |> 
            ungroup()
          
        } else if (input$mparameter == 'Ordered / Available') {
          ts_init <- ts_date_aggr |> 
            group_by(date) |> 
            summarise(availability = sum(availability),
                      order = sum(order)) |> 
            mutate(oa_percent = ifelse((is.na(availability)| availability==0) & order > 0, 1,
                                       ifelse((is.na(availability)| availability==0) , 0, order / availability)) 
            ) |>  
            select(date, value = oa_percent) 
          
        } else {
          return()
        }
        
        if (input$chart_type == "bar") {
          ts <- ts_init |> 
            ggplot(aes(x=date, y=value)) +
            geom_col(color = c('seagreen')) +
            ylab(input$mparameter)
        } else {
          ts <- ts_init |> 
            ggplot(aes(x=date, y=value)) +
            geom_point(color = c('seagreen')) +
            geom_line(color = c('seagreen')) +
            ylab(input$mparameter)
        }
        
        
        if (input$mparameter == 'Ordered / Available') {
          ts <- ts +
            scale_y_continuous(labels = scales::percent_format(scale = 100))
        }
      } else {
        if (input$mparameter == 'Available') {
          ts_init <- ts_date_aggr |> 
            group_by(date, item, per) |> 
            summarise(value = sum(availability)) |> 
            ungroup()
          
        } else if (input$mparameter == 'Ordered') {
          ts_init <- ts_date_aggr |> 
            group_by(date, item, per) |> 
            summarise(value = sum(order)) |> 
            ungroup()
          
        } else if (input$mparameter == 'Ordered / Available') {
          ts_init <- ts_date_aggr |> 
            group_by(date, item, per) |> 
            summarise(availability = sum(availability),
                      order = sum(order)) |> 
            ungroup() |> 
            mutate(oa_percent = ifelse((is.na(availability)| availability==0) & order > 0, 1,
                                       ifelse((is.na(availability)| availability==0) , 0, order / availability))
            ) |>  
            select(date, item, per, value = oa_percent) 
          
        } else {
          return()
        }
        
        if (input$chart_type == "bar") {
          ts <- ts_init |> 
            filter(item %in% input$item & date >= format(input$daterange[1]) & date <= format(input$daterange[2])) |> 
            ggplot(aes(x=date, y=value)) +
            geom_col(color = c('seagreen')) +
            facet_grid(item + per~.) +
            ylab(input$mparameter)
        } else {
          ts <- ts_init |> 
            filter(item %in% input$item & date >= format(input$daterange[1]) & date <= format(input$daterange[2])) |> 
            ggplot(aes(x=date, y=value)) +
            geom_point(color = c('seagreen')) +
            geom_line(color = c('seagreen')) +
            facet_grid(item + per~.) +
            ylab(input$mparameter)
        }

        if (input$mparameter == 'Ordered / Available') {
          ts <- ts +
            scale_y_continuous(labels = scales::percent_format(scale = 100))
        }
      }

      return(ts)
    }
  })
  
  
  
  ##############  Produce Item Search tab  ############## 
  
  item_button_df = reactiveValues(data = milc %>%
                                    filter(item %in% ex_item))
  item_button_df_fp = reactiveValues(data = fp %>%
                                       filter(item %in% ex_item))
  item_button_df_so = reactiveValues(data = so %>%
                                       filter(item %in% ex_item))
  item_button_df_wsl = reactiveValues(data = wsl %>%
                                        filter(item %in% ex_item))
  
  # Only update tables if button pressed
  observeEvent(input$update_it, {
    if (is.null(input$mitem)) { #only update data frame if item filter is not blank
      item_button_df$data = milc %>%
        filter(item %in% "ahksdaobohafb") #using gibberish so that no data is returned
      item_button_df_fp$data = fp %>%
        filter(item %in% "ahksdaobohafb") #using gibberish so that no data is returned
      item_button_df_so$data = so %>%
        filter(item %in% "ahksdaobohafb") #using gibberish so that no data is returned
      item_button_df_wsl$data = wsl %>%
        filter(item %in% "ahksdaobohafb") #using gibberish so that no data is returned
    } else {
      item_button_df$data = milc %>%
        filter(item %in% input$mitem)
      item_button_df_fp$data = fp %>%
        filter(item %in% input$mitem)
      item_button_df_so$data = so %>%
        filter(item %in% input$mitem)
      item_button_df_wsl$data = wsl %>%
        filter(item %in% input$mitem)
    }
    
  })
  
  
  output$infotable <- renderTable({
    item_button_df$data %>% 
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
      rename(Category = category, Item = item, Per = per) %>% 
      select(-n) %>% 
      pivot_longer(!Item, names_to = "Names", values_to ="Variables") %>% 
      mutate(Item = ifelse(Names == 'Category', Item, NA)) %>% 
      replace(is.na(.), '')
  }) 
  
  output$costtable <- renderTable({
    item_button_df_fp$data %>% 
      mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
      filter(!is.na(availability) & !is.na(cost)) %>% 
      group_by(item, cost) %>% 
      summarise(count = n()) %>% 
      arrange(item, desc(cost)) %>% 
      ungroup() %>% 
      left_join(item_button_df_fp$data %>% 
                  mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
                  filter(!is.na(availability) & !is.na(cost)) %>% 
                  group_by(item) %>% 
                  summarise(ttl_count = n()) %>% 
                  ungroup()
                , by = c('item')
      ) %>% 
      left_join(item_button_df_fp$data %>% 
                  mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
                  filter(!is.na(availability) & !is.na(cost)) %>% 
                  group_by(item) %>% 
                  summarise(max_cost = max(cost)) %>% 
                  ungroup()
                , by = c('item')
      ) %>% 
      mutate('Percent Total' = count / ttl_count,
             item = ifelse(max_cost == cost, item, '')) %>% 
      select(-ttl_count, -max_cost) %>% 
      mutate_at(vars(-item,-count,-'Percent Total'), funs(. %>% round(2) %>% scales::dollar())) %>% 
      mutate_at(vars(-item,-count,-cost), funs(. %>% round(2) %>% scales::percent())) %>% 
      rename(Item = item, Cost = cost, Count = count)
      
  })
  
  output$farmigotable <- renderDataTable({
    dt <- item_button_df$data %>% 
      select(item) %>% 
      distinct() %>% 
      full_join(sr %>% #obtain full week list
                  filter(date <= fp_max & group_id != "skip") %>% 
                  select(season_week) %>% 
                  distinct()
                , by = character()
      ) %>% 
      left_join(item_button_df_so$data %>%  
                  filter(sold != '') %>% 
                  select(season_week, item, sold) %>% 
                  distinct()
                , by = c('season_week','item')
      ) %>% 
      arrange(desc(season_week)) %>% 
      mutate(item = str_replace_all(str_replace_all(item,' ', ''),'-','_'),
             season_week = as.character(season_week),
             sold = as.character(sold),
             '_' = NA) %>% 
      rename(Item = item) %>% 
      select('_', Item, season_week, sold)
    
    dt_ft = dt %>% 
      pivot_wider(names_from = season_week, values_from = sold) %>% 
      replace(is.na(.), '') 
    
    dat <- datatable(dt_ft
                     , rownames = FALSE                      #remove row numbers
                     , class = 'cell-border stripe'          #add lines between rows/columns
                     , caption = htmltools::tags$caption(
                       style = 'caption-side: top; text-align: left;'
                       ,'Farmigo Sales'
                     )
                     , options = list(dom = 't'
                                      , pageLength = 1000)
                     ) %>%    
      formatStyle(
        names(dt %>% 
                mutate(sold = ifelse(sold != '', 1, 0)) %>% 
                pivot_wider(names_from = season_week, values_from = sold) %>% 
                mutate(Item = NA) %>% 
                replace(is.na(.), 0)
        )
        , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
        , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
      )
    return(dat)
  }) 
  
  output$grouptable <- renderDataTable({
    dt <- slc %>% #start with full group list (always)
      filter(!is.na(group_id)) %>% 
      select(group_id) %>% 
      distinct() %>%
      left_join(item_button_df$data %>% #join in the items that have been in shares to associated groups
                  select(item) %>% 
                  distinct() %>% 
                  inner_join(item_button_df_wsl$data %>%  
                               filter(!is.na(item) & !is.na(amt_share) & !is.na(prcnt_amnt)) %>% 
                               select(item, group_id) %>% 
                               distinct()
                             , by = c('item')
                             )
                , by = c('group_id')
                ) %>% 
      full_join(sr %>% #obtain full week list
                  filter(date <= fp_max & group_id != "skip") %>% 
                  select(season_week) %>% 
                  distinct()
                , by = character()
      ) %>% 
      left_join(item_button_df_wsl$data %>%  #bring in member information
                  filter(!is.na(item) & !is.na(amt_share) & !is.na(prcnt_amnt)) %>% 
                  left_join(sn %>% 
                              group_by(season_week, group_id) %>% 
                              summarise(members = sum(members)) %>% 
                              ungroup()
                            , by = c('season_week','group_id')
                  ) %>% 
                  mutate(member_actual = ceiling(prcnt_amnt*members),
                         member_val = ifelse(member_actual != members, 1, 0)
                  ) %>% 
                  group_by(season_week, item, group_id) %>% 
                  summarise(member_actual = sum(member_actual),
                            member_val = sum(member_val),
                            ttl_members = max(members)) %>% 
                  ungroup() 
                , by = c('season_week','item','group_id')
      ) 
    dt_gt <- dt %>% 
      mutate(member_prct = round((member_actual / ttl_members)*100),
        member_val = ifelse(member_val > 0, paste0(member_actual, '(',member_prct,'%)'), member_actual)
        ) %>% 
      select(season_week, group_id, item, member_val) %>% 
      arrange(desc(season_week)) %>% 
      # select(-item) %>% 
      mutate(season_week = as.character(season_week),
             member_val = as.character(member_val)) %>% 
      pivot_wider(names_from = season_week, values_from = member_val) %>% 
      arrange(group_id, item) %>% 
      mutate(item = str_replace_all(str_replace_all(item,' ', ''),'-','_')) %>% 
      rename(G = group_id, Item = item) %>% 
      replace(is.na(.), '')
    
    dat <- datatable(dt_gt
                     , rownames = FALSE                      #remove row numbers
                     , class = 'cell-border stripe'          #add lines between rows/columns
                     , caption = htmltools::tags$caption(
                       style = 'caption-side: top; text-align: left;'
                       ,'Share Quantity - (%) means only for percentage of members'
                     )
                     , options = list(dom = 't'
                                      , pageLength = 1000)
                     ) %>% 
      formatStyle(
        names(dt %>%
                select(season_week, group_id, item, member_val = member_actual) %>%
                arrange(desc(season_week)) %>%
                # select(-item) %>%
                mutate(season_week = as.character(season_week),
                       member_val = as.integer(member_val)) %>%
                pivot_wider(names_from = season_week, values_from = member_val) %>%
                arrange(group_id, item) %>%
                mutate(item = NA) %>% 
                rename(G = group_id, Item = item) %>%
                replace(is.na(.), 0)
        )
        , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
        , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
      )
    return(dat)
  }) 
  
  # output$test = renderPrint(length(input$mitem))
  
  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
