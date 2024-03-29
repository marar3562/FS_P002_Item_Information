#
# 

library(googlesheets4)
library(tidyverse)
library(DT)
library(scales)
library(bslib)
library(thematic)

#gs4_auth(cache = ".secrets") #used to achieve the secrets file

gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) #use in shinyapps prod to connect to data
#gs4_auth(email = "")


#Farmer Produce tab
sheet_id = "1m45w0hQOkvUFvNpc5MwQcN4snGs0Lkrb6kJfOUJf6tw"
fp = range_read(sheet_id
                ,sheet = 'farm_produce'
                ,skip = 1
                ,col_types = 'icccnnnnncD'
)

#Master Item List tab
mil = range_read(sheet_id
                 ,sheet = 'master_item_list'
                 ,col_types = 'cccccccnccD'
)

#Share Rotation tab
sr = range_read(sheet_id
                ,sheet = 'share_rotation'
                ,col_types = '----Dicic----------'
)

#Weekly Share Lists tab
wsl = range_read(sheet_id
                 ,sheet = 'weekly_share_lists'
                 ,col_types = 'iccicin--D'
)

#Share Numbers tab
sn = range_read(sheet_id
                ,sheet = 'share_numbers'
                ,col_types = 'icccicD-'
)

#Inventory Sales tab
is = range_read(sheet_id
                ,sheet = 'inv_sales'
                ,col_types = 'iccnnccD--'
)


fp_max = fp %>%
  summarise(max = max(`Archive Date`)) %>%
  pull

mil_max = mil %>%
  summarise(max = max(`Snapshot Date`)) %>%
  pull

milc = mil %>% filter(`Snapshot Date` == fp_max)

mitem_list = milc %>% 
  filter(Category %in% c('Produce - Vegetables','Produce - Fruits')) %>% 
  select(Item) %>% 
  distinct() %>% 
  arrange(Item)

# Setting Theme (https://shiny.rstudio.com/app-stories/weather-lookup-bslib.html)
my_theme <- bs_theme(bootswatch = "cerulean")
# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = my_theme,
  
    # Application title
    titlePanel("Produce Item Search"),
    
    tabsetPanel(
      tabPanel("tab 1",
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "slate")),
                   selectInput("mitem","Item",mitem_list$Item)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), tableOutput("infotable"),tableOutput("costtable"))
                 )
                 ,fluidRow(dataTableOutput("farmigotable"),style = "height:120px; overflow-y: scroll;overflow-x: scroll;")
                 ,fluidRow(dataTableOutput("grouptable"),style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                 )
               )
               )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  

    output$infotable <- renderTable({
      milc %>% 
        filter(Item == input$mitem) %>% 
        select(Item, Category, Per = Preferred_Per_Value, Note = `Farmer Dashboard Notes`) %>% 
        mutate(n = row_number(),
               Note = ifelse(is.na(Note), 'FILL ME IN PLEASE!!!', Note)) %>%
        left_join(fp %>% 
                    mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
                    filter(!is.na(availability) & !is.na(Cost)) %>% 
                    group_by(Item) %>% 
                    summarise('Average Cost' = mean(Cost)
                              ,'75th % Cost' = quantile(Cost, 0.75)
                              ,'50th % Cost' = median(Cost)
                              ,'25th % Cost' = quantile(Cost, 0.25)
                    ) %>% 
                    ungroup() %>% 
                    mutate_at(vars(-Item), funs(. %>% round(2) %>% scales::dollar()))
                  , by = c('Item')) %>% 
        select(-Item) %>% 
        pivot_longer(!n, names_to = "Names", values_to ="Variables") %>% 
        select(-n) %>% 
        replace(is.na(.), '')
    }) 
    
    output$costtable <- renderTable({
      fp %>% 
        mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
        filter(!is.na(availability) & !is.na(Cost)) %>% 
        filter(Item == input$mitem) %>% 
        group_by(Item, Cost) %>% 
        summarise(Count = n()) %>% 
        arrange(Item, desc(Cost)) %>% 
        ungroup() %>% 
        left_join(fp %>% 
                    mutate(availability = ifelse(is.na(av_max), av_min, av_max)) %>%
                    filter(Item == input$mitem) %>% 
                    filter(!is.na(availability) & !is.na(Cost)) %>% 
                    group_by(Item) %>% 
                    summarise(ttl_count = n()) %>% 
                    ungroup()
                  , by = c('Item')
        ) %>% 
        mutate('Percent Total' = Count / ttl_count) %>% 
        select(-Item,-ttl_count) %>% 
        mutate_at(vars(-Count,-'Percent Total'), funs(. %>% round(2) %>% scales::dollar())) %>% 
        mutate_at(vars(-Count,-Cost), funs(. %>% round(2) %>% scales::percent()))
    })
    
    output$farmigotable <- renderDataTable({
    #output$farmigotable <- renderTable({
      dt <- milc %>% 
        filter(Item == input$mitem) %>%
        select(Item) %>% 
        distinct() %>% 
        full_join(sr %>% #obtain full week list
                    rename(Week = 'Week #') %>% 
                    filter(Date <= fp_max & Group_Id != "skip") %>% 
                    select(Week) %>% 
                    distinct()
                  , by = character()
        ) %>% 
        left_join(is %>%  
                    filter(Sold != '') %>% 
                    filter(Item == input$mitem) %>%
                    select(Week, Item, Sold) %>% 
                    distinct()
                  , by = c('Week','Item')
        ) %>% 
        arrange(desc(Week)) %>% 
        mutate(Item = '') %>% 
        rename('__________' = Item) %>% 
        mutate(Week = as.character(Week),
               Sold = as.character(Sold)) 
      dt_ft = dt %>% 
        pivot_wider(names_from = Week, values_from = Sold) %>% 
        replace(is.na(.), '')
      
      dat <- datatable(dt_ft
                       , rownames = FALSE                      #remove row numbers
                       , class = 'cell-border stripe'          #add lines between rows/columns
                       , options = list(dom = 't')) %>%    
        formatStyle(
          names(dt %>% 
                  mutate(Sold = ifelse(Sold != '', 1, 0)) %>% 
                  pivot_wider(names_from = Week, values_from = Sold) %>% 
                  replace(is.na(.), 0)
          )
          , backgroundColor = styleInterval(c(1), c('azure2', 'seagreen'))   #updating color background of each cell
          , color = styleInterval(c(1), c('black', 'white'))                 #updating text color of each cell
        )
      return(dat)
    }) 
    
    output$grouptable <- renderDataTable({
    #output$grouptable <- renderTable({
      dt <- milc %>% 
        filter(Item == input$mitem) %>%
        select(Item) %>% 
        distinct() %>% 
        full_join(sr %>% #obtain full week list
                    rename(Week = 'Week #') %>% 
                    filter(Date <= fp_max & Group_Id != "skip") %>% 
                    select(Week) %>% 
                    distinct()
                  , by = character()
        ) %>% 
        inner_join(wsl %>% #obtain full group list
                     filter(!is.na(Item)) %>% 
                     select(Week, Group_Id) %>% 
                     distinct()
                   , by = c('Week')
        ) %>% 
        left_join(wsl %>%  
                    filter(!is.na(Item) & !is.na(Amt_Share) & !is.na(Prcnt_Amnt)) %>% 
                    filter(Item == input$mitem) %>%
                    left_join(sn %>% 
                                group_by(Week, Group_Id) %>% 
                                summarise(Members = sum(Members)) %>% 
                                ungroup()
                              , by = c('Week','Group_Id')
                    ) %>% 
                    mutate(Member_actual = ceiling(Prcnt_Amnt*Members),
                           member_val = ifelse(Member_actual != Members, 1, 0)
                    ) %>% 
                    group_by(Week, Item, Group_Id) %>% 
                    summarise(Member_actual = sum(Member_actual),
                              member_val = sum(member_val)) %>% 
                    ungroup() 
                  , by = c('Week','Item','Group_Id')
        ) 
      dt_gt <- dt %>% 
        mutate(member_val = ifelse(member_val > 0, paste0(Member_actual, '_(%)'), Member_actual)) %>% 
        select(Week, Item, Group_Id, member_val) %>% 
        arrange(desc(Week)) %>% 
        select(-Item) %>% 
        mutate(Week = as.character(Week),
               member_val = as.character(member_val)) %>% 
        pivot_wider(names_from = Week, values_from = member_val) %>% 
        arrange(Group_Id) %>% 
        replace(is.na(.), '')
      
      dat <- datatable(dt_gt
                       , rownames = FALSE                      #remove row numbers
                       , class = 'cell-border stripe'          #add lines between rows/columns
                       , options = list(dom = 't')) %>%    
        formatStyle(
          names(dt %>% 
                  select(Week, Item, Group_Id, member_val = Member_actual) %>% 
                  arrange(desc(Week)) %>% 
                  select(-Item) %>% 
                  mutate(Week = as.character(Week),
                         member_val = as.integer(member_val)) %>% 
                  pivot_wider(names_from = Week, values_from = member_val) %>% 
                  arrange(Group_Id) %>% 
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
