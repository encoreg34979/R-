#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(readr)
library(XML)
library(dplyr)
library(ggplot2)
library(tidyr)
library(arules)

# Define UI for application that draws a histogram
ui <- fluidPage(
   column(12,tableOutput("rout"))
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   orders <- read.csv("orders.csv")
   display <- orders %>%
     separate(NAME, c("Category", "Brand"), sep = "\\(") %>%
     distinct(Category,BUYERID) %>%
     group_by(BUYERID) %>%
     mutate(row = row_number()) %>%
     spread(row,Category) %>% #第一個值為欲拆解的欄位,第二個為拆解後代表的欄位
     ungroup() %>%  #想要去除BUYERID 但是先前用的GROUPBY 
     select(-BUYERID) %>%
     filter_at(2,all_vars(!is.na(.)))
   
   write.table(display,file = "apriori.csv",sep = "," , na = "",row.names = FALSE,col.names = FALSE)
     
   
  
  
  
  
  
  output$rout <- renderTable(display)
}

# Run the application 
shinyApp(ui = ui, server = server)

