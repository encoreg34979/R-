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
# Define UI for application that draws a histogram
ui <- fluidPage(
  column(6,tableOutput("table")),
  column(6,textOutput("text"))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   orders <- read.csv("orders.csv",stringsAsFactors = FALSE)
   income_result <- orders %>%
     mutate(Month = as.Date(orders$CREATETIME,"%Y-%m-%d %H:%M:%S")) %>%
     mutate(Month = substring(Month,1,7)) %>%
     separate(NAME, c("Category","Brand"),sep = "\\(") %>%
     group_by(Month,Category) %>%
     summarise(Income = sum(PRICE)) %>%
     spread(Category,Income,fill = 0)
   #------
   category_sum <- colSums(income_result[,-1])
   highest_category_index = which(category_sum==max(category_sum)) #找最大值所在的index
   output$text <- renderText(colnames(income_result[,-1])[highest_category_index]) #index所指向的欄位名稱
   #output$table <- renderTable(income_result)
   write.table(income_result,file = "income_sesult.csv" ,sep = "," ,row.names = FALSE,col.names = TRUE) #輸出結果為CSV檔
}

# Run the application 
shinyApp(ui = ui, server = server)

