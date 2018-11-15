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
   column(12,tableOutput("rout"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  income_result <- read.csv("income_sesult.csv",stringsAsFactors = FALSE)
  
  income_result <- income_result %>%
    gather(Category,Income,2:73,na.rm=FALSE) %>%#第一個參數放的是組合之後的欄位名稱，第二個參數放”組合值”的欄位名稱，第三個參數是哪些欄位要被組，na.rm可以決定要不要去除空欄位
    group_by(Category) %>%
    summarise(Category_Income = sum(Income)) %>%
    arrange(desc(Category_Income)) #desc() 降冪排序
  #----------------------
  
  #display <- ggplot(income_result, aes(x="", y=Category_Income, fill=Category)) +
  #  geom_bar(width = 1, stat = "identity") +
  #  coord_polar("y", start=0) +
  #  theme(text=element_text(family="Arial", size=12))  #畫圓餅圖
  #---------------------
  
  
  
  output$rout <- renderTable(income_result)
}

# Run the application 
shinyApp(ui = ui, server = server)

