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
  orders <- read.csv("orders.csv")
  user <- read.csv("user.csv")
  display <- orders %>%
    mutate(Month = as.Date(orders$CREATETIME, "%Y-%m-%d %H:%M:%S")) %>%
    mutate(Month = substring(Month,1,7)) %>%
    separate(NAME , c("Category" , "Brand"), sep = "\\(") %>%
    filter (Month == "2017-05",Category == "生活家電", PRICE >= 450) %>%
    merge(user, by.x = "BUYERID" ,by.y = "ID")  %>%   # order's BUYID to user's ID
    select(ID,PRICE,PAYMENTTYPE,ACCOUNT,MOBILE)
  
  output$rout <- renderTable(display)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

