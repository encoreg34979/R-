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
# Define UI for application that draws a histogram
ui <- fluidPage(
  column(6, plotOutput("plot")),
  column(6,tableOutput("table"))
  

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    user <- read.csv("user.csv",stringsAsFactors = FALSE)
    result <- user %>%
    mutate(Month = as.Date(user$CREATETIME, "%m/%d/%Y %H:%M:%S")) %>%
    mutate(Month = substring(Month,1,7)) %>%  #取出時間的前7個字
    group_by(Month) %>%
    summarise(MembersCount = n())
    
    
    display <- ggplot(result,aes(x = Month,y = MembersCount)) + geom_bar(stat = "identity",fill = "skyblue")
#-----
    order <- read.csv("orders.csv",stringsAsFactors = FALSE,fileEncoding = "big5")
    buyerList <- order %>% 
      distinct(BUYERID)

    buyerList <- as.integer(buyerList$BUYERID) #ID是整數 原本的是datafram
    
    buyresult <- user %>%
      mutate(Month = as.Date(user$CREATETIME,"%m/%d/%Y %H:%M:%S")) %>%
      mutate(Month = substring(Month,1,7)) %>%   #取出時間的前7個字
      mutate(HasBought = user$ID %in% buyerList)  %>% #取出有消費過的人
      group_by(Month,HasBought) %>%
      summarise(MembersCount = n())
    display <- ggplot(buyresult,aes(x = Month,y = MembersCount,fill = HasBought)) + geom_bar(stat = "identity")
  output$plot <- renderPlot(display)
  output$table <- renderTable(buyresult)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

