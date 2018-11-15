
library(arules)
library(readr)
library(XML)
library(dplyr)
library(ggplot2)
library(tidyr)

windowsFonts(BL = windowsFont("微軟正黑體"))
par(family = "BL")    #設定字體



result <- read.transactions("apriori.csv" , sep = "," , rm.duplicates = TRUE,quote = "") #quote = "" delete NA
summary(result)
itemFrequencyPlot(result, topN = 72, names = FALSE, support = 0.01) #總共72項商品,不顯示名字,品項支持度不到1%就不畫

rules = apriori(data = result, parameter = list(support = 0.05, confidence = 0.1, minlen = 2))

inspect(sort(rules, by = "lift")[1:10])

