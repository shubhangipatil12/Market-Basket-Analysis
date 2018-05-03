install.packages("tidyverse")
install.packages("arules")
install.packages("arulesViz", dependencies = TRUE)
install.packages("magrittr")
install.packages("dplyr")

library(magrittr)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(dplyr)

# Data exploration and processing 

retail <- read_excel('C:/Users/shubhangipatil/Desktop/R/Online Retail.xlsx')

# Find which values are complete, without missing values

retail <- retail[complete.cases(retail),]

#Mutate used for adding new variables preserving old varaibles

retail <- retail %>% mutate(Description=as.factor(Description))
retail <- retail %>% mutate(Country=as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")

retail$InvoiveNo <- as.numeric(as.character(retail$InvoiceNo))

#Gets the details related to all the fields in dataset
glimpse(retail)

#What time do customers purchase online from the retailer?

#Extracting hour to get the time of purchase

retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)

retail %>%
  ggplot(aes(x=Time))+
  geom_histogram(stat = "count", fill="indianred")

#How many items does each Customer buy?

detach("package:dplyr", unload=TRUE)

retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))


#Get the top 10 bestsellers

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp

tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#Changing the format of the data frame to get all items bought in rows

retail_sorted <- retail[order(retail$CustomerID),]
library(dplyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))


#Removing irrelevant columns like customerid and Date columns

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

#Write it to csv files to check the format

write.csv(itemList,"C:/Users/shubhangipatil/Desktop/R/market_basket.csv", quote = FALSE, row.names = TRUE)

#Looking at transactions and finding what they are

tr <- read.transactions('C:/Users/shubhangipatil/Desktop/R/market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)


#Item frequency plot

#To rectify figure margins too large error we get
graphics.off()
par("mar")
par(mar=c(1,1,1,1))

itemFrequencyPlot(tr, topN=20, type=c('absolute'))
#Result Fig1-Freq Item

#Forming association rules and mining which itemsets are most common

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)


#To check the top 10 rules 

inspect(rules[1:10])

#Plotting these top rules

topRules <- rules[1:10]
plot(topRules)
#Result Fig 2 Scatter Plot of Rules
plot(topRules,method='graph')
#Result Fig 3 Graph of Rules
plot(topRules,method='grouped')
#Result Fig 4 Grouped names of the rules

