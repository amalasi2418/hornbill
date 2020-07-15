# Exploratory data analysis (EDA) of online retail info of UK based company

# loading concerned packages
#install.packages("treemapify")
library(treemapify)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)

#install.packages("Displayr")
#install.packages("flipPlots")

#require(devtools)
#install_github("Displayr/flipPlots")
#library(flipPlots)

# reading data file
df <- read.csv("online_retail_II.csv")

# understanding the data
# size of the file
dim(df)
summary(df)
str(df)

# lets convert customer.ID to character to avoid doing any math on it
df$Customer.ID <- as.character(df$Customer.ID)


# looking at the sample data
head(df)

# lets check for NA's
df %>% is.na %>% colSums

# Only customer id seems to have missing values and is of not relevance 
# currently. if required we will deal with it later.

### DATA CLEANING ###

# Lets make separate df for free items 

df_free <- df %>% filter(Price == 0)
head(df_free)

# Lets make separate df for items for which invoice was canceled
df_canceled <- df %>% filter(str_detect(Invoice,"C."))
head(df_canceled)

# Lets remove the free items and canceled invoice from the data set
df <- df %>% filter(Price!=0)
df <- df %>% filter(!grepl("C.", Invoice))

# lets check the dimension so the rows were deleted or not
dim(df)
summary(df)

# Removing negative prices as there are returned items with incorrect Invoice
# information or not updated

df <- df %>% filter(Price > 0)
summary(df)

### DATA CLEANING FINISHED ###


### DATA PRE-PROCESSING ###

# lets find total sales
df$Total_Sales <- df$Quantity * df$Price

# lets split the InvoiceDate into year, month day, hour, week etc.. 

df$InvoiceDate <- ymd_hms(df$InvoiceDate)
df$date <- format(df$InvoiceDate, "%Y/%m/%d")
df$month <- month(df$InvoiceDate)
df$day_of_week <- wday(df$InvoiceDate)
df$hour <- hour(df$InvoiceDate)

### DATA VISUALIZATION ###

# lets understand the distribution of orders placed geographically
# calculating the order frequency country wise

#freq_country <- table(df$Country)

# converting to df for ggplot
#fcountry <- as.data.frame(freq_country)

fcountry <- df %>% group_by(Country) %>% summarise(Order_frequency = sum(Quantity))
Total_Sales1 <- df %>% group_by(Country) %>% summarise(Total_Sales = sum(Total_Sales))
fcountry <- cbind(fcountry, Total_Sales1[2])
# renaming columns
# names(fcountry)

#names(fcountry)[names(fcountry) == 'Var1'] <- 'Country'
#names(fcountry)[names(fcountry) == 'Freq'] <- 'Order_frequency'

# arranging factor levels in increasing order of 'order frequency'
fcountry$Country <- factor(fcountry$Country, 
                           levels = fcountry$Country[
                             order(fcountry$Total_Sales)])

# visual inspection of order frequency as a function of country
fcountry %>%  ggplot(aes(Country,Total_Sales/1e6)) + 
  geom_bar(stat = "identity", color = 'purple', alpha =0.3) + 
  #scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Total sales (£ in millions)") +
  #scale_y_discrete(limits=c(0:10)) +
  theme_bw() +
  coord_flip()

# visual inspection of order frequency as a function of country (w/o UK)
fcountry1 <- fcountry %>% filter(!grepl("United Kingdom", Country))

fcountry1 %>%  ggplot(aes(Country,Total_Sales/1e6)) + 
  geom_bar(stat = "identity", color = 'purple', alpha =0.3) + 
  #scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Total sales (£ in millions)") +
  #scale_y_discrete(limits=c(0:10)) +
  theme_bw() +
  coord_flip()

# calculating percentage sales share as we can see UK clearly dominates
fcountry$market_share <- fcountry$Total_Sales*100/sum(fcountry$Total_Sales)
fcountry1$market_share <- fcountry1$Total_Sales*100/sum(fcountry1$Total_Sales)

# plotting the market share using treemap in ggplot
ggplot(fcountry, aes(area = market_share, 
                     fill = market_share, label = Country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)

# plotting the market share using treemap in ggplot w/o UK
ggplot(fcountry1, aes(area = market_share, 
                      fill = market_share, label = Country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)


# history of orders placed
# the order history clearly shows maximum order placed during Nov-Dec
# holiday period. With orders being placed between 11 am till 3 pm.Not
# much variation in orders placed during the week. Although maximum orders
# placed on Friday followed by Wednesday.
df %>% ggplot(aes(month, Total_Sales/1e6)) +
  geom_bar(stat = 'identity', color = 'red', alpha = 0.3) +
  scale_x_discrete(limits=c(1:12)) +
  ylab('Total sales (£ in millions)') +
  xlab('Months')
  #scale_x_continuous(limits=c(1:12))

df %>% ggplot(aes(hour, Total_Sales/1e6)) +
  geom_bar(stat = 'identity', color = 'red', alpha = 0.3) +
  ylab('Total sales (£ in millions)') +
  xlab('Hours')
  #scale_x_discrete(limits=c(6:21))+
  #theme_classic()

df %>% ggplot(aes(day_of_week, Total_Sales/1e6)) +
  geom_bar(stat = 'identity', color = 'red', alpha = 0.3) +
  ylab('Total sales (£ in millions)') +
  xlab('Days of the week')

# lets look at the total monthly sales figure

df %>% group_by(month) %>% summarise(Sales = sum(Total_Sales)/1e6) %>%
  ggplot(aes(month, Sales)) +
  geom_point(color = "red", size = 3, alpha = 0.3) +
  geom_line(color = "black", size = 1.25, alpha = 0.3, linetype = 2) +
  scale_x_discrete(limits=month.name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Months of the year") + 
  ylab("Total monthly sales (£ in million)")

# extracting monthly sales figures of countries
top_sales <- df %>% group_by(month, Country) %>% 
  summarise(Sales = sum(Total_Sales)/1e6) %>% arrange(desc(Sales))


Country_sales <- df %>% group_by(month, Country) %>% 
  summarise(Sales = sum(Total_Sales)/1e6) %>% group_by(Country) %>% nest() 

Country_sales <- Country_sales %>% arrange(Country)

Country_sales <- Country_sales %>% unnest(data)

top_country_sales <- Country_sales %>% 
  filter(Country %in% c("EIRE", "Germany", "France", "Netherlands"))

top_country_sales %>% ggplot(aes(month, Sales, color = Country)) +
  geom_point(size = 3, alpha = 0.3) +
  geom_line(size = 1.25, alpha = 0.3, linetype = 2) +
  scale_x_discrete(limits=month.name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Months of the year") + 
  ylab("Total monthly sales (£ in million)")


#top_sales %>% ggplot(aes(month, Sales, color = Country)) +
#  geom_point(size = 3, alpha = 0.3) +
#  geom_line(size = 1.25, alpha = 0.3, linetype = 2) +
#  scale_x_discrete(limits=month.name) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  xlab("Months of the year") + 
#  ylab("Total monthly sales (£ in million)")
  #scale_y_discrete(limits=c(0:.5))

# daily sales figures
df %>% group_by(day_of_week) %>% summarise(Sales = sum(Total_Sales)/1e6) %>%
  ggplot(aes(day_of_week, Sales)) +
  geom_point(color = "red", size = 3, alpha = 0.3) +
  geom_line(color = "black", size = 1.25, alpha = 0.3, linetype = 2) +
  scale_x_discrete(limits=c("Mon","Tue", "Wed", "Thur","Fri","Sat","Sun")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Days of the week") + 
  ylab("Total sales per day (£ in million)")

# hourly sales figures
df %>% group_by(hour) %>% summarise(Sales = sum(Total_Sales)/1e6) %>%
  ggplot(aes(hour, Sales)) +
  geom_point(color = "red", size = 3, alpha = 0.3) +
  geom_line(color = "black", size = 1.25, alpha = 0.3, linetype = 2) +
  #scale_x_discrete(limits=c(5:20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Time in hours") + 
  ylab("Total hourly sales (£ in million)")


# lets now analyze the popular items being sold
# the stock code directly related to the product description
# we can notice some stock codes end with characters defining in the color
# of the product. This means a product when analyze products we should
# keep it independent of color. 



Popular_items <- as.data.frame(df %>% group_by(StockCode, Description) %>% 
  summarise(Quantity = sum(Quantity))) 

#Popular_items <- intersect(df[,2:3], Popular_items)


Total_sales2 <-  as.data.frame(df %>% group_by(StockCode, Description) %>%
  summarise(Total_sales = sum(Total_Sales)))

#Popular_items <- Popular_items[,-3] %>% arrange(desc(Quantity))
Popular_items <- cbind(Popular_items, Total_Sales=Total_sales2[,3])

Popular_items <- Popular_items %>% arrange(desc(Total_Sales))

head(Popular_items, 10)
dim(Popular_items)

# looking at the top 10 items, we can see information on StockCode for 
# some items is entered incorrectly. We cannot delete the item as it 
# is 2nd most popular. 

# We can further do analysis on what type of color is more popular
# or fill in the blank cases if any by using StockCode and Description 
# information matching it with unit price of items.

# last part of EDA is the number of transactions
Weekly_transactions <- df %>% group_by(date, day_of_week) %>% 
  summarise(revenue = sum(Total_Sales), transactions = n_distinct(Invoice)) %>% 
  mutate(Avg_order_cost = round(revenue/transactions,2)) %>% ungroup()

head(Weekly_transactions)

Weekly_transactions %>% ggplot(aes(day_of_week, Avg_order_cost)) +
  geom_bar(stat = 'identity')

Weekly_transactions %>% ggplot(aes(day_of_week, Avg_order_cost)) +
  geom_boxplot()

Weekly_transactions %>% ggplot(aes(x = date, y = revenue)) + 
  geom_line()

Weekly_transactions %>% 
  group_by(day_of_week) %>% 
  summarise(Avg_order_cost = sum(Avg_order_cost)) %>% 
  ggplot(aes(day_of_week, Avg_order_cost)) + 
  geom_point(color = "red", size = 3, alpha = 0.3) +
  geom_line(color = "black", size = 1.25, alpha = 0.3, linetype = 2) 


Weekly_transactions %>% 
  group_by(day_of_week) %>% 
  summarise(transactions = sum(transactions)) %>% 
  ggplot(aes(day_of_week, transactions)) + 
  geom_point(color = "red", size = 3, alpha = 0.3) +
  geom_line(color = "black", size = 1.25, alpha = 0.3, linetype = 2)


# number of transactions done bu customers per country

Customer_data <- df %>% group_by(Country, date) %>% 
  summarise(customers = n_distinct(Customer.ID), revenue = sum(Total_Sales),
            quantity = sum(Quantity),transactions = n_distinct(Invoice)) %>% 
  mutate(Avg_trans_cost =  revenue/transactions) %>%
  arrange(desc(revenue)) 


Customer_data %>% ggplot(aes(customers, transactions)) + geom_point()

# customer analysis
Customer <- df %>% group_by(Country, date) %>% 
  summarise(customers = n_distinct(Customer.ID), revenue = sum(Total_Sales),
            quantity = sum(Quantity),transactions = n_distinct(Invoice)) %>% 
  mutate(Avg_trans_cost =  revenue/transactions)

Customer <- Customer %>% mutate(trans_per_cust = transactions/customers)

#Customer$Country <- factor(Customer$Country, 
 #                          levels = Customer$Country[order(Customer$customers)])

Customer %>% ggplot(aes(Country, customers)) +
  geom_boxplot(stat = "identity", color = 'purple', alpha =0.3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Number of unique customers") +
  theme_bw() +
  coord_flip()

Customer$Country <- factor(Customer$Country, 
                           levels = Customer$Country[order(Customer$transactions)])

Customer %>% ggplot(aes(Country, transactions)) +
  geom_bar(stat = "identity", color = 'purple', alpha =0.3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Number of transactions") +
  theme_bw() +
  coord_flip()

Customer$Country <- factor(Customer$Country, 
                           levels = Customer$Country[order(Customer$trans_per_cust)])

Customer %>% ggplot(aes(Country, trans_per_cust)) +
  geom_bar(stat = "identity", color = 'purple', alpha =0.3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Number of transactions per customers") +
  theme_bw() +
  coord_flip()

Customer %>% ggplot(aes(customers, revenue)) +geom_point()

Customer_new <- Customer %>% filter(!grepl("United Kingdom", Country))

Customer_new %>% ggplot(aes(customers, revenue)) +geom_point()

Customer_new %>% ggplot(aes(customers, trans_per_cust)) +geom_point()

Customer_new %>% ggplot(aes(customers, transactions)) +geom_point()
  
#%>% ggplot(aes(Country, customers)) +
#  geom_bar(stat = 'identity')

# Next we will work on customer segmentation and sales prediction