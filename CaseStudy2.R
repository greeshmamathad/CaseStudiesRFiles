#Case Study 2

library(tidyr)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(readxl)
library(highcharter)
library(lubridate)
library(scales)
library(RColorBrewer)
library(wesanderson)
library(plotly)
library(shiny)
library(readxl)
library(readr)
library(scales)
library(stringr)
library(boot)
library(reshape2)


data <- read_csv(file = 'casestudy.csv')

summary(data)

glimpse(data)


summary(is.na(data))

# Total revenue for the current year
data %>%
  group_by(year) %>%
  summarise(Revenue =sum(net_revenue))

#New Customer Revenue e.g. new customers not present in previous year only


cust_2017 <- data %>% 
  filter( year == 2017) 

cust_2016 <- data %>% 
  filter( year == 2016)

cust_2015 <- data %>% 
  filter( year == 2015)


new_cust_2017 <- anti_join(cust_2017, cust_2016, by = c("customer_email" = "customer_email"))
new_cust_2016 <- anti_join(cust_2016, cust_2015, by = c("customer_email" = "customer_email"))

new_cust_2017 %>%
  summarise(Revenue_new_cust_2017 = sum(net_revenue))

new_cust_2016 %>%
  summarise(Revenue_new_cust_2016 = sum(net_revenue))

#Existing Customer Growth. 
#To calculate this, use the Revenue of existing customers for current year –(minus) Revenue of existing customers from the previous year

result = data %>% 
  group_by(customer_email) 

head(result)

# Existing Customer Revenue Current Year

cust_2017 <- data %>% 
  filter( year == 2017) 


cust_2017 %>%
  summarise(Revenue_existing_cust_2017 = sum(net_revenue))

#Existing Customer Revenue Prior Year

cust_2016 <- data %>% 
  filter( year == 2016) 


cust_2016 %>%
  summarise(Revenue_existing_cust_2016 = sum(net_revenue))


# Total Customers Current Year


cust_2017 <- data %>% 
  filter( year == 2017) 

cust_2017

# Total Customers Previous Year
cust_2016 <- data %>% 
  filter( year == 2016) 

cust_2016

# New Customers
new_cust_2017 <- anti_join(cust_2017, cust_2016, by = c("customer_email" = "customer_email"))

new_cust_2017


# Lost Customers

lost_cust <- anti_join(cust_2015, cust_2017, cust_2016, by = c("customer_email" = "customer_email"))

lost_cust

#Plot1

data %>% 
  group_by(year) %>% 
  ggplot(aes(x=year, y=net_revenue))+
  geom_boxplot()+
  labs(y="net_revenue")


#Plot2

data %>% 
  group_by(year = 2017) %>% 
  ggplot(aes(x=year, y=net_revenue))+
  geom_boxplot()+
  labs(y="net_revenue")


