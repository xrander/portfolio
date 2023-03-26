# Setting working directory
setwd('C:/Users/aduol/Documents/Personal/Online_retail_store/Online Retail Store')

# Loading libraries
library(tidyverse)
library(readxl)
library(rnaturalearth)
library(gridExtra)
library(plotly)

# online_store <- read_excel('Online Retail.xlsx')

str(online_store)

# Getting the column names
colnames(online_store)

# Getting the revenue generated
# online_store <- online_store %>%
  #mutate(Revenue = UnitPrice * Quantity)

#write.csv(online_store,
 #         'C:/Users/aduol/Documents/Personal/Online_retail_store/Online Retail Store/online_store.csv',
  #        row.names = F)


online_store <- read.csv('online_store.csv')

summary(online_store)

# InvoiceDate is seen as a character but should be a date data type.

online_store <- online_store %>%
  mutate(InvoiceDate = as.POSIXct(InvoiceDate, format = "%Y-%m-%d %H:%M:%S")) # Changing character type to Posixct

online_store$CustomerID <- as.factor(online_store$CustomerID)


## Revenue per region
regional_sales <- online_store %>%
  select(Country, Revenue, CustomerID) %>%
  filter(Country != 'United Kingdom') %>%
  group_by(Country) %>%
  summarize(total_revenue = sum(Revenue),
            average_revenue = mean(Revenue)
            )

# smallest earning and highest earning regions
top_10 <- regional_sales %>% top_n(10, wt = total_revenue)


top_countries <- ggplot(top_10,
                        aes(reorder(Country, total_revenue),
                            total_revenue/10000))+
  geom_bar(aes(fill = Country),
           stat = 'identity')+
  labs(title = 'Top Revenue Generating regions Excluding the UK',
       x = 'Country',
       y = 'Revenue generated in tens of thousands')+
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'))

tc <- ggplotly(top_countries)


# To see the values of the countries with the lowest generated revenue
bottom_10 <- regional_sales %>% top_n(-10, total_revenue)

least_countries <- ggplot(bottom_10,
                        aes(reorder(Country, total_revenue),
                            total_revenue))+
  geom_bar(aes(fill = Country),
           stat = 'identity')+
  labs(title = 'Least Revenue Generating regions',
       x = 'Country',
       y ='Revenue generated') +
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'))

lc <- ggplotly(least_countries)



# Result in the form of a table
sorted <- regional_sales[order(regional_sales$total_revenue, decreasing = T),]
print(sorted)


# Month with highest sale.

# Extracting months and dates from the posixt invoice date
online_store <- online_store %>%
  mutate(month_num = as.integer(format(InvoiceDate, format ='%m')),
         year = factor(format(InvoiceDate, format = '%Y'),
                       levels = c(2010, 2011)),
         month = factor(month.abb[month_num],
                        levels = c('Jan', 'Feb', 'Mar',
                                   'Apr', 'May', 'Jun',
                                   'Jul', 'Aug', 'Sep',
                                   'Oct', 'Nov', 'Dec'))
         )

monthly_revenue <- online_store %>%
  select(month, year, Revenue) %>%
  group_by(year, month) %>%
  summarize(total_revenue = sum(Revenue),
            average_revenue = mean(Revenue),) %>%
  mutate(percent_change = (total_revenue - lag(total_revenue))/lag(total_revenue) * 100,
         month_num = as.integer(month))

#monthly trend
mnth_chn <- ggplot(monthly_revenue, aes(month, percent_change))+
  geom_bar(aes(fill = month),
           na.rm = T,
           stat = 'identity')

ggplotly(mnth_chn)


# Month with the highest revenue
mnth_in <- ggplot(monthly_revenue, aes(month, total_revenue)) +
  geom_bar(aes(fill = year),stat = 'identity') +
  scale_fill_manual(values = c('lightgreen', 'darkgreen'))


## Customers
cp <- online_store %>%
  select(CustomerID, Revenue) %>%
  filter(!is.na(CustomerID)) %>%
  group_by(CustomerID) %>%
  summarize(number_of_purchase = length(CustomerID),
            total_purchase = sum(Revenue)) # This returns all the revenue generated from each customers and the number of times they made a purchase
  
top_10_cp <- cp %>% top_n(10, wt = total_purchase)



# To get the top 10 customers that spent the most
tp_10_customer<- top_10_cp %>%
  ggplot(aes(reorder(CustomerID, total_purchase/10000), y = total_purchase/10000, fill = CustomerID)) +
  geom_bar(stat =  'identity')+
  labs(title = 'Top spending customers',
       x = 'Customer ID',
       y = 'Amount spent in ten thousands')+
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'))

ggplotly(tp_10_customer)

# Proportion of spenders to other customers
length(top_10_cp$CustomerID)/length(cp$CustomerID)
# 
num_of_p <- cp %>% top_n(10) %>%
  ggplot(aes(CustomerID, number_of_purchase, fill = CustomerID))+
  geom_bar(stat = 'identity',
           show.legend = F)+
  labs(title='Number of purchase made')+
  xlab('Customer ID')+
  ylab('Number of purchase')
ggsave('num_of_p.jpg')

gg_np <- ggplotly(num_of_p)

grid.arrange(top_10_cp, num_of_p, ncol = 2)

# Customers contribution to total revenue
top_10 <- cp %>% top_n(10, wt = total_purchase)

# Top customers ID
top_10$CustomerID

# Amount generated from customers with more than one order
online_store %>% select(CustomerID, Revenue) %>%
  filter(length(CustomerID)>1) %>%
  summarize(revenue = sum(Revenue))

# Proportion of top customers in the total purchase
sum(top_10$total_purchase)/sum(online_store$Revenue) * 100

# 14 % of the total purchase is by the top customers

