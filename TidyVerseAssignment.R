#Get the current working directory
getwd()
#"D:/Personal DataProjects/R_assignment" <--My Working directory

#Importing the necessary libraries

library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(scales)


#Importing the necessary files
#Bikes
bikes <- read_excel('00_data\\01_bike_sales\\01_raw_data\\bikes.xlsx')

#Shops
shops <- read_excel('00_data\\01_bike_sales\\01_raw_data\\bikeshops.xlsx')

#Orders
orders <- read_excel('00_data\\01_bike_sales\\01_raw_data\\orderlines.xlsx')

#Explore the structure of the data
str(bikes)
glimpse(bikes)

str(orders)
glimpse(orders)

str(shops)
glimpse(shops)

#Left join orders and bikes and check the table structure
str(left_join(orders,bikes,by=c("product.id"="bike.id")))

#Join all three tables
joined_tbl <- left_join(orders,bikes,by=c("product.id"="bike.id")) %>% 
  left_join(shops,by = c("customer.id"="bikeshop.id"))

#Check the new table
str(joined_tbl)

#Inspect the category column
joined_tbl %>% select(category) %>% 
  filter(str_detect(category,"^Mountain")) %>% unique()

#Split the category column into 3 columns.
joined_tbl2 <- joined_tbl %>% separate(category,into = c("Category-1"
                                                   ,"Category-2","Category-3"
                                                   ),sep = '-')

#Create new column and multiply quantity and price
joined_tbl3 <- joined_tbl2 %>% mutate(total_price = price * quantity)

#Check if the total price column is correct.
select(joined_tbl3,quantity,price,total_price) %>% filter(quantity != 1)

#Check the structure 
glimpse(joined_tbl3)

#Rearrange the columns and drop unwanted columns and store in a new variable
joined_tbl4 <- joined_tbl3 %>% select(-...1,-url) %>% select(contains("order"),contains("id")
,model,gender,frame.material,weight,model.year,contains("category"),name,
location,lat,lng,price,quantity,total_price)


names(joined_tbl4)

#Rename columns : replace "." and "-" with "_"
joined_tbl4 <- joined_tbl4 %>% set_names(names(.) %>% str_replace_all("(\\.|-)","_"))

names(joined_tbl4)

## BUSINESS INSIGHTS :
#1. Amount of sales by year :

sales_year_tbl <- joined_tbl4 %>% select(order_date,total_price) %>%
  mutate(Year = year(order_date))

#Find the number of years in the data was collected for
unique(sales_year_tbl$Year) #2015,2016,2017,2018,2019 are the
                            #years in our data

#Group the data by year and sum the prices.
SalesByYear_tbl <- sales_year_tbl %>% group_by(Year) %>%
  summarise(yearly_sales = sum(total_price))

#To find out the year with maximum and minimum sales.
SalesByYear_tbl[which.max(SalesByYear_tbl$yearly_sales),] #Max sales
SalesByYear_tbl[which.min(SalesByYear_tbl$yearly_sales),] #Min sales

#Formatting the yearly sales column into dollars
YearlySales_tbl <-SalesByYear_tbl %>% mutate(yearly_sales_frmtd = scales::
                             dollar(yearly_sales,big.mark=",",
                                    decimal.mark = ".",suffix = "$",
                                    prefix=""))
#scales::dollar was used since scales library was not loaded initially.

#Visualize the findings

library(ggplot2)

YearlySales_tbl %>% ggplot(aes(x=Year,y=yearly_sales)) + 
  geom_col(fill = '#2DC6D6') + geom_label(aes(label = yearly_sales_frmtd)) +
  geom_smooth(method = 'lm',se=FALSE) + scale_y_continuous(labels = dollar) +
  labs(title = "Revenue by year",y="Yearly sales")

########################################################################
#2. Sales by category  

#Some values are mis represented as E. Correct them.
joined_tbl4$Category_1 <- sub("E","E-Bikes",joined_tbl4$Category_1)
joined_tbl4$Category_3 <- sub("E","E-Bikes",joined_tbl4$Category_3)

SalesByCat <- joined_tbl4 %>% select(order_date,Category_1,total_price) %>%
  mutate(Year = year(order_date)) %>% select(-order_date) %>% 
  group_by(Category_1,Year) %>% summarise(sales = sum(total_price)) %>%
  arrange(Year)

#Format the sales column
SalesByCat <- SalesByCat %>% mutate(sales_frmtd = dollar(sales,prefix = "",suffix="$",
                                           big.mark = ",",decimal.mark = "."
                                           ))


#To check which category had the maximum sales on a particular year.  
view(SalesByCat %>% group_by(Year) %>% mutate(max_sales = max(sales)) %>%
  ungroup())

#Visualise the findings

SalesByCat %>% ggplot(aes(x=Year,y=sales,fill = Category_1)) + 
                        geom_col() + facet_wrap(~ Category_1) +
                        geom_smooth(method = 'lm',se=FALSE)
                          scale_y_continuous(labels = dollar) +
                        labs(title = "Revenue by Year and Category",
                             fill = "Category of bike")
                          
install.packages("writexl")
library(writexl)

SalesByCat %>% write_xlsx('D:\\Personal DataProjects\\R_assignment\\00_data\\01_bike_sales\\01_raw_data\\SalesCategory.xlsx')




