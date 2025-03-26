install.packages("readxl")
library(readxl)
ecommerce_data<-read_excel("C:\\Users\\ganes\\Downloads\\ecommerce_sales_data.xlsx")
library(lubridate)
head(ecommerce_data)
library(ggplot2)
# Convert date column to Date type
ecommerce_data$Purchase_Date <- as.Date(ecommerce_data$Purchase_Date)

# Calculate Total Revenue for each transaction
ecommerce_data <- ecommerce_data %>%
  mutate(Total_Revenue = Price * Quantity)

# Handle missing values (if any)
ecommerce_data <- d %>%
  replace_na(list(Discount = 0, Category = "Unknown"))

# View summary of data
summary(ecommerce_data)

# Aggregate revenue by product
top_products <- ecommerce_data %>%
  group_by(Product_Name) %>%
  summarise(Total_Revenue = sum(Total_Revenue), Total_Sales = sum(Quantity)) %>%
  arrange(desc(Total_Revenue))

# Visualize top 10 products
ggplot(head(top_products, 10), aes(x = reorder(Product_Name, Total_Revenue), y = Total_Revenue, fill = Product_Name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Products by Revenue", x = "Product Name", y = "Total Revenue") +
  theme_minimal()

# Aggregate revenue by date
sales_trends <- ecommerce_data %>%
  group_by(Purchase_Date) %>%
  summarise(Daily_Revenue = sum(Total_Revenue))

# Plot sales trends
ggplot(sales_trends, aes(x = Purchase_Date, y = Daily_Revenue)) +
  geom_line(color = "blue") +
  labs(title = "Daily Sales Revenue Trend", x = "Date", y = "Total Revenue") +
  theme_minimal()

sales_by_age <- ecommerce_data %>%
  group_by(Age_Group) %>%
  summarise(Total_Revenue = sum(Total_Revenue))

ggplot(sales_by_age, aes(x = Age_Group, y = Total_Revenue, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales Revenue by Age Group", x = "Age Group", y = "Total Revenue") +
  theme_minimal()

sales_by_location <- ecommerce_data %>%
  group_by(Location) %>%
  summarise(Total_Revenue = sum(Total_Revenue))

ggplot(sales_by_location, aes(x = reorder(Location, Total_Revenue), y = Total_Revenue, fill = Location)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sales Revenue by Location", x = "Location", y = "Total Revenue") +
  theme_minimal()

discount_impact <- ecommerce_data %>%
  mutate(Discount_Applied = ifelse(Discount > 0, "Discounted", "Non-Discounted")) %>%
  group_by(Discount_Applied) %>%
  summarise(Total_Revenue = sum(Total_Revenue))

ggplot(discount_impact, aes(x = Discount_Applied, y = Total_Revenue, fill = Discount_Applied)) +
  geom_bar(stat = "identity") +
  labs(title = "Impact of Discounts on Sales", x = "Discount Applied", y = "Total Revenue") +
  theme_minimal()

price_analysis <- ecommerce_data %>%2
  group_by(Price) %>%
  summarise(Total_Sales = sum(Quantity)) %>%
  arrange(Price)

ggplot(price_analysis, aes(x = Price, y = Total_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Price vs Sales Volume", x = "Price", y = "Total Sales") +
  theme_minimal()
