set.seed(100)

sales <- data.frame(
  product = sample(c("Laptop","Mobile","Tablet"), 50, replace = TRUE),
  city = sample(c("Pune","Mumbai","Delhi"), 50, replace = TRUE),
  Quantity = sample(1:5, 50, replace = TRUE),
  Price = sample(seq(10000,50000,5000), 50, replace = TRUE)
)

sales$Revenue <- sales$Quantity * sales$Price


# Q1 Show records where Revenue > 100000
high_revenue <- sales[sales$Revenue > 100000, ]
high_revenue


# Q2 Sort data by Revenue in descending order
sorted_sales <- sales[order(-sales$Revenue), ]
sorted_sales


# Q3 Find Total revenue by City
total_revenue_city <- aggregate(Revenue ~ city, data = sales, sum)
total_revenue_city


# Q4 Find average price by product
avg_price_product <- aggregate(Price ~ product, data = sales, mean)
avg_price_product


# Q5 Identify the product with highest total revenue
product_revenue <- aggregate(Revenue ~ product, data = sales, sum)
max_row <- which.max(product_revenue$Revenue)
highest_revenue_product <- product_revenue[max_row, ]
print(highest_revenue_product)




data <- data.frame(
  Gender=c("Male","Female","Male","Female","Male","Female"),
  Purchase=c("Yes","No","Yes","Yes","No","Yes"),
  Amount=c(5000,7000,8000,6000,4000,9000)
)


data <- data.frame(
  Gender=c("Male","Female","Male","Female","Male","Female"),
  Purchase=c("Yes","No","Yes","Yes","No","Yes"),
  Amount=c(5000,7000,8000,6000,4000,9000)
)

# Q1 Calculate mean and median of Amount
mean_amount <- mean(data$Amount)
median_amount <- median(data$Amount)
mean_amount
median_amount


# Q2 Create a frequency table for Gender
gender_freq <- table(data$Gender)
gender_freq


# Q3 Create a cross-tabulation of Gender and Purchase
cross_tab <- table(data$Gender, data$Purchase)
cross_tab


# Q4 Find total purchase amount by Gender
total_amount_gender <- aggregate(Amount ~ Gender, data=data, sum)
total_amount_gender


# Q5 Count number of "Yes" Purchase
yes_count <- sum(data$Purchase == "Yes")
yes_count