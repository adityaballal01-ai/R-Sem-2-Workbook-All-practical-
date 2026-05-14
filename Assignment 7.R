# Loading dataset
data <- read.csv("E:/Sem 2/R programming-Rajesh Sir/Sem workbook/customer_data.csv")

# 1) Pearson correlation between Income and SpendingScore
correlation_value <- cor(data$Income, data$SpendingScore)

print(correlation_value)

# Interpretation
# If value is close to +1 = strong positive relation
# If value is close to -1 = strong negative relation
# If value is near 0 = weak or no relation


# 2) Significance test for correlation
correlation_test <- cor.test(data$Income, data$SpendingScore)

print(correlation_test)

# Conclusion:
# p-value < 0.05 means correlation is statistically significant


# 3) Generate normal distribution using Income
normal_income <- rnorm(
  100,
  mean(data$Income),
  sd(data$Income)
)

print(normal_income)

# Compare summary of actual income and generated income
summary(data$Income)
summary(normal_income)


# 4) One sample t-test
# H0 : Average income = 50000

income_test <- t.test(data$Income, mu = 50000)

print(income_test)

# Interpretation:
# p-value < 0.05 means average income is significantly different from 50000


# 5) Binomial test for purchase success rate

purchase_success <- sum(data$Purchase == "Yes")

binomial_result <- binom.test(
  purchase_success,
  nrow(data),
  p = 0.5
)

print(binomial_result)

# Interpretation:
# p-value < 0.05 means purchase success rate differs from 50%