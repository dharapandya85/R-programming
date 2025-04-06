#print("Hello, R in VS Code!")
# Load necessary library
options(repr.plot.res = 150, repr.plot.width = 7, repr.plot.height = 5)

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(readr)
library(dplyr)
#library(readxl)

# Create the data frame
sales_data <- data.frame(
  Month = factor(c("Feb", "Mar", "Apr", "May", "Jun", "Jul"),
                  levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul")),
  New_Food_Donors = c(15, 20, 30, 40, 50, 60),
  New_Recipients = c(20, 25, 35, 45, 55, 70),
  New_Drivers = c(5, 8, 12, 15, 20, 25),
  Estimated_Deliveries = c(100, 150, 200, 250, 300, 350),
  Potential_Revenue_Min = c(5000, 7500, 10000, 12500, 15000, 17500),
  Potential_Revenue_Max = c(10000, 15000, 20000, 25000, 30000, 35000),
  Min_Profit = c(2500, 3750, 6000, 9000, 12500, 16000),
  Max_Profit = c(5000, 7500, 12000, 18000, 25000, 32000)
)
sales_data <- read_csv("Updated_NY_Food_Drop_Profit_Forecast.csv")

# Convert Month to factor to maintain order
sales_data$Month <- factor(sales_data$Month, levels = unique(sales_data$Month))

# Melt data for ggplot
library(reshape2)
sales_data_melted <- melt(sales_data, id.vars = "Month", variable.name = "Profit_Type", value.name = "Revenue")

# Create the line chart
p<-ggplot(sales_data_melted, aes(x = Month, y = Revenue, group = Profit_Type, color = Profit_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Sales Forecast for Logistics Business",
       x = "Month",
       y = "Potential Revenue ($)") +
  scale_color_manual(values = c("blue", "red"), labels = c("Min Profit ($50 per delivery)", "Max Profit ($100 per delivery)")) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(p)
# Summary of Data
summary(sales_data)

print(p)

# 🚀 1. Growth Trends for Donors, Recipients, and Drivers
p<-ggplot(sales_data, aes(x = Month)) +
  geom_line(aes(y = New_Food_Donors, color = "Food Donors"), size = 1) +
  geom_line(aes(y = New_Recipients, color = "Recipients"), size = 1) +
  geom_line(aes(y = New_Drivers, color = "Drivers"), size = 1) +
  geom_point(aes(y = New_Food_Donors, color = "Food Donors"), size = 2) +
  geom_point(aes(y = New_Recipients, color = "Recipients"), size = 2) +
  geom_point(aes(y = New_Drivers, color = "Drivers"), size = 2) +
  labs(
    title = "Monthly Growth: Donors, Recipients, and Drivers",
    x = "Month",
    y = "Count"
  ) +
  theme(legend.title = element_blank())

print(p)

# colnames(sales_data) <- c(
  theme(legend.title = element_blank())
# colnames(sales_data) <- c(
#   "Month", 
#   "New_Food_Donors", 
#   "New_Recipients", 
#   "New_Drivers", 
#   "Estimated_Deliveries", 
#   "Min_Revenue", 
#   "Max_Revenue"
# )
# sales_data$Min_Revenue <- as.numeric(sales_data$Min_Revenue)
# sales_data$Max_Revenue <- as.numeric(sales_data$Max_Revenue)
# sales_data <- sales_data %>% drop_na(Min_Revenue, Max_Revenue)

# 💰 2. Revenue Potential Analysis
p<-ggplot(sales_data, aes(x = Month)) +
  geom_line(aes(y = `Potential Revenue (Min Profit $50 per delivery)`), color = "Min Revenue", size = 1) +
  geom_line(aes(y = `Potential Revenue (Max Profit $100 per delivery)`), color = "Max Revenue", size = 1) +
  geom_point(aes(y =`Potential Revenue (Min Profit $50 per delivery)`),color="blue", size = 2) +
  geom_point(aes(y =`Potential Revenue (Max Profit $100 per delivery)`), color="red",size = 2)+
  labs(
    title = "Monthly Potential Revenue (Min vs Max)",
    x = "Month",
    y = "Revenue ($)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

  print(p)
# 📊 3. Correlation Analysis
# 📊 3. Correlation Analysis
correlations <- cor(sales_data[, c("New_Food_Donors", "New_Recipients", "New_Drivers", "Estimated_Deliveries", "Potential_Revenue_Min", "Potential_Revenue_Max")])
print("Correlation Matrix:")
print(correlations)

# 🚚 4. Deliveries vs. Revenue
p<-ggplot(sales_data, aes(x =`Estimated Deliveries`)) +
  geom_point(aes(y = `Potential Revenue (Min Profit $50 per delivery)`, color = "Min Revenue"), size = 3) +
  geom_point(aes(y = `Potential Revenue (Max Profit $100 per delivery)`, color = "Max Revenue"), size = 3) +
  geom_smooth(aes(y =`Potential Revenue (Min Profit $50 per delivery)`), method = "lm", color = "blue", se = FALSE) +
  geom_smooth(aes(y =`Potential Revenue (Max Profit $100 per delivery)`), method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Estimated Deliveries vs. Revenue",
    x = "Estimated Deliveries",
    y = "Potential Revenue ($)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

print(p)

# Plot sales over time
# p<-ggplot(sales_data, aes(x = Date, y = Revenue)) +
#   geom_line(color = "blue") +
#   geom_point(color = "red") +
#   labs(title = "Logistics Revenue Over Time",
#        x = "Date",
#        y = "Revenue ($)") +
#   theme_minimal()

# print(p)
# # Decompose the time series
# decomposed_ts <- decompose(sales_ts)
# plot(decomposed_ts)
# # Fit ARIMA model
# fit <- auto.arima(sales_ts)

# # Forecast for the next 6 months
# forecast_result <- forecast(fit, h = 6)

# # Plot the forecast
# p<-plot(forecast_result, 
#      main = "Revenue Forecast for Logistics Business",
#      xlab = "Year",
#      ylab = "Revenue ($)")


# print(p)
# # Calculate forecast accuracy
# accuracy(forecast_result)

# # Scenario: Increase deliveries by 10%
# sales_data$Projected_Revenue <- sales_data$Revenue * 1.10

# p<-ggplot(sales_data, aes(x = Date)) +
#   geom_line(aes(y = Revenue, color = "Actual"), size = 1) +
#   geom_line(aes(y = Projected_Revenue, color = "Projected"), size = 1, linetype = "dashed") +
#   labs(title = "Revenue Projection with Increased Deliveries",
#        x = "Date",
#        y = "Revenue ($)") +
#   theme_minimal() +
#   scale_color_manual(values = c("Actual" = "blue", "Projected" = "green"))
# print(p)

# p<-ggplot(sales_data, aes(x = Estimated_Deliveries)) +
#   geom_point(aes(y = Min_Profit, color = "Min Profit"), size = 3) +
#   geom_point(aes(y = Max_Profit, color = "Max Profit"), size = 3) +
#   geom_smooth(aes(y = Min_Profit), method = "lm", color = "blue", se = FALSE) +
#   geom_smooth(aes(y = Max_Profit), method = "lm", color = "red", se = FALSE) +
#   labs(
#     title = "Estimated Deliveries vs. Profit",
# print(p)

# bts_data <- read_csv("CFSPRELIM2017.CF1700P1-Data.csv")
#     y = "Profit ($)"
#   ) +
#   theme_minimal() +
#   theme(legend.title = element_blank())

# bts_data <- read_csv("CFSPRELIM2017.CF1700P1-Data.csv")
# cfs_data <- read_csv("FAF5.5.1_HiLoForecasts.csv")

# # Merge Data for Analysis
# merged_data <- full_join(bts_data, cfs_data, by = "State")

# # Visualization
# ggplot(merged_data, aes(x = Year, y = FreightVolume, color = TransportationMode)) +
#   geom_line() +
#   labs(title = "U.S. Freight Volume Trends", x = "Year", y = "Volume (tons)")


print(p)