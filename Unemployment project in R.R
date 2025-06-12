# Unemployment project
# 1. How has the unemployment rate changed over time (by date)?
# 2. Are there any seasonal patterns in unemployment? (Analyze based on the frequency column.)
# 3. Which years or months had the highest and lowest unemployment rates?
# 4. Which Indian states have the highest and lowest unemployment rates?
# 5. Can you identify any regional disparities in employment and unemployment?
# 6. Is there a correlation between the estimated labor participation rate and the unemployment rate across different states?
# 7. What is the estimated employed percentage in each state?
# 8. Are there any states where the employed percentage significantly differs from the national average?
# 9. How does the employed percentage correlate with the unemployment rate?
# 10.What is the estimated labor participation rate in each state?
# 11.Is there any relationship between labor participation and unemployment?

# Solution

# Activate packages for data analysis and visualization
library("dplyr")
library("ggplot2")

# Import the Unemployment dataset.
Unemployment_Data <- readxl::read_xlsx("Unemployment dataset.xlsx")
View(Unemployment_Data)

# Question 1
Changes_Overtime <- Unemployment_Data %>% group_by(Date) %>% summarise(Total_Rate = sum(`Estimated Unemployment Rate (%)`))%>% 
  arrange(Date)
View(Changes_Overtime)

# Per year 
# 2019
Changes_Overtime_2019 <- select(Unemployment_Data, Date, `Estimated Unemployment Rate (%)`)
Changes_Overtime_2019 <- Changes_Overtime_2019  %>% filter(substr(Date,7,10) == 2019) %>%
  group_by(Date) %>% summarise(Average = mean(`Estimated Unemployment Rate (%)`))

View(Changes_Overtime_2019)
# Note that the output from the query is currently arranged by day, which may cause the results to 
# appear inconsistent. To change the aggregation from daily to monthly, we can use the following 
# approach:
# First
Changes_Overtime_2019$month <- as.integer(substr(Changes_Overtime_2019$Date,4,5))
Changes_Overtime_2019$year <- as.integer(substr(Changes_Overtime_2019$Date,7,10))

# Second
Changes_Overtime_2019 <- Changes_Overtime_2019[order(Changes_Overtime_2019$month),]
View(Changes_Overtime_2019)

# To visualize our findings we use line chart
ggplot(data = Changes_Overtime_2019, mapping = aes(x = month, y = Average)) + geom_line() +
  labs(title = "Unemployment rate in 2019",
       x = "Months",
       y = "Average unemployment rate (%)") +
  theme_minimal()

ggplot(data = Changes_Overtime_2019, mapping = aes(x = month, y = Average)) +
  geom_line(color = "red", size = 1.2) +  # Add color and adjust line size
  labs(title = "Unemployment Rate in 2019",
       x = "Month",
       y = "Average Unemployment Rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and style the title
    axis.title.x = element_text(size = 12, face = "bold"),             # Style x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),             # Style y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1),                  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),                              # Style y-axis labels
    panel.grid.major = element_line(color = "grey80"),                  # Style major grid lines
    panel.grid.minor = element_line(color = "grey90")                   # Style minor grid lines
  )

# For 2020
Changes_Overtime_2020 <- select(Unemployment_Data, Date, `Estimated Unemployment Rate (%)`)
Changes_Overtime_2020 <- Changes_Overtime_2020 %>% filter(substr(Date,7,10) == 2020) %>%
  group_by(Date) %>% summarise(Average = mean(`Estimated Unemployment Rate (%)`))

# Rearrange the date
Changes_Overtime_2020$month <- as.integer(substr(Changes_Overtime_2020$Date,4,5))
Changes_Overtime_2020$year <- as.integer(substr(Changes_Overtime_2020$Date,7,10))

Changes_Overtime_2020 <- Changes_Overtime_2020[order(Changes_Overtime_2020$month),]
View(Changes_Overtime_2020)

# Visualization
ggplot(data = Changes_Overtime_2020, mapping = aes(x = month, y = Changes_Overtime_2020$Average)) +
  geom_line(color = "navy", size = 1.2) + labs(title = "Unemployment rate in 2020",
                                               x = "Months",
                                               y = "Average unemployment rate (%)") + theme_minimal() 
# From my findings, the unemployment rate was at it's low in the month of January and was ai tis high in
# the month of April

# Question 2 
Seasonal_pattern <- Unemployment_Data %>% group_by(Month = substr(Date,4,5)) %>% 
  summarise(Average = mean(`Estimated Unemployment Rate (%)`))
View(Seasonal_pattern)

# Visuals
ggplot(data = Seasonal_pattern, mapping = aes(x = Month, y = Average, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Unemployment Rate by Month",
       x = "Month",
       y = "Average Unemployment Rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Question 3
# In terms of years 
# For the highest year
Highest_Years <- Unemployment_Data %>% group_by(Years = substr(Date,7,10)) %>% 
  summarise(Unempoyment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% 
  arrange(desc(Unempoyment_Rate)) %>% head(1)
View(Highest_Years)

# For the lowest year
Lowest_Years <- Unemployment_Data %>% group_by(Years = substr(Date,7,10)) %>% 
  summarise(Unempoyment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% 
  arrange(Unempoyment_Rate) %>% head(1)
View(Lowest_Years)

# For Highest month
Highest_Month <- Unemployment_Data %>% group_by(Month = substr(Date,4,5)) %>% 
  summarise(Unemployment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% 
  arrange(desc(Unemployment_Rate)) %>% arrange(desc(Unemployment_Rate)) %>% head(1)
print(Highest_Month)

# For the lowest month
Lowest_Month <- Unemployment_Data %>% group_by(substr(Date,4,5)) %>%
  summarise(Unemployment_Rate = sum(`Estimated Unemployment Rate (%)`)) %>% arrange(Unemployment_Rate) %>%
  head(1)

View(Lowest_Month)

# Question 4
# Region with the highest unemployment rate
Highest_Unemployment_Rate <- Unemployment_Data %>% group_by(Region) %>% summarise(Total = sum(`Estimated Unemployment Rate (%)`)) %>% arrange(desc(Total)) %>% head(1)
View(Highest_Unemployment_Rate)

# Region with the lowest unemployment rate
Lowest_Unemployment_Rate <- Unemployment_Data %>% group_by(Region) %>% 
  summarise(Total = sum(`Estimated Unemployment Rate (%)`)) %>% arrange(Total) %>% head(1)
View(Lowest_Unemployment_Rate)

# Question 5
Disperities <- Unemployment_Data %>% group_by(Region) %>% summarise(Total_Unemployed = sum(`Estimated Unemployment Rate (%)`),
                                                                    Total_Employed = sum(`Estimated Employed Rate (%)`))
View(Disperities)

# Question 6
cor(Unemployment_Data$`Estimated Labour Participation Rate (%)`, Unemployment_Data$`Estimated Unemployment Rate (%)`)

# Since the correlation is negative, there is no correlation between estimated labor participation rate and the unemployment rate

# Question 7 
Percentage <- Unemployment_Data %>% group_by(Region) %>% summarise(Sum_Employed = sum(`Estimated Employed`)) %>%
  mutate(Employed_Percentage = Sum_Employed/sum(Unemployment_Data$`Estimated Employed`) * 100)
View(Percentage)

# Visuals
Percentage_VIz <- select(Percentage, Region, Employed_Percentage) %>% arrange(desc(Employed_Percentage))
ggplot(data = Percentage_VIz, mapping = aes(y = Region, x = Employed_Percentage)) + geom_bar(stat = "identity",
                                                                                             color = "black",
                                                                                             fill = "red",
                                                                                             width = 0.8)
# My data is being arranged according to the regions instead of the Employed percentage to change this,
# we say:
Percentage_VIz <- Percentage_VIz %>%
  mutate(Region = fct_reorder(Region, Employed_Percentage))

ggplot(data = Percentage_VIz, mapping = aes(y = Region, x = Employed_Percentage)) + geom_bar(stat = "identity",
                                                                                             color = "black",
                                                                                             fill = "red",
                                                                                             width = 0.8) + 
  labs(title = "Region employed percentage",
       x = "Total percentage",
       y = "Regions") + theme_minimal()
# Question 8
States <- Unemployment_Data %>% group_by(Region) %>% summarise(Average = mean(`Estimated Employed`))

# Question 9
Employed_VS_Unemployed <- cor(Unemployment_Data$`Estimated Unemployment Rate (%)`,Unemployment_Data$`Estimated Employed Rate (%)`)
print(Employed_VS_Unemployed)

ggplot(data = Unemployment_Data, mapping = aes(x = `Estimated Unemployment Rate (%)`,
                                               y = `Estimated Employed Rate (%)`)) + 
  geom_point(color = "navy", alpha = 0.8, size = 2, pch = 10)
View(Labor_Visuals)

# Hence the correlation between the unemployment rate and the employed rate is negative.

# Question 10
Labour_Rate <- Unemployment_Data %>% group_by(Region) %>% summarise(Total = sum(`Estimated Labour Participation Rate (%)`))
View (Labour_Rate)
 
# Question 11
Labor_Unemployment <- cor(Unemployment_Data$`Estimated Labour Participation Rate (%)`,
                          Unemployment_Data$`Estimated Unemployment Rate (%)`)
 ggplot(data = Unemployment_Data, mapping = aes(x = `Estimated Unemployment Rate (%)`,
                        y = `Estimated Labour Participation Rate (%)`)) + 
   geom_point(color = "navy", alpha = 0.8, size = 2, pch = 10)
View(Labor_Visuals)