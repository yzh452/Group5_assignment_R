# load packages
library(ggplot2)
library(dplyr)
library(lubridate)

# read csv file
visitor_data<-read.csv('VisitorArrivals 28Jul2024.csv')

# check df structure
str(visitor_data)



# Data Cleaning: Remove rows containing "Total" 
visitor_data_clean <- subset(visitor_data, 
                             Country_of_residence != "Total" & 
                               NZ_port != "Total" & 
                               Length_of_stay != "Total" & 
                               Travel_purpose != "Total",
                             select = c(1, 7:11))


# formatting data column
visitor_data_clean$Week_ended <- as.Date(visitor_data_clean$Week_ended, format="%Y-%m-%d")

# extract year & month
visitor_data_clean$Year <- year(visitor_data_clean$Week_ended)
visitor_data_clean$Month <- month(visitor_data_clean$Week_ended, label = TRUE)

annual_visitors <- visitor_data_clean %>%
  group_by(Year) %>%
  summarize(total_visitors = sum(Count))

ggplot(annual_visitors, aes(x = Year, y = total_visitors)) +
  geom_line() +
  geom_point() +
  labs(title = "Annual International Visitor Trends",
       x = "Year",
       y = "Total Visitors")+
  theme_bw()

monthly_visitors <- visitor_data_clean %>%
  filter(Year >= 2022 & Month >= 5) %>%
  group_by(Year, Month) %>%
  summarize(total_visitors = sum(Count))

# create column for Month_Year lables
monthly_visitors$Month_Year <- factor(paste(monthly_visitors$Year, monthly_visitors$Month, sep = "-"))

# plotting
ggplot(monthly_visitors, aes(x = Month_Year, y = total_visitors)) +
  geom_line(group = 1) +
  labs(title = "Monthly Visitor Numbers (May 2022 - June 2024)",
       x = "Month-Year",
       y = "Total Visitors") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# summarise the number of tourists by country
country_visitors <- visitor_data_clean %>%
  group_by(Country_of_residence) %>%
  summarize(total_visitors = sum(Count)) %>%
  arrange(desc(total_visitors))

# plot bar chart for tourist by country
ggplot(country_visitors, aes(x = reorder(Country_of_residence, -total_visitors), y = total_visitors)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Visitor Numbers by Country",
       x = "Country",
       y = "Total Visitors")

# summarise the number of tourists by months
monthly_visitors <- visitor_data_clean %>%
  filter(Year >= 2022 & Month >= 5) %>%
  group_by(Year, Month) %>%
  summarize(total_visitors = sum(Count))

# plot the changes in the number of tourist by month
ggplot(monthly_visitors, aes(x = interaction(Year, Month), y = total_visitors)) +
  geom_line(group = 1) +
  labs(title = "Monthly Visitor Numbers (May 2022 - June 2024)",
       x = "Month-Year",
       y = "Total Visitors") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# tourist number by week
weekly_visitors <- visitor_data_clean %>%
  filter(Year >= 2022 & Year <= 2024) %>%
  group_by(Week_ended) %>%
  summarize(total_visitors = sum(Count))

# plot the changes by week
ggplot(weekly_visitors, aes(x = Week_ended, y = total_visitors)) +
  geom_line(group = 1) +
  labs(title = "Weekly Visitor Numbers (May 2022 - June 2024)",
       x = "Week Ended",
       y = "Total Visitors") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Summarize the number of tourists by country and characteristic variables (length of stay, purpose)
visitor_characteristics <- visitor_data_clean %>%
  group_by(Country_of_residence, Length_of_stay, Travel_purpose) %>%
  summarize(total_visitors = sum(Count))

# plot the distribution of length of stay by countries
ggplot(visitor_characteristics, aes(x = Length_of_stay, y = total_visitors, fill = Travel_purpose)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country_of_residence) +
  labs(title = "Visitor Characteristics by Citizenship Group",
       x = "Length of Stay",
       y = "Total Visitors")

# summarise by entry port and country
port_visitors <- visitor_data_clean %>%
  group_by(NZ_port, Country_of_residence) %>%
  summarize(total_visitors = sum(Count))

# bar chart by entry port (better use stack?)
ggplot(port_visitors, aes(x = NZ_port, y = total_visitors, fill = Country_of_residence)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "NZ Port Preferences by Country of Residence",
       x = "NZ Port",
       y = "Total Visitors")

# add a period variable：pre-covid, during-covid, post-covid
visitor_data_clean <- visitor_data_clean %>%
  mutate(period = case_when(
    Year < 2020 ~ "Pre-COVID",
    Year >= 2020 & Year <= 2021 ~ "During-COVID",
    Year > 2021 ~ "Post-COVID"
  ))

# compare the changes in number before during and after Covid (better find 5 years data)
covid_period_visitors <- visitor_data_clean %>%
  group_by(period, Year) %>%
  summarize(total_visitors = sum(Count))

# plotting- changes in number before during and after
ggplot(covid_period_visitors, aes(x = Year, y = total_visitors, color = period, group = period)) +
  geom_line() +
  geom_point() +
  labs(title = "Visitor Trends Pre, During, and Post COVID-19",
       x = "Year",
       y = "Total Visitors")+
  theme_bw()
