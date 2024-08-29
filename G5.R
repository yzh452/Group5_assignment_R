# 加载必要的R包
library(ggplot2)
library(dplyr)
library(lubridate)

# 读取数据
visitor_data<-read.csv('VisitorArrivals 28Jul2024.csv')

# 查看数据结构
str(visitor_data)



# 数据清理：去除包含 "Total" 的行（如果有）
visitor_data_clean <- subset(visitor_data, 
                             Country_of_residence != "Total" & 
                               NZ_port != "Total" & 
                               Length_of_stay != "Total" & 
                               Travel_purpose != "Total",
                             select = c(1, 7:11))


# 确保日期列格式正确
visitor_data_clean$Week_ended <- as.Date(visitor_data_clean$Week_ended, format="%Y-%m-%d")

# 提取年份和月份# 提取年份和月份NULL
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

# 使用 paste() 创建 Month-Year 标签
monthly_visitors$Month_Year <- factor(paste(monthly_visitors$Year, monthly_visitors$Month, sep = "-"))

# 绘制图表
ggplot(monthly_visitors, aes(x = Month_Year, y = total_visitors)) +
  geom_line(group = 1) +
  labs(title = "Monthly Visitor Numbers (May 2022 - June 2024)",
       x = "Month-Year",
       y = "Total Visitors") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 按国家汇总游客数量
country_visitors <- visitor_data_clean %>%
  group_by(Country_of_residence) %>%
  summarize(total_visitors = sum(Count)) %>%
  arrange(desc(total_visitors))

# 绘制各国游客数量柱状图
ggplot(country_visitors, aes(x = reorder(Country_of_residence, -total_visitors), y = total_visitors)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Visitor Numbers by Country",
       x = "Country",
       y = "Total Visitors")

# 按月汇总游客数量
monthly_visitors <- visitor_data_clean %>%
  filter(Year >= 2022 & Month >= 5) %>%
  group_by(Year, Month) %>%
  summarize(total_visitors = sum(Count))

# 绘制月度游客数量变化图
ggplot(monthly_visitors, aes(x = interaction(Year, Month), y = total_visitors)) +
  geom_line(group = 1) +
  labs(title = "Monthly Visitor Numbers (May 2022 - June 2024)",
       x = "Month-Year",
       y = "Total Visitors") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 按周汇总游客数量
weekly_visitors <- visitor_data_clean %>%
  filter(Year >= 2022 & Year <= 2024) %>%
  group_by(Week_ended) %>%
  summarize(total_visitors = sum(Count))

# 绘制周度游客数量变化图
ggplot(weekly_visitors, aes(x = Week_ended, y = total_visitors)) +
  geom_line(group = 1) +
  labs(title = "Weekly Visitor Numbers (May 2022 - June 2024)",
       x = "Week Ended",
       y = "Total Visitors") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 按国家和特征变量汇总游客数量
visitor_characteristics <- visitor_data_clean %>%
  group_by(Country_of_residence, Length_of_stay, Travel_purpose) %>%
  summarize(total_visitors = sum(Count))

# 绘制不同国家游客的停留时间分布
ggplot(visitor_characteristics, aes(x = Length_of_stay, y = total_visitors, fill = Travel_purpose)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country_of_residence) +
  labs(title = "Visitor Characteristics by Citizenship Group",
       x = "Length of Stay",
       y = "Total Visitors")

# 按入境港口和国家汇总游客数量
port_visitors <- visitor_data_clean %>%
  group_by(NZ_port, Country_of_residence) %>%
  summarize(total_visitors = sum(Count))

# 绘制入境港口选择的柱状图(use stack?)
ggplot(port_visitors, aes(x = NZ_port, y = total_visitors, fill = Country_of_residence)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "NZ Port Preferences by Country of Residence",
       x = "NZ Port",
       y = "Total Visitors")

# 添加一个时期变量：pre-covid, during-covid, post-covid
visitor_data_clean <- visitor_data_clean %>%
  mutate(period = case_when(
    Year < 2020 ~ "Pre-COVID",
    Year >= 2020 & Year <= 2021 ~ "During-COVID",
    Year > 2021 ~ "Post-COVID"
  ))

# 分析疫情前、中、后游客数量变化
covid_period_visitors <- visitor_data_clean %>%
  group_by(period, Year) %>%
  summarize(total_visitors = sum(Count))

# 绘制疫情前、中、后的游客数量变化图
ggplot(covid_period_visitors, aes(x = Year, y = total_visitors, color = period, group = period)) +
  geom_line() +
  geom_point() +
  labs(title = "Visitor Trends Pre, During, and Post COVID-19",
       x = "Year",
       y = "Total Visitors")+
  theme_bw()
