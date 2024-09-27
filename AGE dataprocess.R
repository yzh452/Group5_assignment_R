#1st dataclean
# Load necessary libraries
library(dplyr)
library(lubridate)

# Load the datasets
data1 <- read.csv("ITM524901_20240902_114033_41.csv")
data2 <- read.csv("2019-21_dailydata.csv")
data3 <- read.csv("2022_24_dailydata.csv")

# Combine the relevant datasets for analysis
combined_data <- bind_rows(data2, data3)

# Convert date column to Date format
combined_data$date <- mdy(combined_data$date)

# Remove unnecessary columns that aren't needed for age analysis
# Adjust the column names based on your specific dataset structure
combined_data <- combined_data %>%
  select(date, age_at_travel_range, total_movements)

# Check for missing values and handle them
# For example, replace NA in total_movements with 0 if that makes sense contextually
combined_data <- combined_data %>%
  mutate(total_movements = ifelse(is.na(total_movements), 0, total_movements))

# Segregate data into three periods: before, during, and after COVID-19
before_covid <- combined_data %>% filter(date < as.Date("2020-03-01"))
during_covid <- combined_data %>% filter(date >= as.Date("2020-03-01") & date <= as.Date("2021-12-31"))
after_covid <- combined_data %>% filter(date > as.Date("2021-12-31"))

# View summary to check data after cleaning
summary(combined_data)


#2nd visualaiztion 
library(ggplot2)
# Aggregate data by age group and period
age_summary_before <- before_covid %>%
  group_by(age_at_travel_range) %>%
  summarize(total_movements = sum(total_movements, na.rm = TRUE)) %>%
  mutate(period = "Before COVID-19")

age_summary_during <- during_covid %>%
  group_by(age_at_travel_range) %>%
  summarize(total_movements = sum(total_movements, na.rm = TRUE)) %>%
  mutate(period = "During COVID-19")

age_summary_after <- after_covid %>%
  group_by(age_at_travel_range) %>%
  summarize(total_movements = sum(total_movements, na.rm = TRUE)) %>%
  mutate(period = "After COVID-19")

# Combine all periods into one dataframe for plotting
age_summary <- bind_rows(age_summary_before, age_summary_during, age_summary_after)

# Factor the period column to ensure correct ordering in plots
age_summary$period <- factor(age_summary$period, levels = c("Before COVID-19", "During COVID-19", "After COVID-19"))

# Create a bar plot comparing the age groups across the three periods
ggplot(age_summary, aes(x = age_at_travel_range, y = total_movements, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) + # Adjust dodge width for separation
  labs(title = "Impact of Age Groups on Tourism Before, During, and After COVID-19",
       x = "Age Group",
       y = "Total Movements") +
  theme_minimal() +
  scale_fill_manual(values = c("Before COVID-19" = "#00AFBB", "During COVID-19" = "#E7B800", "After COVID-19" = "#FC4E07")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a line chart to show trends over time with ordered periods
ggplot(age_summary, aes(x = period, y = total_movements, group = age_at_travel_range, color = age_at_travel_range)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  labs(title = "Trends in Tourism by Age Group Across COVID-19 Periods",
       x = "Time Period",
       y = "Total Movements") +
  theme_minimal() +
  scale_x_discrete(limits = c("Before COVID-19", "During COVID-19", "After COVID-19")) + # Ensure correct x-axis order
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


#3rd Statistical tests to comapre period  
library(car)
# Prepare the data by grouping by age group and period
age_summary <- combined_data %>%
  mutate(period = case_when(
    date < as.Date("2020-03-01") ~ "Before COVID-19",
    date >= as.Date("2020-03-01") & date <= as.Date("2021-12-31") ~ "During COVID-19",
    date > as.Date("2021-12-31") ~ "After COVID-19"
  )) %>%
  group_by(age_at_travel_range, period) %>%
  summarize(total_movements = sum(total_movements, na.rm = TRUE))

# Filter for a specific age group (e.g., "18 - 39 years")
age_18_39 <- age_summary %>% filter(age_at_travel_range == "18 - 39 years")

# Perform ANOVA to compare periods for this age group
anova_result_18_39 <- aov(total_movements ~ period, data = age_18_39)
summary(anova_result_18_39)

# Check assumptions of ANOVA using Levene's Test for homogeneity of variances
leveneTest(total_movements ~ period, data = age_18_39)

# If ANOVA assumptions are not met, perform Kruskal-Wallis Test
kruskal_test_result_18_39 <- kruskal.test(total_movements ~ period, data = age_18_39)
kruskal_test_result_18_39



# Filter for a specific age group (e.g., "01 - 17 years")
age_01_17 <- age_summary %>% filter(age_at_travel_range == "01 - 17 years")

# Perform ANOVA to compare periods for this age group
anova_result_01_17 <- aov(total_movements ~ period, data = age_01_17)
summary(anova_result_01_17)

# Check assumptions of ANOVA using Levene's Test for homogeneity of variances
leveneTest(total_movements ~ period, data = age_01_17)

# If ANOVA assumptions are not met, perform Kruskal-Wallis Test
kruskal_test_result_01_17 <- kruskal.test(total_movements ~ period, data = age_01_17)
kruskal_test_result_01_17



# Filter for a specific age group (e.g., "40 - 69 years")
age_40_69 <- age_summary %>% filter(age_at_travel_range == "40 - 69 years")

# Perform ANOVA to compare periods for this age group
anova_result_40_69 <- aov(total_movements ~ period, data = age_40_69)
summary(anova_result_40_69)

# Check assumptions of ANOVA using Levene's Test for homogeneity of variances
leveneTest(total_movements ~ period, data = age_40_69)

# If ANOVA assumptions are not met, perform Kruskal-Wallis Test
kruskal_test_result_40_69 <- kruskal.test(total_movements ~ period, data = age_40_69)
kruskal_test_result_40_69

#Regression test
#Prepare the data by creating factors for period and age group
combined_data <- combined_data %>%
  mutate(period = case_when(
    date < as.Date("2020-03-01") ~ "Before COVID-19",
    date >= as.Date("2020-03-01") & date <= as.Date("2021-12-31") ~ "During COVID-19",
    date > as.Date("2021-12-31") ~ "After COVID-19"
  )) %>%
  mutate(period = factor(period, levels = c("Before COVID-19", "During COVID-19", "After COVID-19")),
         age_at_travel_range = factor(age_at_travel_range))

# Check the structure of the data to ensure it's ready for regression
str(combined_data)

# Fit a linear regression model with total movements as the response variable
# period and age group as predictors
regression_model <- lm(total_movements ~ period + age_at_travel_range, data = combined_data)
summary(regression_model)

# Optional: Check model diagnostics
par(mfrow = c(2, 2))
plot(regression_model)

# If the assumptions of linear regression are violated, consider a Poisson regression
# Fit a generalized linear model (GLM) with Poisson distribution
glm_model <- glm(total_movements ~ period + age_at_travel_range, 
                 family = poisson(link = "log"), data = combined_data)
summary(glm_model)

# Check model diagnostics for the GLM
plot(glm_model)


# Visualize the regression results

# 1. Effect Plot: Visualizing the impact of 'period' on 'total_movements'
effect_plot_period <- plot(allEffects(regression_model), which = "period",
                           main = "Effect of Period on Visitor Movements",
                           xlab = "Period", ylab = "Total Movements")

# 2. Effect Plot: Visualizing the impact of 'age_at_travel_range' on 'total_movements'
effect_plot_age <- plot(allEffects(regression_model), which = "age_at_travel_range",
                        main = "Effect of Age Group on Visitor Movements",
                        xlab = "Age Group", ylab = "Total Movements")

# 3. Predicted vs. Actual Plot
# Create a data frame with predicted values from the regression model
pred_data <- combined_data %>%
  mutate(predicted_values = predict(regression_model, newdata = combined_data))

# Plotting predicted vs actual values
ggplot(pred_data, aes(x = predicted_values, y = total_movements)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Predicted vs. Actual Visitor Movements",
       x = "Predicted Values", y = "Actual Values") +
  theme_minimal()

# 4. Residual Plot to check model fit
ggplot(pred_data, aes(x = predicted_values, y = residuals(regression_model))) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()




library(cluster)
library(factoextra)
# Prepare the data by selecting and standardizing relevant features for clustering
# Select numeric variables and standardize them for clustering
cluster_data <- combined_data %>%
  select(total_movements, period, age_at_travel_range) %>%
  mutate(period = as.numeric(as.factor(period)),  # Convert factors to numeric
         age_at_travel_range = as.numeric(as.factor(age_at_travel_range))) %>%
  scale()  # Standardize data

# Determine the optimal number of clusters using the Elbow Method
fviz_nbclust(cluster_data, kmeans, method = "wss") + 
  labs(title = "Elbow Method for Optimal Clusters")

# Fit K-Means clustering with the chosen number of clusters (e.g., k = 3)
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(cluster_data, centers = 3, nstart = 25)

# Add cluster assignment to the original data
combined_data$cluster <- kmeans_result$cluster

# Visualize the clustering results
fviz_cluster(kmeans_result, data = cluster_data, 
             geom = "point", ellipse.type = "convex") +
  labs(title = "K-Means Clustering of Visitor Movements")

# Print cluster centers to understand the characteristics of each cluster
print(kmeans_result$centers)

# Optional: Perform Hierarchical Clustering
# Create a distance matrix and perform hierarchical clustering
distance_matrix <- dist(cluster_data, method = "euclidean")
hclust_result <- hclust(distance_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hclust_result, labels = FALSE, main = "Dendrogram of Visitor Movements Clustering")
rect.hclust(hclust_result, k = 3, border = "red")  # Cut the tree into 3 clusters



#Cluster algorithm 
# Add cluster assignment to the original data for further analysis
combined_data$cluster <- as.factor(kmeans_result$cluster)

# View the cluster centers to understand the average characteristics of each cluster
cluster_centers <- kmeans_result$centers
print(cluster_centers)

# Summarize each cluster by key statistics for interpretation
cluster_summary <- combined_data %>%
  group_by(cluster) %>%
  summarize(
    mean_movements = mean(total_movements, na.rm = TRUE),
    median_movements = median(total_movements, na.rm = TRUE),
    count = n(),
    mean_period = mean(as.numeric(as.factor(period)), na.rm = TRUE),
    mean_age = mean(as.numeric(as.factor(age_at_travel_range)), na.rm = TRUE)
  )

print(cluster_summary)

# Manually interpret each cluster based on the summary and cluster centers
# For instance, assign descriptive labels to clusters based on characteristics
# Replace the cluster numbers with meaningful names
combined_data$cluster_label <- case_when(
  combined_data$cluster == 1 ~ "High Movements, Pre-COVID, Young Age Group",
  combined_data$cluster == 2 ~ "Low Movements, COVID Period, Mixed Ages",
  combined_data$cluster == 3 ~ "Moderate Movements, Post-COVID, Older Age Group",
  TRUE ~ as.character(combined_data$cluster)
)

# Summarize each cluster by key statistics for interpretation
cluster_summary <- combined_data %>%
  group_by(cluster_label) %>%
  summarize(
    mean_movements = mean(total_movements, na.rm = TRUE),
    median_movements = median(total_movements, na.rm = TRUE),
    count = n()
  )

print(cluster_summary)

# Simplified x-axis labels for readability
simplified_labels <- c(
  "High Movements, Pre-COVID",
  "Low Movements, COVID Period",
  "Moderate Movements, Post-COVID"
)

# Visualize mean movements by descriptive cluster labels
ggplot(cluster_summary, aes(x = cluster_label, y = mean_movements, fill = cluster_label)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = simplified_labels) +
  labs(title = "Mean Visitor Movements by Descriptive Clusters",
       x = "Cluster Description",
       y = "Mean Movements") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Cluster Groups")) 
