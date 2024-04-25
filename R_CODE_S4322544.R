#setwd("C:/Users/Aleena Omair/Desktop/R")
#getwd()

library(ggplot2)
library(GGally)
library(corrplot)
library(readr)
library(tidyverse)
library(devtools)
library(dplyr)
library(purrr)
library(ggpubr)
library(factoextra)
library(e1071)

dg = read.csv("C:/Users/Aleena Omair/Desktop/R/My_Dataset2.csv")

summary(dg)
head(dg)
str(dg)
row.names(dg)

sum(is.na(dg))
dim(dg)
dg2 <- na.omit(dg)
sum(is.na(dg2))
dim(dg2)
dg_New <- dg2
# Hypothesis 01 (Number of homicide convictions, number of unsuccessful convictions)
hyp1 <- ggplot(dg_New, aes(x=dg_New$Number.of.Homicide.Convictions, 
                             y=dg_New$Number.of.Drugs.Offences.Convictions)) + geom_point()+
  labs(x = "Homicide Convictions", 
       y = "Drugs Offences Convictions")
hyp1


# Hypothesis 02
hyp2 <- ggplot(dg_New, aes(x=dg_New$Number.of.Burglary.Convictions, 
                           y=dg_New$Percentage.of.Burglary.Convictions)) + geom_point()+
  labs(x="No. of Burglary Convictions",
       y="Percentage of Burglary Convictions")
hyp2

# Scatter Plot
scatterplot <- ggplot(dg_New, aes(x=dg_New$Number.of.Burglary.Convictions, 
                           y=dg_New$Percentage.of.Burglary.Convictions)) + geom_point()
scatterplot

scatterplot <- ggplot(dg_New, aes(x=dg_New$Number.of.Burglary.Convictions, 
                                  y=dg_New$Percentage.of.Burglary.Convictions)) + geom_point()
scatterplot

scatterplot <- ggplot(dg_New, aes(x=dg_New$Number.of.Burglary.Convictions, 
                                  y=dg_New$Percentage.of.Burglary.Convictions)) + geom_point()
scatterplot


ggplot(dg2, aes(x = "", y = dg2$Number.of.Fraud.And.Forgery.Unsuccessful, fill = "Box")) +
  geom_boxplot() +
  ggtitle("Number.of.Fraud.And.Forgery.Unsuccessful") +
  theme_minimal() +
  scale_fill_manual(values = "darkgreen")

ggplot(dg2, aes(x = "", y = dg2$Number.of.Admin.Finalised.Unsuccessful, fill = "Box")) +
  geom_boxplot() +
  ggtitle("Number.of.Admin.Finalised.Unsuccessful") +
  theme_minimal() +
  scale_fill_manual(values = "darkgreen")

# Function to identify outliers based on IQR and convert to NA
iqrdetection <- function(column) {
  Q1 <- quantile(column, 0.20)
  Q3 <- quantile(column, 0.80)
  IQR_value <- Q3 - Q1
  outliers <- which(column < (Q1 - 1.5 * IQR_value) | column > (Q3 + 1.5 * IQR_value))
  column[outliers] <- NA
  return(column)
}

# Apply the function to all numeric columns in the dataframe
colsN <- sapply(dg2, is.numeric)
dg2[, colsN] <- lapply(dg2[, colsN], iqrdetection)

sum(is.na(dg2))
dim(dg2)

dg_filled <- as.data.frame(lapply(dg2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
sum(is.na(dg_filled))
dim(dg_filled)

ggplot(dg2, aes(x = "", y = dg2$Number.of.Fraud.And.Forgery.Unsuccessful, fill = "Box")) +
  geom_boxplot() +
  ggtitle("Number.of.Fraud.And.Forgery.Unsuccessful") +
  theme_minimal() +
  scale_fill_manual(values = "lightgreen")

ggplot(dg2, aes(x = "", y = dg2$Number.of.Admin.Finalised.Unsuccessful, fill = "Box")) +
  geom_boxplot() +
  ggtitle("Number.of.Admin.Finalised.Unsuccessful") +
  theme_minimal() +
  scale_fill_manual(values = "lightgreen")

# Descriptive

str(dg_filled)
ggplot(dg_filled, aes(x = `Number.of.Admin.Finalised.Unsuccessful`)) +
  geom_density(fill='pink') + 
  labs(title = "Number.of.Admin.Finalised.Unsuccessful")

ggplot(dg_filled, aes(x = `Number.of.Fraud.And.Forgery.Unsuccessful`)) +
  geom_density(fill='pink') + 
  labs(title = "Number.of.Fraud.And.Forgery.Unsuccessful")

# Assuming that "Month" is a character variable, convert it to a factor with the desired order
dg_filled$Month <- factor(dg_filled$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Plot the bar chart with the correct order of months
ggplot(dg_filled, aes(x = Month, fill = "red")) +
  geom_bar() +
  labs(title = "Bar Chart Showing Months of Dataset", x = "Months", y = "Count")

ggplot(dg_filled,aes(x=dg_filled$Month, fill="red")) +
  geom_bar() + 
  labs(title = "Bar Chart Showing Months of Dataset", x="Months", y="Count")

ggplot(dg_filled,aes(x=`Year`, fill="red")) +
  geom_bar() + labs(title = "Bar Chart Showing Year taken in dataset", x="Year",y="count")

ggplot(dg_filled,aes(x= dg_filled$Number.of.Homicide.Convictions)) + 
  geom_bar(position = position_dodge()) +
  labs(title = "BAR CHART",x= "Number of Homicide Convictions", y= "Count")

ggplot(dg_filled, aes(x = dg_filled$Number.of.Robbery.Convictions)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "HISTOGRAM SHOWING ROBBERIES",
       x = "Number of Robbery Convictions",
       y = "Frequency") +
  theme_minimal()

ggplot(dg_filled, aes(x = dg_filled$Number.of.Theft.And.Handling.Unsuccessful)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "HISTOGRAM SHOWING Theft and Handling Unsuccessful",
       x = "Number of Theft Convictions",
       y = "Frequency") +
  theme_minimal()

ggplot(dg_filled, aes(x = dg_filled$Number.of.Sexual.Offences.Unsuccessful
)) +
  geom_density(fill = "purple", color = "black") +
  labs(title = "Density Plot showing Sexual.Offences.Unsuccessful",
       x = "Sexual.Offences.Unsuccessful",
       y = "Density") +
  theme_minimal()

pie_data <- table(dg_filled$Year)
pie_chart <- pie(pie_data, labels = paste(names(pie_data), "\n", pie_data), main = "Pie Chart")

pie_data <- table(dg_filled$Month)
pie_chart <- pie(pie_data, labels = paste(names(pie_data), "\n", pie_data), main = "Pie Chart")

# Assuming you have a vector of custom colors
custom_colors <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "grey", "yellow", "cyan", "magenta", "darkgreen")

# Create a table of counts for each month
pie_data <- table(dg_filled$Month)

# Create a pie chart with custom colors
pie_chart <- pie(pie_data, labels = paste(names(pie_data), "\n", pie_data), main = "Pie Chart", col = custom_colors)

# Data Normalization
# Separate numeric and categorical columns
numeric_cols <- sapply(dg_filled, is.numeric)
categorical_cols <- dg_filled[, !numeric_cols, drop = FALSE]

# Normalize numeric columns
normalized_numeric <- as.data.frame(scale(dg_filled[, numeric_cols]))

# Combine normalized numeric columns with original categorical columns
dg_normalized <- cbind(normalized_numeric, categorical_cols)

str(dg_normalized)

percentage_cols <- sapply(dg_normalized, function(x) any(grepl("%", x)))

dg_New <- dg_normalized %>%
  mutate_at(vars(which(percentage_cols)), ~{
    numeric_values <- as.numeric(gsub("%", "", .))
    coalesce(numeric_values, NA)
  })

dg_New$County <- as.integer(factor(dg_New$County))
dg_New$Month <- as.integer(factor(dg_New$Month))

str(dg_New)

numeric_data <- dg_New %>%
  select_if(is.numeric)
cor_matrix <- cor(numeric_data)
print(cor_matrix)
print(colnames(cor_matrix))

# Machine Learning
# 01 (Linear Regression)
target_label <- "Number.of.Robbery.Convictions"
print(colnames(cor_matrix))
if (target_label %in% colnames(cor_matrix)) {
  cor_with_target <- cor_matrix[target_label, ]
    top5 <- names(sort(cor_with_target, decreasing = TRUE)[2:6])
    print(top5)
} else {
  cat("Target label not found in the correlation matrix.")
}

linearReg <- lm(dg_New$Number.of.Robbery.Convictions ~ 
                  dg_New$Number.of.Admin.Finalised.Unsuccessful + dg_New$Number.of.Public.Order.Offences.Unsuccessful + dg_New$Number.of.Theft.And.Handling.Unsuccessful
                , data = dg_New)
summary(linearReg)

lrPLOT <- ggplot(dg_New, aes(x=dg_New$Number.of.Robbery.Convictions, 
                             y=dg_New$Number.of.Admin.Finalised.Unsuccessful)) + geom_point()
lrPLOT

# 02 (K Means Clustering)
dg_New_top5 <- dg_New[, c("Number.of.Robbery.Convictions", top5)]
km.res <- kmeans(dg_New_top5, 3, nstart = 25)
km.res

fviz_cluster(km.res, data = dg_New_top5,
             palette = c("blue", "green", "purple"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# 03 Classification SVM
target_label <- "Number.of.Robbery.Convictions"
print(colnames(cor_matrix))
if (target_label %in% colnames(cor_matrix)) {
  cor_with_target <- cor_matrix[target_label, ]
  top5C <- names(sort(cor_with_target, decreasing = TRUE)[2:6])
  print(top5C)
} else {
  cat("Target label not found in the correlation matrix.")
}
dg_New_top5_Cls <- dg_New[, c("County", top5C)]

# Split your data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(dg_New_top5_Cls), 0.8 * nrow(dg_New_top5_Cls))
train_data <- dg_New_top5_Cls[train_indices, ]
test_data <- dg_New_top5_Cls[-train_indices, ]
target_variable <- "County"

random_val1 <- 7
random_val2 <- 0.9

svm_model <- svm(as.factor(County) ~ ., data = train_data, kernel = "radial", cost = random_val1, gamma = random_val2)
svm_model
predictions <- predict(svm_model, test_data)

# Evaluate the model
accuracy <- sum(predictions == test_data$County) / nrow(test_data)
print(paste("Accuracy:", round(accuracy, 4)))

# Confusion Matrix
conf_matrix <- table(Actual = test_data$County, Predicted = predictions)
print("Confusion Matrix:")
print(conf_matrix)
