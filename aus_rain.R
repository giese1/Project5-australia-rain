# packages

library(tidyverse)
library(plotly)
library(caret)
library(party)
library(randomForest)
library(e1071)
library(neuralnet)
library(rpart.plot)
library(cowplot)
library(corrplot)
library(DMwR)

# loading dataset

df_data <- read.csv("weatherAUS.csv", header = TRUE, sep = ",")

str(df_data)

summary(df_data)

# transforming date variable into date

df_data$Date <- as.POSIXct(df_data$Date, format = "%Y-%m-%d")
df_data$Date

df_data$Date <- as.Date(df_data$Date)

# minimum temperature plot

a <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = MinTemp), 
            color = "blue",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Temperature" ,
       title = "Min temperature along the years")

a

# maximum temperature plot

b <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = MaxTemp), 
            color = "red",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Temperature" ,
       title = "Max temperature along the years")

b

plot_grid(a, b, ncol=2, nrow=1)

# wind speed in 9am plot

a <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = WindSpeed9am), 
            color = "red",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "wind (km/hr)" ,
       title = "Wind speed in 9am along the years")

a

# wind speed in 3pm plot

b <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = WindSpeed3pm), 
            color = "blue",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "wind (km/hr)" ,
       title = "Wind speed in 3pm along the years")

b

plot_grid(a, b, ncol=2, nrow=1)

# humidity in 9am plot

a <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Humidity9am), 
            color = "red",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Humidity (%)" ,
       title = "Humidity in 9am along the years")

a

# humidity in 3pm plot

b <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Humidity3pm), 
            color = "blue",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Humidity (%)" ,
       title = "Humidity in 3pm along the years")

b

plot_grid(a, b, ncol=2, nrow=1)

# pressure in 9am plot

a <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Pressure9am), 
            color = "red",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Pressure (hpa)" ,
       title = "Pressure in 9am along the years")

a

# pressure in 3pm plot

b <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Pressure3pm), 
            color = "blue",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Pressure (hpa)" ,
       title = "Pressure in 3pm along the years")

b

plot_grid(a, b, ncol=2, nrow=1)

# cloud presence in 9am in sky plot

a <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Cloud9am), 
            color = "red",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Clouds (oktas)" ,
       title = "Fraction of sky obscured by clouds in 9am along the years")

a

# cloud presence in 3pm in sky plot

b <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Cloud3pm), 
            color = "blue",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Clouds (oktas)" ,
       title = "Fraction of sky obscured by clouds in 3pm along the years")

b

plot_grid(a, b, ncol=2, nrow=1)

# Temperature in 9am plot

a <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Temp9am), 
            color = "red",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Temperature (C)" ,
       title = "Temperature in 9am along the years")

a

# Temperature in 3pm plot

b <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Temp3pm), 
            color = "blue",
            alpha = 0.6,
            size = 0.6) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Temperature (C)" ,
       title = "Temperature in 3pm along the years")

b

plot_grid(a, b, ncol=2, nrow=1)

# Frequency of the locations column

freq_location <- data.frame(cbind(Frequency = table(df_data$Location), Percent = prop.table(table(df_data$Location)) * 100))
freq_location
str(freq_location)

# Histogram of frequency of weathers measurements locations

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
a <- ggplot(data = freq_location, mapping = aes(x = Frequency, y = row.names(freq_location))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_location), color = row.names(freq_location)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 3, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Frequency of locations") +
  tema
a

# frequency of strongest wind gust direction

freq_wind_dir <- data.frame(cbind(Frequency = table(df_data$WindGustDir), Percent = prop.table(table(df_data$WindGustDir)) * 100))
freq_wind_dir
str(freq_wind_dir)

# Histogram of frequency of strongest wind gust direction

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
b <- ggplot(data = freq_wind_dir, mapping = aes(x = Frequency, y = row.names(freq_wind_dir))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_wind_dir), color = row.names(freq_wind_dir)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 3, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Frequency of locations") +
  tema
b

# frequency of wind direction in 9am

freq_wind_dir9 <- data.frame(cbind(Frequency = table(df_data$WindDir9am), Percent = prop.table(table(df_data$WindDir9am)) * 100))
freq_wind_dir9
str(freq_wind_dir9)

# Histogram of frequency of wind direction in 9am

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
a <- ggplot(data = freq_wind_dir9, mapping = aes(x = Frequency, y = row.names(freq_wind_dir9))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_wind_dir9), color = row.names(freq_wind_dir9)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 3, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Frequency of locations") +
  tema
a

# frequency of wind direction in 3pm

freq_wind_dir3 <- data.frame(cbind(Frequency = table(df_data$WindDir3pm), Percent = prop.table(table(df_data$WindDir3pm)) * 100))
freq_wind_dir3
str(freq_wind_dir3)

# Histogram of frequency of wind direction in 3pm

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
b <- ggplot(data = freq_wind_dir3, mapping = aes(x = Frequency, y = row.names(freq_wind_dir3))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_wind_dir3), color = row.names(freq_wind_dir3)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 3, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Frequency of locations") +
  tema
b

# frequency table if it rains or not

freq_rain_today <- data.frame(cbind(Frequency = table(df_data$RainToday), Percent = prop.table(table(df_data$RainToday)) * 100))
freq_rain_today
str(freq_rain_today)

# histogram of frquency if it rains in the day or not

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
a <- ggplot(data = freq_rain_today, mapping = aes(x = Frequency, y = row.names(freq_rain_today))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_rain_today), color = row.names(freq_rain_today)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 3, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Frequency of locations") +
  tema
a

# removing NA values from dataset

df_data$Evaporation <- NULL
df_data$Sunshine <- NULL

df_data <-na.omit(df_data)

# Temperature in 9am and if it rains or not along the years

a <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Temp9am, col = RainToday), 
            alpha = 0.6,
            size = 0.6) +
  scale_color_manual(labels = c("No", "Yes"),
                     values = c("red", "blue")) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Temperature (C)" ,
       title = "Temperature in 9am and rain in the day or not along the years")

a

# Temperature in 3pm and if it rains or not along the years

b <- ggplot(data = df_data) +
  geom_line(aes(x = Date, y = Temp3pm, col = RainToday), 
            alpha = 0.6,
            size = 0.6) +
  scale_color_manual(labels = c("No", "Yes"),
                     values = c("red", "blue")) +
  theme_minimal() +
  labs(x = "Date", 
       y = "Temperature (C)" ,
       title = "Temperature in 3pm and rain in the day or not along the years")

b

plot_grid(a, b, ncol=2, nrow=1)

#### Pre proecessing ####

# removing unecessary columns for machine learning

df_data$Date <- NULL
df_data$Location <- NULL
df_data$WindGustDir <- NULL
df_data$WindDir9am <- NULL
df_data$WindDir3pm <- NULL
df_data$RainToday <- NULL

# spliting data into training and test

indexes <- sample(1:nrow(df_data), size = 0.7 * nrow(df_data))
train.data <- df_data[indexes,]
test.data <- df_data[-indexes,]
class(train.data)
class(test.data)

str(train.data)

prop.table(table(train.data$RainTomorrow)) * 100

# balancing target variable with SMOTE

train.data.balanced <- SMOTE(RainTomorrow ~ ., train.data, perc.over = 100, perc.under = 200)

# checking balanced target

prop.table(table(train.data.balanced$RainTomorrow)) * 100

#### Machine learning ####

# svm machine learning model

set.seed(123)

ma_model <- svm(RainTomorrow ~. ,data = train.data.balanced)
summary(ma_model)
print(ma_model)

# prevision if rains tomorrow or not (80%)

pred_model <- predict(ma_model, test.data)



table(pred_model, test.data$RainTomorrow)

confusionMatrix(pred_model, test.data$RainTomorrow)

# RainTomorrow predictive data

pred_model_plot <- as.data.frame(pred_model)
names(pred_model_plot) <- c("RainTomorrow")

names(pred_model_plot)


tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = pred_model_plot , mapping = aes(x = as.numeric(RainTomorrow))) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RainTomorrow") +
  ggtitle("Rains tomorrow or not predictive data") +
  tema

a

# RainTomorrow real data

options(repr.plot.width=14, repr.plot.height=6)
b <- ggplot(data = test.data , mapping = aes(x = as.numeric(RainTomorrow))) +
  geom_histogram(fill = "red", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RainTomorrow") +
  ggtitle("Rains tomorrow or not real data") +
  tema

b

plot_grid(a, b, ncol = 2, nrow = 1)

# naive bayes machine learning model

ma_model <- naiveBayes(RainTomorrow ~. ,data = train.data.balanced)
summary(ma_model)
print(ma_model)

# prevision if rains tomorrow or not (77%)

pred_model <- predict(ma_model, test.data)



table(pred_model, test.data$RainTomorrow)

confusionMatrix(pred_model, test.data$RainTomorrow)

# RainTomorrow predictive data

pred_model_plot <- as.data.frame(pred_model)
names(pred_model_plot) <- c("RainTomorrow")

names(pred_model_plot)


tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = pred_model_plot , mapping = aes(x = as.numeric(RainTomorrow))) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RainTomorrow") +
  ggtitle("Rains tomorrow or not predictive data") +
  tema

a

# RainTomorrow real data

options(repr.plot.width=14, repr.plot.height=6)
b <- ggplot(data = test.data , mapping = aes(x = as.numeric(RainTomorrow))) +
  geom_histogram(fill = "red", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RainTomorrow") +
  ggtitle("Rains tomorrow or not real data") +
  tema

b

plot_grid(a, b, ncol = 2, nrow = 1)


# random forest machine learning model

ma_model <- randomForest(RainTomorrow ~. ,data = train.data.balanced)
summary(ma_model)
plot(ma_model)

# prevision if rains tomorrow or not (80%)

pred_model <- predict(ma_model, test.data)



table(pred_model, test.data$RainTomorrow)

confusionMatrix(pred_model, test.data$RainTomorrow)

# RainTomorrow predictive data

pred_model_plot <- as.data.frame(pred_model)
names(pred_model_plot) <- c("RainTomorrow")

names(pred_model_plot)


tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = pred_model_plot , mapping = aes(x = as.numeric(RainTomorrow))) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RainTomorrow") +
  ggtitle("Rains tomorrow or not predictive data") +
  tema

a

# RainTomorrow real data

options(repr.plot.width=14, repr.plot.height=6)
b <- ggplot(data = test.data , mapping = aes(x = as.numeric(RainTomorrow))) +
  geom_histogram(fill = "red", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("RainTomorrow") +
  ggtitle("Rains tomorrow or not real data") +
  tema

b

plot_grid(a, b, ncol = 2, nrow = 1)
