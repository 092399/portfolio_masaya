# Author : Masaya Inoue
# Publish Date : 02/17/2024
# This is for getting some initial insights for business challenge II with Team 3 but not for Alessio

# Working Directory 
setwd("~/Desktop/HULT MBANDD/Spring_Term/Business Challenge II")

# Libraries
library(ggplot2)
library(ggthemes)
library(vtreat)
library(MLmetrics)
library(stringr)
library(rpart)
library(caret)
library(plyr)
library(dplyr)
library(data.table)
library(visdat)
library(naniar)
library(corrplot)

# train.csv - Personal records for about two-thirds (~8700) of the passengers, to be used as training data.
# PassengerId - A unique Id for each passenger. Each Id takes the form gggg_pp where gggg indicates a group the passenger is travelling with and pp is their number within the group. People in a group are often family members, but not always.
# HomePlanet - The planet the passenger departed from, typically their planet of permanent residence.
# CryoSleep - Indicates whether the passenger elected to be put into suspended animation for the duration of the voyage. Passengers in cryosleep are confined to their cabins.
# Cabin - The cabin number where the passenger is staying. Takes the form deck/num/side, where side can be either P for Port or S for Starboard.
# Destination - The planet the passenger will be debarking to.
# Age - The age of the passenger.
# VIP - Whether the passenger has paid for special VIP service during the voyage.
# RoomService, FoodCourt, ShoppingMall, Spa, VRDeck - Amount the passenger has billed at each of the Spaceship Titanic's many luxury amenities.
# Name - The first and last names of the passenger.
# Transported - Whether the passenger was transported to another dimension. This is the target, the column you are trying to predict.



# Data Loading 
train_df <- read.csv('data/train.csv')
test_df <- read.csv('data/test.csv')

head(train_df)



# Define function to handle outliers for a numeric variable
handle_outliers <- function(df, variable, method = "zscore", threshold = 3) {
  if (method == "zscore") {
    # Calculate Z-scores
    z_scores <- scale(df[[variable]])
    
    # Remove outliers based on Z-score
    df <- df[abs(z_scores) < threshold, ]
  } else if (method == "winsorize") {
    # Calculate percentiles
    lower_limit <- quantile(df[[variable]], 0.05)
    upper_limit <- quantile(df[[variable]], 0.95)
    
    # Winsorize outliers
    df[[variable]][df[[variable]] < lower_limit] <- lower_limit
    df[[variable]][df[[variable]] > upper_limit] <- upper_limit
  }
  
  return(df)
}

# Define function to handle outliers for a numeric variable
handle_outliers <- function(df, variable, method = "zscore", threshold = 3) {
  if (method == "zscore") {
    # Calculate Z-scores
    z_scores <- scale(df[[variable]])
    
    # Remove outliers based on Z-score
    df <- df[abs(z_scores) < threshold, ]
  } else if (method == "winsorize") {
    # Calculate percentiles
    lower_limit <- quantile(df[[variable]], 0.05)
    upper_limit <- quantile(df[[variable]], 0.95)
    
    # Winsorize outliers
    df[[variable]][df[[variable]] < lower_limit] <- lower_limit
    df[[variable]][df[[variable]] > upper_limit] <- upper_limit
  }
  
  return(df)
}


# Define function to create numeric labels for variables
create_numeric_labels <- function(df, variable) {
  # Create numeric labels based on variable values
  labels <- cut(df[[variable]], breaks = 10, labels = FALSE, include.lowest = TRUE)
  
  # Add labels as new column in dataframe
  #df[[paste0(variable, "_label")]] <- labels
  
  # Add labels to existing column in dataframe
  df[[variable]] <- labels
  
  return(df)
}


# Define a function to fill Null HomePlanet, Destination, Cabindeck, Cabinnum for each group
fill_group_nulls <- function(df) {
  # Extract rows with Null values
  null_rows <- df %>% filter(is.na(HomePlanet) | is.na(Destination) | is.na(Cabin))
  
  # Exit if there are no Null rows
  if (nrow(null_rows) == 0) {
    return(df)
  }
  
  
  # Process each group
  for (group_num in unique(null_rows$Groupnum)) {
    # Extract rows within the group
    group_rows <- df %>% filter(Groupnum == group_num)
    # Find the first row within the group that does not have Null values for HomePlanet
    filled_homeplanet_rows <- group_rows %>% filter(!is.na(HomePlanet))
    if (nrow(filled_homeplanet_rows) > 0) {
      filled_homeplanet_row <- filled_homeplanet_rows %>% slice(1)
      df[df$Groupnum == group_num & is.na(df$HomePlanet), "HomePlanet"] <- filled_homeplanet_row$HomePlanet
    }
    # Find the first row within the group that does not have Null values for Destination
    filled_destination_rows <- group_rows %>% filter(!is.na(Destination))
    if (nrow(filled_destination_rows) > 0) {
      filled_destination_row <- filled_destination_rows %>% slice(1)
      df[df$Groupnum == group_num & is.na(df$Destination), "Destination"] <- filled_destination_row$Destination
    }
    # Find the first row within the group that does not have Null values for Cabindeck
    filled_cabin_rows <- group_rows %>% filter(!is.na(Cabindeck))
    if (nrow(filled_cabin_rows) > 0) {
      filled_cabin_row <- filled_cabin_rows %>% slice(1)
      df[df$Groupnum == group_num & is.na(df$Cabindeck), "Cabindeck"] <- filled_cabin_row$Cabindeck
    }
    # Find the first row within the group that does not have Null values for Cabinnumber
    filled_cabin_rows <- group_rows %>% filter(!is.na(Cabinnum))
    if (nrow(filled_cabin_rows) > 0) {
      filled_cabin_row <- filled_cabin_rows %>% slice(1)
      df[df$Groupnum == group_num & is.na(df$Cabinnum), "Cabinnum"] <- filled_cabin_row$Cabinnum
    }
    # Find the first row within the group that does not have Null values for Cabinside
    filled_cabin_rows <- group_rows %>% filter(!is.na(Cabinside))
    if (nrow(filled_cabin_rows) > 0) {
      filled_cabin_row <- filled_cabin_rows %>% slice(1)
      df[df$Groupnum == group_num & is.na(df$Cabinside), "Cabinside"] <- filled_cabin_row$Cabinside
    }
  }
  return(df)
}

convert_to_binary <- function(data, variable) {
  data[[variable]] <- ifelse(data[[variable]] == "True", 1, 0)
  return(data)
}

split_cabin <- function(data, variable) {
  data$Cabindeck <- sapply(strsplit(data[[variable]], "/"), "[", 1)
  data$Cabinnum <- sapply(strsplit(data[[variable]], "/"), "[", 2)
  data$Cabinside <- sapply(strsplit(data[[variable]], "/"), "[", 3)
  data$Cabinnum <- as.numeric(data$Cabinnum)
  
  return(data)
}

split_passenger_id <- function(df, id_column) {
  split_ids <- strsplit(df[[id_column]], "_")
  
  df$Groupnum <- sapply(split_ids, `[`, 1)
  df$Groupnum <- as.numeric(df$Groupnum)
  
  df$Groupsize <- sapply(split_ids, `[`, 2)
  df$Groupsize <- as.numeric(df$Groupsize)
  
  return(df)
}

# Define function for Replacement NA with mode for Categorical variables
mode_imputation <- function(df, categorical_columns) {
  for (col in categorical_columns) {
    mode_val <- names(sort(table(df[[col]], exclude = NULL), decreasing = TRUE)[1])
    df[[col]][is.na(df[[col]])] <- mode_val
  }
  return(df)
}

# Define function for Replacement NA with mode for Numerical variables
median_imputation <- function(df, numerical_columns) {
  for (col in numerical_columns) {
    median_val <- median(df[[col]], na.rm = TRUE)
    df[[col]][is.na(df[[col]])] <- median_val
  }
  return(df)
}

# Define function for Normalization
min_max <- function(df, numerical_columns) {
  normalized_df <- df
  
  # Create a new column
  #for (col in numerical_columns) {
  #  normalized_df[[paste0(col, "_norm")]] <- (df[[col]] - min(df[[col]])) / (max(df[[col]]) - min(df[[col]]))
  #}
  
  # Overwrite to existing columns
  for (col in numerical_columns) {
    normalized_df[[col]] <- (df[[col]] - min(df[[col]])) / (max(df[[col]]) - min(df[[col]]))
  }
  
  return(normalized_df)
}

fill_missing_proportional <- function(data, column) {
  non_missing_values <- data[[column]][!is.na(data[[column]])]
  non_missing_count <- length(non_missing_values)
  proportions <- table(non_missing_values) / non_missing_count
  data[[column]][is.na(data[[column]])] <- sample(names(proportions), size = sum(is.na(data[[column]])), replace = TRUE, prob = proportions)
  
  return(data)
}



##### CLEANING PROCESS ####################################################################################################

##### Train Dataset #######################################################################################################
# Check 
train_df[train_df == ""] <- NA
colSums(is.na(train_df))
str(train_df)

#Check NA with visuals and summary
vis_miss(train_df)
train_df %>% miss_summary()

# Summary Statistics
summary(train_df[c("Age", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")])
boxplot(train_df$Age)
boxplot(train_df[c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")])

# Convert booleans to binary
train_df <- convert_to_binary(train_df, "Transported")
train_df <- convert_to_binary(train_df, "VIP")
train_df$VIP[is.na(train_df$VIP)] <- 0


# Call the function to split the 'Cabin' column
train_df <- split_cabin(train_df, "Cabin")

# Split Passenger ID and storing in train_df
train_df <- split_passenger_id(train_df, 'PassengerId')



#####  Handling Missing Values #####
# Assumption : If the passenger spent money, they are not in cryosleep.
train_df$Total_spending <- rowSums(train_df[, c("RoomService","FoodCourt", "ShoppingMall", "Spa", "VRDeck")], na.rm = TRUE)
train_df$CryoSleep <- ifelse(is.na(train_df$CryoSleep) & train_df$Total_spending > 0, "False", train_df$CryoSleep)
train_df <- convert_to_binary(train_df, "CryoSleep")
#train_df <- median_imputation(train_df, "CryoSleep")
#train_df <- fill_missing_proportional(train_df, "CryoSleep")
train_df$CryoSleep <- as.numeric(train_df$CryoSleep)

# Assumption : Each group is from same Home Planet & Destination, Assigned in same cabin deck and side
# Split the dataset (Single and Group)
train_df_group <- train_df %>%
  group_by(Groupnum) %>%
  filter(n() > 1) %>%
  ungroup()

train_df_single<- train_df %>%
  group_by(Groupnum) %>%
  filter(n() <= 1) %>%
  ungroup()


colSums(is.na(train_df))
train_df_group_executed <- fill_group_nulls(train_df_group)
train_df <- rbind(train_df_single, train_df_group_executed)
colSums(is.na(train_df))


# For categorical columns, call "mode_imputation" to replace NA with mode for each variables
categorical_columns <- c("HomePlanet", "Destination", "Cabindeck", "Cabinside")
train_df <- mode_imputation(train_df, categorical_columns)

# For numerical columns, call "median_imputation" to replace NA with mode for each variables
numerical_columns <- c( "Age", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Total_spending")
train_df <- median_imputation(train_df, numerical_columns)

# For numerical columns, call "handle_outliers" to deal with outliers
numerical_columns <- c( "Age")
for (numeric_col in numerical_columns) {
  train_df <- handle_outliers(train_df, numeric_col, method = "winsorize")
}

train_df_cryo <- train_df %>% filter(is.na(CryoSleep) | CryoSleep != 0)
train_df_uncryo <- train_df %>% filter(CryoSleep == 0)
spending <- c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Total_spending")
for (spending_col in spending) { 
  train_df_uncryo <- handle_outliers(train_df_uncryo, spending_col, method = "winsorize")}
train_df <- rbind(train_df_cryo, train_df_uncryo)


numerical_columns <- c("Groupnum", "Age", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Total_spending")

# Apply create_labels function to each variable
#variables <- c("Age_norm", "RoomService_norm", "FoodCourt_norm", "ShoppingMall_norm", "Spa_norm", "VRDeck_norm", "Total_spending_norm")
for (variable in numerical_columns) {
  train_df <- create_numeric_labels(train_df, variable)
}

# Call function "Normalization" to normalize business unit
#for (numeric_col in numerical_columns) {
#  train_df <- min_max(train_df, numeric_col)
#}

# Convert categorical variables into dummy variables
dummy_variables <- model.matrix(~ HomePlanet + Destination + Cabindeck + Cabinside - 1, data = train_df)
dummy_df <- as.data.frame(dummy_variables)
dummy_df <- dummy_df %>% rename(`PSO`= `DestinationPSO J318.5-22`, `Trappist` = `DestinationTRAPPIST-1e`)
train_df <- cbind(train_df, dummy_df)


# Remove the original variables not using for model
train_df <- subset(train_df, select = -c(PassengerId, HomePlanet, Cabin, Cabindeck, Cabinside, Destination, Name, Cabinnum))
train_df <- subset(train_df, select = -c(HomePlanetMars))

# Displaying the structure of the updated dataframe
str(train_df)
colSums(is.na(train_df))

# 欠損値を予測するためのデータセットを作成する（欠損値のない行）
train_data <- train_df[complete.cases(train_df), ]

# 欠損値を含む列を目的変数として選択
target_col <- "CryoSleep"

# 欠損値を予測するためのモデルを構築（例としてランダムフォレストを使用）
library(randomForest)
model <- randomForest(CryoSleep ~ .- Age - VIP - Groupnum - Groupsize - CabindeckT, data = train_data)

# 欠損値を含む行のデータを準備
missing_data <- train_df[!complete.cases(train_df), ]

# 欠損値を予測
predicted_values <- predict(model, missing_data)

# 欠損値を予測した値で欠損値を埋める
train_df$CryoSleep[!complete.cases(train_df)] <- predicted_values

##### Test Dataset #######################################################################################################
# Check 
test_df[test_df == ""] <- NA
colSums(is.na(test_df))
str(test_df)

#Check NA with visuals and summary
vis_miss(test_df)
test_df %>% miss_summary()

# Summary Statistics
summary(test_df[c("Age", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")])
boxplot(test_df$Age)
boxplot(test_df[c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")])

# Convert booleans to binary
test_df <- convert_to_binary(test_df, "VIP")
test_df$VIP[is.na(test_df$VIP)] <- 0


# Call the function to split the 'Cabin' column
test_df <- split_cabin(test_df, "Cabin")

# Split Passenger ID and storing in test_df
test_df <- split_passenger_id(test_df, 'PassengerId')


#####  Handling Missing Values #####
# Assumption : If the passenger spent money, they are not in cryosleep.
test_df$Total_spending <- rowSums(test_df[, c("RoomService","FoodCourt", "ShoppingMall", "Spa", "VRDeck")], na.rm = TRUE)
test_df$CryoSleep <- ifelse(is.na(test_df$CryoSleep) & test_df$Total_spending > 0, "False", test_df$CryoSleep)
test_df <- convert_to_binary(test_df, "CryoSleep")
#test_df <- median_imputation(test_df, "CryoSleep")
#test_df <- fill_missing_proportional(test_df, "CryoSleep")
test_df$CryoSleep <- as.numeric(test_df$CryoSleep)

# Assumption : Each group is from same Home Planet & Destination, Assigned in same cabin deck and side
# Split the dataset (Single and Group)
test_df_group <- test_df %>%
  group_by(Groupnum) %>%
  filter(n() > 1) %>%
  ungroup()

test_df_single<- test_df %>%
  group_by(Groupnum) %>%
  filter(n() <= 1) %>%
  ungroup()


colSums(is.na(test_df))
test_df_group_executed <- fill_group_nulls(test_df_group)
test_df <- rbind(test_df_single, test_df_group_executed)
colSums(is.na(test_df))


# For categorical columns, call "mode_imputation" to replace NA with mode for each variables
categorical_columns <- c("HomePlanet", "Destination", "Cabindeck", "Cabinside")
test_df <- mode_imputation(test_df, categorical_columns)

# For numerical columns, call "median_imputation" to replace NA with mode for each variables
numerical_columns <- c( "Age", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Total_spending")
test_df <- median_imputation(test_df, numerical_columns)


# For numerical columns, call "handle_outliers" to deal with outliers
numerical_columns <- c( "Age")
for (numeric_col in numerical_columns) {
  test_df <- handle_outliers(test_df, numeric_col, method = "winsorize")
}

test_df_cryo <- test_df %>% filter(is.na(CryoSleep) | CryoSleep != 0)
test_df_uncryo <- test_df %>% filter(CryoSleep == 0)
spending <- c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Total_spending")
for (spending_col in spending) { 
  test_df_uncryo <- handle_outliers(test_df_uncryo, spending_col, method = "winsorize")}
test_df <- rbind(test_df_cryo, test_df_uncryo)


numerical_columns <- c("Groupnum", "Age", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Total_spending")
# Apply create_labels function to each variable
# variables <- c("Age_norm", "RoomService_norm", "FoodCourt_norm", "ShoppingMall_norm", "Spa_norm", "VRDeck_norm", "Total_spending_norm")
for (variable in numerical_columns) {
  test_df <- create_numeric_labels(test_df, variable)
}

# Call function "Normalization" to normalize business unit

#for (numeric_col in numerical_columns) {
#  test_df <- min_max(test_df, numeric_col)
#}

# Convert categorical variables into dummy variables
dummy_variables <- model.matrix(~ HomePlanet + Destination + Cabindeck + Cabinside - 1, data = test_df)
dummy_df <- as.data.frame(dummy_variables)
dummy_df <- dummy_df %>% rename(`PSO`= `DestinationPSO J318.5-22`, `Trappist` = `DestinationTRAPPIST-1e`)
test_df <- cbind(test_df, dummy_df)


# Remove the original variables not using for model
test_df <- subset(test_df, select = -c(HomePlanet, Cabin, Cabindeck, Cabinside, Destination, Name, Cabinnum))
test_df <- subset(test_df, select = -c(HomePlanetMars))

# Displaying the structure of the updated dataframe
str(test_df)
colSums(is.na(test_df))

# Create a dataset for predicting missing values (rows without missing values)
test_data <- test_df[complete.cases(test_df), ]

# Select the column with missing values as the target variable
target_col <- "CryoSleep"

# Build a model to predict missing values (using random forest as an example)
library(randomForest)
model <- randomForest(CryoSleep ~ . - Age - VIP - Groupnum - Groupsize - CabindeckT  , data = test_data)

# Prepare the data for rows containing missing values
missing_data <- test_df[!complete.cases(test_df), ]

# Predict the missing values
predicted_values <- predict(model, missing_data)

# Fill in the missing values with the predicted values
test_df$CryoSleep[!complete.cases(test_df)] <- predicted_values



##### Correlation Matrix ##############################################################################

correlation_matrix <- cor(train_df)
corrplot(correlation_matrix, method = "circle")
str(train_df)

correlation_matrix <- cor(test_df[, -which(names(test_df) == "PassengerId")])　
corrplot(correlation_matrix, method = "circle")
str(test_df)
summary(test_df)
##########################################################################################
# SAMPLE

# Set seeds
set.seed(1234)

# Using the training data, create a validation set.(Ratio 8:2)
train      <- round(nrow(train_df) %*% .6)
validation <- round(nrow(train_df) %*% .4)

trainIdx <- sample(1:nrow(train_df), train)
remaining_rows <- setdiff(1:nrow(train_df), trainIdx)
validationIdx <-sample(remaining_rows, validation)

trainSet      <- train_df[trainIdx, ]
validationSet <- train_df[validationIdx, ]


# Arrange 
####################################################################################################
#  Model
###################################################################################################
# Select a model CryoSleep - Age - VIP - RoomService - FoodCourt - ShoppingMall - Spa - VRDeck - Transported - Groupnum - Groupsize - Total_spending - HomePlanetEarth - HomePlanetEuropa - DestinationPSO J318.5-22 - DestinationTRAPPIST-1e - CabindeckB - CabindeckC - CabindeckD - CabindeckE - CabindeckF - CabindeckG - CabindeckT - CabinsideS



# Logistic Regression Model with Normalized
model <- glm(Transported ~ . , data = train_df, family = "binomial")

# Predictions
predictions <- predict(model, newdata = train_df, type = "response")
predictions_glm <- ifelse(predictions >= 0.5, 1, 0)

# Evaluation
accuracy <- mean((predictions >= 0.5 & train_df$Transported == 1) | (predictions < 0.5 & train_df$Transported == 0))
print(accuracy)

# Model Summary
summary(model)



# Parsimonious regression 
# First get the variable and p-values
pVals <- data.frame(varNames = names(na.omit(coef(model))),
                    pValues = summary(model)$coefficients[,4])

# Determine which variable names to keep 
keeps <- subset(pVals$varNames, pVals$pValues<0.1)

# Remove unwanted columns
train_parsimony <- train_df[,names(train_df) %in% keeps]

# Append the y-variable
train_parsimony$Transported <- train_df$Transported

# Refit a model
model2 <- glm(Transported ~ ., data =train_parsimony,  family = "binomial")
summary(model2)

# Predictions
predictions_glm2 <- predict(model2, newdata = train_df, type = "response")

# Evaluation
accuracy <- mean((predictions >= 0.5 & train_df$Transported == 1) | (predictions < 0.5 & train_df$Transported == 0))
print(accuracy)



predictions <- predict(model, newdata = test_df, type = "response")
predictions_glm <- ifelse(predictions >= 0.5, 1, 0)
predictions_classes <- ifelse(predictions >= 0.5, 'True', 'False')


########  Decision Tree ################################################################
library(rpart)
tree_model <- rpart(Transported ~ ., data = train_df, method = "class", cp = 0.001)
predictions <- predict(tree_model, newdata = train_df, type = "class",)
accuracy <- mean(predictions == train_df$Transported)
print(accuracy)
printcp(tree_model)

tree_model <- rpart(Transported ~ ., data = train_parsimony, method = "class", cp = 0.001)
predictions <- predict(tree_model, newdata = train_df, type = "class",)
accuracy <- mean(predictions == train_df$Transported)
print(accuracy)
printcp(tree_model)

predictions <- predict(tree_model, newdata = test_df, type = "class")
predictions_tree <- predictions
predictions_tree <- as.numeric(predictions_tree) - 1
predictions_classes <- ifelse(predictions == "1", 'True', 'False')


###### Random Forest ###############################################################################
#install.packages(("randomForest"))
library(randomForest)
rf_model <- randomForest(Transported ~ .- Total_spending, data = trainSet, ntree = 100, mtry = 3, nodesize = 40)
predictions <- predict(rf_model, newdata = trainSet, type = "class")
predictions <- ifelse(predictions >= 0.5, 1, 0)
accuracy <- mean(predictions == trainSet$Transported)
print(paste("Accuracy on training data:", accuracy))
predictions_validation <- predict(rf_model, newdata = validationSet, type = "class")
predictions_validation <- ifelse(predictions_validation >= 0.5, 1, 0)
accuracy_validation <- mean(predictions_validation == validationSet$Transported)
print(paste("Accuracy on validation data:", accuracy_validation))


rf_model <- randomForest(Transported ~  . , data = train_df, ntree = 200, mtry = 10, nodesize = 40)
predictions <- predict(rf_model, newdata = train_df, type = "class")
predictions <- ifelse(predictions >= 0.5, 1, 0)
accuracy <- mean(predictions == train_df$Transported)
print(paste("Accuracy on train_df:", accuracy))

predictions <- predict(rf_model, newdata = test_df, type = "class")
predictions <- ifelse(predictions >= 0.5, 1, 0)
predictions_rf <- predictions
predictions_classes <- ifelse(predictions == 1, 'True', 'False')

sampleSubmission <- data.frame(PassengerId = test_df$PassengerId, Transported = predictions_classes)
colSums(is.na(sampleSubmission))

write.csv(sampleSubmission, "~/Desktop/HULT MBANDD/Spring_Term/Business Challenge II/Submission_final.csv", row.names = FALSE)


######## Gradient Boosting Classifier ###############################################################
#install.packages("gbm")
library(gbm)
gbm_model <- gbm(Transported ~ . -Total_spending, data = train_parsimony, distribution = "bernoulli", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01)

predictions <- predict(gbm_model, newdata = train_df, n.trees = 1000, type = "response")
predictions_classes <- ifelse(predictions >= 0.5, 1, 0)
accuracy <- mean(predictions_classes == train_df$Transported)
print(accuracy)


###########################################################################################################

predictions_glm
predictions_tree
predictions_rf

final_predictions <- ifelse((as.numeric(predictions_glm) + as.numeric(predictions_tree) + as.numeric(predictions_rf)) >= 2, 1, 0)

print(final_predictions)
final_predictions <- ifelse(final_predictions == 1, "True", "False")
sampleSubmission <- data.frame(PassengerId = test_df$PassengerId, Transported = final_predictions)
colSums(is.na(sampleSubmission))



############ Submission #########################################

sampleSubmission <- data.frame(PassengerId = test_df$PassengerId, Transported = predictions_classes)
colSums(is.na(sampleSubmission))


write.csv(sampleSubmission, "~/Desktop/HULT MBANDD/Spring_Term/Business Challenge II/Submission.csv", row.names = FALSE)





