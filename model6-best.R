# Load necessary libraries
library(caret)  # For cross-validation and model training
library(dplyr)  # For data manipulation
library(leaps)  # For all subsets regression
library(DMwR2)

set.seed(120401002)

# Step 1: Loading Data
players <- read.csv('players.csv')
games <- read.csv('games.csv')
test <- read.csv('test.csv')


# Step 2: Processing Player Data
# Convert work rate to numeric
players$attacking_work_rate <- as.numeric(factor(players$attacking_work_rate, 
                                                 levels = c("low", "medium", "high"), 
                                                 labels = c(1, 2, 3)))

players$defensive_work_rate <- as.numeric(factor(players$defensive_work_rate, 
                                                 levels = c("low", "medium", "high"), 
                                                 labels = c(1, 2, 3)))

games$home_player_1[is.na(games$home_player_1)] <- 0
games$away_player_1[is.na(games$away_player_1)] <- 0

player_summary <- players %>%
  group_by(player_id, year) %>%
  summarise(
    avg_overall_rating = mean(overall_rating, na.rm = TRUE),
    avg_potential = mean(potential, na.rm = TRUE),
    avg_crossing = mean(crossing, na.rm = TRUE),
    avg_attacking_work_rate = mean(attacking_work_rate, na.rm = TRUE),
    avg_defensive_work_rate = mean(defensive_work_rate, na.rm = TRUE)
  ) %>% arrange(player_id, year)

players_summary_test <- player_summary %>% group_by(player_id) %>% filter(year == max(year)) %>% ungroup()

# Step 3: Merging Player Data with Games
for (i in 1:11) {
  games <- games %>%
    left_join(player_summary, by = c(setNames("player_id", paste0("home_player_", i)), "year")) %>%
    rename_with(~ paste0(., "_home_player_", i), 
                c("avg_overall_rating", "avg_potential", "avg_crossing", "avg_attacking_work_rate", "avg_defensive_work_rate")) %>%
    
    left_join(player_summary, by = c(setNames("player_id", paste0("away_player_", i)), "year")) %>%
    rename_with(~ paste0(., "_away_player_", i), 
                c("avg_overall_rating", "avg_potential", "avg_crossing", "avg_attacking_work_rate", "avg_defensive_work_rate"))

  test <- test %>%
    left_join(players_summary_test, by = c(setNames("player_id", paste0("home_player_", i)))) %>%
    rename_with(~ paste0(., "_home_player_", i), 
                c("avg_overall_rating", "avg_potential", "avg_crossing", "avg_attacking_work_rate", "avg_defensive_work_rate")) %>%
    
    left_join(players_summary_test, by = c(setNames("player_id", paste0("away_player_", i)))) %>%
    rename_with(~ paste0(., "_away_player_", i), 
                c("avg_overall_rating", "avg_potential", "avg_crossing", "avg_attacking_work_rate", "avg_defensive_work_rate"))
}


# Step 4: Calculating Win/Loss and Point Difference
games <- games %>%
  filter(home_team_goal != away_team_goal) %>%
  mutate(
    point_diff = abs(home_team_goal - away_team_goal),  
    win_loss = ifelse(home_team_goal > away_team_goal, 1, 0)  
  )

# Step 5: Selecting Important Columns
home_columns <- grep("^avg_(potential|overall_rating|defensive_work_rate|attacking_work_rate|crossing)_home_player", colnames(games), value = TRUE)
away_columns <- grep("^avg_(potential|overall_rating|defensive_work_rate|attacking_work_rate|crossing)_away_player", colnames(games), value = TRUE)

# Combine home and away columns with 'year' and 'home_team_id'
selected_columns <- c("year", "home_team_id", "away_team_id", home_columns, away_columns,
                      "avg_overall_rating_home_player_1", "avg_overall_rating_away_player_1")

# Create filtered games and test dataframes
filtered_games <- games[, selected_columns]
filtered_test <- test[, selected_columns[-1]]  # Test doesn't have 'year' column

# Calculate mean values for each game
filtered_games$mean_potential_home <- rowMeans(filtered_games[, grep("avg_potential_home_player", colnames(filtered_games))], na.rm = TRUE)
filtered_games$mean_potential_away <- rowMeans(filtered_games[, grep("avg_potential_away_player", colnames(filtered_games))], na.rm = TRUE)

filtered_games$mean_overall_rating_home <- rowMeans(filtered_games[, grep("avg_overall_rating_home_player", colnames(filtered_games))], na.rm = TRUE)
filtered_games$mean_overall_rating_away <- rowMeans(filtered_games[, grep("avg_overall_rating_away_player", colnames(filtered_games))], na.rm = TRUE)

filtered_games$mean_defensive_work_rate_home <- rowMeans(filtered_games[, grep("avg_defensive_work_rate_home_player", colnames(filtered_games))], na.rm = TRUE)
filtered_games$mean_defensive_work_rate_away <- rowMeans(filtered_games[, grep("avg_defensive_work_rate_away_player", colnames(filtered_games))], na.rm = TRUE)

filtered_games$mean_attacking_work_rate_home <- rowMeans(filtered_games[, grep("avg_attacking_work_rate_home_player", colnames(filtered_games))], na.rm = TRUE)
filtered_games$mean_attacking_work_rate_away <- rowMeans(filtered_games[, grep("avg_attacking_work_rate_away_player", colnames(filtered_games))], na.rm = TRUE)


filtered_games$avg_crossing_home <- rowMeans(filtered_games[, grep("avg_crossing_home_player", colnames(filtered_test))], na.rm = TRUE)
filtered_games$avg_crossing_away <- rowMeans(filtered_games[, grep("avg_crossing_away_player", colnames(filtered_test))], na.rm = TRUE)

filtered_games[is.na(filtered_games)] <- 0

filtered_games <- filtered_games %>%  rowwise() %>%
  mutate(combined_work_rate_home_player_1 = 0.85*avg_defensive_work_rate_home_player_1 + 0.15*avg_attacking_work_rate_home_player_1, 
         combined_work_rate_home_player_2 = 0.70*avg_defensive_work_rate_home_player_2 + 0.30*avg_attacking_work_rate_home_player_2, 
         combined_work_rate_home_player_3 = 0.70*avg_defensive_work_rate_home_player_3 + 0.30*avg_attacking_work_rate_home_player_3, 
         combined_work_rate_home_player_4 = 0.70*avg_defensive_work_rate_home_player_4 + 0.30*avg_attacking_work_rate_home_player_4, 
         combined_work_rate_home_player_5 = 0.50*avg_defensive_work_rate_home_player_5 + 0.50*avg_attacking_work_rate_home_player_5, 
         combined_work_rate_home_player_6 = 0.50*avg_defensive_work_rate_home_player_6 + 0.50*avg_attacking_work_rate_home_player_6, 
         combined_work_rate_home_player_7 = 0.50*avg_defensive_work_rate_home_player_7 + 0.50*avg_attacking_work_rate_home_player_7, 
         combined_work_rate_home_player_8 = 0.50*avg_defensive_work_rate_home_player_8 + 0.50*avg_attacking_work_rate_home_player_8, 
         combined_work_rate_home_player_9 = 0.30*avg_defensive_work_rate_home_player_9 + 0.70*avg_attacking_work_rate_home_player_9, 
         combined_work_rate_home_player_10 = 0.30*avg_defensive_work_rate_home_player_10 + 0.70*avg_attacking_work_rate_home_player_10, 
         combined_work_rate_home_player_11 = 0.30*avg_defensive_work_rate_home_player_11 + 0.70*avg_attacking_work_rate_home_player_11,
         combined_work_rate_away_player_1 = 0.85*avg_defensive_work_rate_away_player_1 + 0.15*avg_attacking_work_rate_away_player_1, 
         combined_work_rate_away_player_2 = 0.70*avg_defensive_work_rate_away_player_2 + 0.30*avg_attacking_work_rate_away_player_2, 
         combined_work_rate_away_player_3 = 0.70*avg_defensive_work_rate_away_player_3 + 0.30*avg_attacking_work_rate_away_player_3, 
         combined_work_rate_away_player_4 = 0.70*avg_defensive_work_rate_away_player_4 + 0.30*avg_attacking_work_rate_away_player_4, 
         combined_work_rate_away_player_5 = 0.50*avg_defensive_work_rate_away_player_5 + 0.50*avg_attacking_work_rate_away_player_5, 
         combined_work_rate_away_player_6 = 0.50*avg_defensive_work_rate_away_player_6 + 0.50*avg_attacking_work_rate_away_player_6, 
         combined_work_rate_away_player_7 = 0.50*avg_defensive_work_rate_away_player_7 + 0.50*avg_attacking_work_rate_away_player_7, 
         combined_work_rate_away_player_8 = 0.50*avg_defensive_work_rate_away_player_8 + 0.50*avg_attacking_work_rate_away_player_8, 
         combined_work_rate_away_player_9 = 0.30*avg_defensive_work_rate_away_player_9 + 0.70*avg_attacking_work_rate_away_player_9, 
         combined_work_rate_away_player_10 = 0.30*avg_defensive_work_rate_away_player_10 + 0.70*avg_attacking_work_rate_away_player_10, 
         combined_work_rate_away_player_11 = 0.30*avg_defensive_work_rate_away_player_11 + 0.70*avg_attacking_work_rate_away_player_11,)

filtered_games <- filtered_games %>% rowwise() %>%
  mutate(avg_combined_work_rate_home = mean(c(combined_work_rate_home_player_1,combined_work_rate_home_player_2,
                                              combined_work_rate_home_player_3,combined_work_rate_home_player_4,
                                              combined_work_rate_home_player_5,combined_work_rate_home_player_6, 
                                              combined_work_rate_home_player_7,combined_work_rate_home_player_8,
                                              combined_work_rate_home_player_9,combined_work_rate_home_player_10,
                                              combined_work_rate_home_player_11), na.rm = TRUE), 
         avg_combined_work_rate_away = mean(c(combined_work_rate_away_player_1,combined_work_rate_away_player_2, 
                                              combined_work_rate_away_player_3,combined_work_rate_away_player_4, 
                                              combined_work_rate_away_player_5,combined_work_rate_away_player_6, 
                                              combined_work_rate_away_player_7,combined_work_rate_away_player_8, 
                                              combined_work_rate_away_player_9,combined_work_rate_away_player_10,
                                              combined_work_rate_away_player_1), na.rm = TRUE))


# Apply same calculations to the test data
filtered_test$mean_potential_home <- rowMeans(filtered_test[, grep("avg_potential_home_player", colnames(filtered_test))], na.rm = TRUE)
filtered_test$mean_potential_away <- rowMeans(filtered_test[, grep("avg_potential_away_player", colnames(filtered_test))], na.rm = TRUE)

filtered_test$mean_overall_rating_home <- rowMeans(filtered_test[, grep("avg_overall_rating_home_player", colnames(filtered_test))], na.rm = TRUE)
filtered_test$mean_overall_rating_away <- rowMeans(filtered_test[, grep("avg_overall_rating_away_player", colnames(filtered_test))], na.rm = TRUE)

filtered_test$mean_defensive_work_rate_home <- rowMeans(filtered_test[, grep("avg_defensive_work_rate_home_player", colnames(filtered_test))], na.rm = TRUE)
filtered_test$mean_defensive_work_rate_away <- rowMeans(filtered_test[, grep("avg_defensive_work_rate_away_player", colnames(filtered_test))], na.rm = TRUE)

filtered_test$mean_attacking_work_rate_home <- rowMeans(filtered_test[, grep("avg_attacking_work_rate_home_player", colnames(filtered_test))], na.rm = TRUE)
filtered_test$mean_attacking_work_rate_away <- rowMeans(filtered_test[, grep("avg_attacking_work_rate_away_player", colnames(filtered_test))], na.rm = TRUE)


filtered_test$avg_crossing_home <- rowMeans(filtered_test[, grep("avg_crossing_home_player", colnames(filtered_test))], na.rm = TRUE)
filtered_test$avg_crossing_away <- rowMeans(filtered_test[, grep("avg_crossing_away_player", colnames(filtered_test))], na.rm = TRUE)

filtered_test <- filtered_test %>% rowwise() %>%
  mutate(combined_work_rate_home_player_1 = 0.85*avg_defensive_work_rate_home_player_1 + 0.15*avg_attacking_work_rate_home_player_1, 
         combined_work_rate_home_player_2 = 0.70*avg_defensive_work_rate_home_player_2 + 0.30*avg_attacking_work_rate_home_player_2, 
         combined_work_rate_home_player_3 = 0.70*avg_defensive_work_rate_home_player_3 + 0.30*avg_attacking_work_rate_home_player_3, 
         combined_work_rate_home_player_4 = 0.70*avg_defensive_work_rate_home_player_4 + 0.30*avg_attacking_work_rate_home_player_4, 
         combined_work_rate_home_player_5 = 0.50*avg_defensive_work_rate_home_player_5 + 0.50*avg_attacking_work_rate_home_player_5, 
         combined_work_rate_home_player_6 = 0.50*avg_defensive_work_rate_home_player_6 + 0.50*avg_attacking_work_rate_home_player_6, 
         combined_work_rate_home_player_7 = 0.50*avg_defensive_work_rate_home_player_7 + 0.50*avg_attacking_work_rate_home_player_7, 
         combined_work_rate_home_player_8 = 0.50*avg_defensive_work_rate_home_player_8 + 0.50*avg_attacking_work_rate_home_player_8, 
         combined_work_rate_home_player_9 = 0.30*avg_defensive_work_rate_home_player_9 + 0.70*avg_attacking_work_rate_home_player_9, 
         combined_work_rate_home_player_10 = 0.30*avg_defensive_work_rate_home_player_10 + 0.70*avg_attacking_work_rate_home_player_10, 
         combined_work_rate_home_player_11 = 0.30*avg_defensive_work_rate_home_player_11 + 0.70*avg_attacking_work_rate_home_player_11,
         combined_work_rate_away_player_1 = 0.85*avg_defensive_work_rate_away_player_1 + 0.15*avg_attacking_work_rate_away_player_1, 
         combined_work_rate_away_player_2 = 0.70*avg_defensive_work_rate_away_player_2 + 0.30*avg_attacking_work_rate_away_player_2, 
         combined_work_rate_away_player_3 = 0.70*avg_defensive_work_rate_away_player_3 + 0.30*avg_attacking_work_rate_away_player_3, 
         combined_work_rate_away_player_4 = 0.70*avg_defensive_work_rate_away_player_4 + 0.30*avg_attacking_work_rate_away_player_4, 
         combined_work_rate_away_player_5 = 0.50*avg_defensive_work_rate_away_player_5 + 0.50*avg_attacking_work_rate_away_player_5, 
         combined_work_rate_away_player_6 = 0.50*avg_defensive_work_rate_away_player_6 + 0.50*avg_attacking_work_rate_away_player_6, 
         combined_work_rate_away_player_7 = 0.50*avg_defensive_work_rate_away_player_7 + 0.50*avg_attacking_work_rate_away_player_7, 
         combined_work_rate_away_player_8 = 0.50*avg_defensive_work_rate_away_player_8 + 0.50*avg_attacking_work_rate_away_player_8, 
         combined_work_rate_away_player_9 = 0.30*avg_defensive_work_rate_away_player_9 + 0.70*avg_attacking_work_rate_away_player_9, 
         combined_work_rate_away_player_10 = 0.30*avg_defensive_work_rate_away_player_10 + 0.70*avg_attacking_work_rate_away_player_10, 
         combined_work_rate_away_player_11 = 0.30*avg_defensive_work_rate_away_player_11 + 0.70*avg_attacking_work_rate_away_player_11,)

names(filtered_test)


filtered_test <- filtered_test %>% rowwise() %>%
  mutate(avg_combined_work_rate_home = mean(c(combined_work_rate_home_player_1,combined_work_rate_home_player_2,
                                              combined_work_rate_home_player_3,combined_work_rate_home_player_4,
                                              combined_work_rate_home_player_5,combined_work_rate_home_player_6, 
                                              combined_work_rate_home_player_7,combined_work_rate_home_player_8,
                                              combined_work_rate_home_player_9,combined_work_rate_home_player_10,
                                              combined_work_rate_home_player_11), na.rm = TRUE), 
         avg_combined_work_rate_away = mean(c(combined_work_rate_away_player_1,combined_work_rate_away_player_2, 
                                              combined_work_rate_away_player_3,combined_work_rate_away_player_4, 
                                              combined_work_rate_away_player_5,combined_work_rate_away_player_6, 
                                              combined_work_rate_away_player_7,combined_work_rate_away_player_8, 
                                              combined_work_rate_away_player_9,combined_work_rate_away_player_10,
                                              combined_work_rate_away_player_1), na.rm = TRUE))

for (col in 1:ncol(filtered_test)) {
  filtered_games[is.na(filtered_games[,col]),col] <- mean(filtered_games[,col], na.rm = T)
}

# Define predictors
predictors <- c("home_team_id","away_team_id","mean_potential_home", "mean_potential_away", "mean_overall_rating_home",
                "mean_overall_rating_away", "mean_defensive_work_rate_home",
                "mean_defensive_work_rate_away", "mean_attacking_work_rate_home",
                "mean_attacking_work_rate_away", "avg_overall_rating_home_player_1.1", "avg_overall_rating_away_player_1.1",
                "avg_combined_work_rate_home", "avg_combined_work_rate_away")

# Split essential data into training and testing datasets
essential_train <- essential[, c(predictors, "win_loss", "point_diff")]
essential_test <- filtered_test[, predictors]
essential_test$avg_overall_rating_home_player_1.1[is.na(essential_test$avg_overall_rating_home_player_1.1)] <- 0 
essential_test$avg_overall_rating_away_player_1.1[is.na(essential_test$avg_overall_rating_away_player_1.1)] <- 0 
essential_test$home_team_id[is.na(essential_test$home_team_id)] <- 0 
essential_test$away_team_id[is.na(essential_test$away_team_id)] <- 0 


essential_test <- essential_test %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))
sum(is.na(essential_test))



# Model to Predict win_loss (classification)
essential_train$win_loss <- as.factor(essential_train$win_loss)  # Convert to factor

# Impute missing values using median or another method
preProcValues <- preProcess(essential_train[, c(predictors, "win_loss")], method = "medianImpute")
essential_train_imputed <- predict(preProcValues, essential_train[, c(predictors, "win_loss")])

essential_test <- knnImputation(essential_test, k = 7) 

# Define control for repeated cross-validation
train_control <- trainControl(
  method = "repeatedcv",    # Repeated cross-validation
  number = 15,              # 10-fold CV
  repeats = 5,              # Repeat 5 times for stability
  savePredictions = TRUE    # Save predictions for later analysis
)
# Train the win_loss model using logistic regression
win_loss_model <- train(
  form = win_loss ~ .,  # Model formula (response ~ predictors)
  data = essential_train_imputed[, c(predictors, "win_loss")],  # Subset of data
  method = "glm",       # Generalized linear model (logistic regression)
  family = binomial(link=logit),    # Logistic regression for binary classification
  trControl = train_control
)

# Predict win_loss probabilities for the test data
win_loss_probabilities <- predict(win_loss_model, essential_test, type = "prob")[, 2]  # Probability of win

# Impute missing values using median 
preProcValues <- preProcess(essential_train[, c(predictors, "point_diff")], method = "medianImpute")
essential_train_imputed <- predict(preProcValues, essential_train[, c(predictors, "point_diff")])

# 2. Model to Predict point_diff (Poisson regression)
point_diff_model <- train(
  form = point_diff ~ .,  # Model formula (response ~ predictors)
  data = essential_train_imputed[, c(predictors, "point_diff")],  # Subset with predictors and point_diff
  method = "glm",         # Generalized linear model
  family = poisson(link = "log"),  # Poisson regression for integer outputs
  trControl = train_control# Cross-validation settings
)

sum(is.na(essential_test))

# Predict point_diff for the test data (integer values)
point_diff_predictions <- predict(point_diff_model, essential_test)

# Combine predictions into a result dataframe
test_predictions <- data.frame(
  win_loss_probability = win_loss_probabilities,  # Probability of winning (win = 1)
  point_diff_prediction = point_diff_predictions  # Predicted point difference (integer)
)

# Print the test predictions
head(test_predictions)
dim(test_predictions)

colnames(test_predictions) <- c("winner", "winby")
write.csv(test_predictions, "test_predictions6.csv", row.names = FALSE)

