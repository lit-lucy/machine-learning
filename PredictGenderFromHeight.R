library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# Outcome and predictors
y <- heights$sex
x <- heights$height

# training set - used to develop algorithm
# test set - pretend we don't know the outcome
# createDataPartition from caret - randomly generate indices to split the dataset
set.seed(2007)
# times - how many random samples of indices, p - proportion of the data represented by indices 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# First algorithm - randomly guessing the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
# Calculating overall accuracy
mean(y_hat == test_set$sex)

# Checking data averages
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# Second algorithm - predict Male if height is within 2 SD from the avg male height
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
# Overall accuracy
mean(y_hat == y)

# Data analysis that leads to conclusion of using harmonic average (F1-score) 
# which is harmonic average of precision and recall
# Tabulate predicted vs. real data
table(predicted = y_hat, actual = test_set$sex)
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

# Prevalence. The proportion of females in dataset is just 20%
prev <- mean(y == "Female")
prev

# Confusion Matrix metrics
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall["Accuracy"]
cm$byClass[c("Sensitivity", "Specificity", "Prevalence")]

# Thirds algorithm - same as the second but maximizing F_1 score instead of overall accuracy
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
# Plot F_1 measure against the cutoff
data.frame(cutoff, F_1) %>%
  ggplot(aes(cutoff, F_1)) +
  geom_point() +
  geom_line()

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)