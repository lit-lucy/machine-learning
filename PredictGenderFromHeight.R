library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# Outcome and predictors
y <- heights$sex
x <- heights$height

# training set - used to develop algorithm
# test set - pretend we don't know outcome
# createDataPartition from caret - randomly generate indices to split the dataset
set.seed(2007)
# times - how many random samples of indices, p - proportion of the data represented by indeces 
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

# Predict Male if heigh is within 2 SD from the avg male height
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
# Overall accuracy
mean(y_hat == y)

# Optimize the cutoff using only the training set
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

# Plot accuracy for cutoffs
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()

# Select cutoff with the best accuracy
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Test cutoff on the test set
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)








