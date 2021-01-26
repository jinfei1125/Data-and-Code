
library(titanic)
titanic <- as_tibble(titanic_train) %>%
  mutate(Survived = factor(Survived))
# 1. Use 10-fold cross validation to build and evaluate a logistic regression 
# predicting `Survived` as a function of interacting 
# `Age` and `Sex`. To answer this, you will need to:
#     - Build the classifer on the training set(s) across folds
#     - Evaluate each classifer using the test set(s) across folds
set.seed(1234)
titanic_cv10 <- vfold_cv(data = titanic, v = 10)
titanic_cv10 %>%
  names()
titanic_cv10$splits[[1]]

holdout_results <- function(splits) {
  mod <- glm(Survived ~ Age * Sex, data = analysis(splits),
    family = binomial)
  holdout <- assessment(splits)
  res <- augment(mod, newdata = holdout) %>%
    # Following two lines are important
    as_tibble() %>%
    mutate(.prob = logit2prob(.fitted),
        .pred = round(.prob))
  res
}

titanic_cv10 <- titanic_cv10 %>%
  mutate(results = map(splits,holdout_results)) %>%
  unnest(results) %>%
  mutate(.pred = factor(.pred)) %>%
  group_by(id) %>%
  accuracy(truth = Survived, estimate = .pred)
titanic_cv10

# 2. Calculate the cross-validation *error rate* 
# (not the accuracy rate) from your solution and report it. 

1 - mean(titanic_auto_cv10$.estimate, na.rm = TRUE)

# calculate test accuracy rate
accuracy(x_test_accuracy, 
         truth = Survived, 
         estimate = .pred)

# 3. Is this similar to the error from using LOOCV earlier 
# in the session? Why or why not, do you think? 
# (offer just a couple thoughts on the patterns 
#   -- differences and similarities in error, 
#   computational speed, etc. 
#   -- from each approach to resampling 
#   in this classification setting).