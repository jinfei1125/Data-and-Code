

library(titanic)
titanic <- as_tibble(titanic_train) %>%
  mutate(Survived = factor(Survived))
# 1. Use 10-fold cross validation to build and evaluate a logistic regression 
# predicting `Survived` as a function of interacting 
# `Age` and `Sex`. To answer this, you will need to:
#     - Build the classifer on the training set(s) across folds
#     - Evaluate each classifer using the test set(s) across folds

survive_age_woman_x <- glm(Survived ~ Age * Sex, data = titanic,
                           family = binomial)
summary(survive_age_woman_x)

holdout_results <- function(splits, i) {
  mod <- glm(Survived ~ Age * Sex, data = analysis(splits))
  
  holdout <- assessment(splits)
  
  res <- augment(mod, newdata = holdout) %>%
    mse(truth = Survived, estimate = .fitted)

  res
}

titanic_cv10 <- vfold_cv(data = titanic, v = 10)
titanic_cv10 %>%
    names()




# 2. Calculate the cross-validation *error rate* 
# (not the accuracy rate) from your solution and report it. 

x_test_accuracy <- augment(train_model, 
                           newdata = testing(titanic_split)) %>% 
  as_tibble() %>%
  mutate(.prob = logit2prob(.fitted),
         .pred = factor(round(.prob)))

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