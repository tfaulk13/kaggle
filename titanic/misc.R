glimpse(train)
head(train)
summary(train)



## Cleaning data --------------------------------------------------------------
train$male <- as.numeric(train$Sex == "male")
train$female <- as.numeric(train$Sex == "female")

train$lower_class <- as.numeric(train$Pclass == 1)
train$middle_class <- as.numeric(train$Pclass == 2)
train$upper_class <- as.numeric(train$Pclass == 3)

train$minor <- as.numeric(train$Age <= 18.00)
train$senior <- as.numeric(train$Age >= 60.00)

train$cherbourg <- as.numeric(train$Embarked == "C")
train$queenstown <- as.numeric(train$Embarked == "Q")
train$southampton <- as.numeric(train$Embarked == "S")

# Factor the classification column
train$Survived <- as.factor(train$Survived)


# Simplifying dataset
train_reg_df <- train %>%
  select(Survived, Age, SibSp, Parch, Fare, male, middle_class, upper_class, minor, senior, cherbourg, queenstown) %>%
  filter(!is.na(Age)) %>%
  filter(!is.na(cherbourg))


## EDA ------------------------------------------------------------------------

# corrplot
corrs <- cor(train_reg_df)
corrplot(corrs, method = 'square', type = 'upper')


## First Model: Logistic Regression -------------------------------------------
model_glm <- train(Survived ~ ., 
                   data = train_reg_df, 
                   method = 'glm',
                   family = 'binomial',
                   preProcess = c('scale', 'center'))

## Prediction and classification
glm_pred <- predict(model_glm)

## Confusion Matrix
xtab <- table(glm_pred, train_reg_df$Survived)
confusionMatrix(xtab) #0.802 a fine start




## Second Model: Logistic Regression w/ CV ------------------------------------
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     savePredictions = TRUE)

model_glm_cv <- train(Survived ~ .,  
                 data = train_reg_df, 
                 method = "glm", 
                 family = "binomial",
                 trControl = ctrl, 
                 tuneLength = 5)

glm_pred_cv <- predict(model_glm_cv)

xtab_2 <- table(train_reg_df$Survived, glm_pred_cv)
confusionMatrix(xtab_2) # The exact same -_-

## Third Model: KNN -----------------------------------------------------------
model_knn <- train(Survived ~ .,  
                 data = train_reg_df, 
                 method = "knn",
                 trControl = ctrl, 
                 tuneLength = 5) 

model_knn # 0.68 P U

## Fourth Model: Random Forest ------------------------------------------------
ctrl_2 <- trainControl(method = "oob", 
                     number = 10, 
                     savePredictions = TRUE)


model_rf <- train(Survived ~ .,
                  data = train_reg_df,
                  method = 'ranger',
                  trControl = ctrl_2,
                  preProcess = c('scale', 'center')) 

model_rf # 0.8188 a little better

## Fifth Model: Adaboost ------------------------------------------------------
model_ada <- train(Survived ~ .,
                  data = train_reg_df,
                  method = 'ada',
                  trControl = ctrl)

model_ada #0.8149 meh

