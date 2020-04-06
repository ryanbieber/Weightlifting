##Modelling with Caret 
Train <- createDataPartition(df_model$medal, p=0.8, list=FALSE)
training <- df_model[ Train, ]
testing <- df_model[ -Train, ]
train_binary <- training$medal

mod_fit <- train(as.factor(medal) ~ as.factor(gender) + as.factor(nation) + bw_diff + age + as.factor(category),  
                  data=training, method="glm", family="binomial")
logit <- predict(mod_fit, newdata=testing, type="prob")
logit$prob <- ifelse(logit$`1`>.5, 1, 0)
confusionMatrix(data = as.factor(logit$prob), testing$medal)

training$gender <- as.factor(training$gender)
training$nation <- as.factor(training$nation)
training$category <- as.factor(training$category)
training_en <- training[,2:6]

##fixing medal variable for glmnet
training$medal <- ifelse(training$medal==1, "yes", "no")
testing$medal <- ifelse(testing$medal==1, "yes", "no")

# elastic net
trControl <- trainControl(method="cv", number=5, 
                          verboseIter = FALSE, 
                          returnResamp = "all",
                          classProbs = TRUE,
                          savePredictions="final")

mod_elastic_net <- train(medal ~ gender + nation + category + bw_diff + age, data = training, method = "glmnet", 
                             trControl = trControl ,
                             tuneGrid = expand.grid(alpha = 1,
                                                    lambda = seq(0.001,0.1,by = 0.001)))
elastic_net<- predict(mod_elastic_net, newdata=testing, type="prob")
elastic_net$prob <- ifelse(elastic_net$yes>.5, "yes", "no")

confusionMatrix(as.factor(elastic_net$prob), as.factor(testing$medal))

# random forest

mod_random_forest <- train(medal ~ gender + nation + category + bw_diff + age, data = training, method = "rf", 
                         trControl = trControl )
random_forest <- predict(mod_random_forest, newdata=testing, type="prob")
random_forest$prob <- ifelse(random_forest$yes>.5, "yes", "no")

confusionMatrix(as.factor(random_forest$prob), as.factor(testing$medal))

# xgboost tree

mod_xgboost <- train(medal ~ gender + nation + category + bw_diff + age, data = training, method = "xgbTree", 
                           trControl = trControl )
xgboost <- predict(mod_xgboost, newdata=testing, type="prob")
xgboost$prob <- ifelse(xgboost$yes>.5, "yes", "no")

confusionMatrix(as.factor(xgboost$prob), as.factor(testing$medal))

# nnet

mod_nnet <- train(medal ~ gender + nation + category + bw_diff + age, data = training, method = "nnet", 
                     trControl = trControl )
nnet <- predict(mod_nnet, newdata=testing, type="prob")
nnet$prob <- ifelse(nnet$yes>.5, "yes", "no")

confusionMatrix(as.factor(nnet$prob), as.factor(testing$medal))


model_list <- caretList(
  medal~., data=training,
  trControl=trControl,
  methodList=c("glm", "rf", "nnet", "xgbTree", "glmnet")
)

modelCor(resamples(model_list))

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
summary(greedy_ensemble)

library("caTools")
model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"yes"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$medal)
