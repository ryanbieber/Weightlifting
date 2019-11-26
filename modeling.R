##Modelling
Train <- createDataPartition(df_model$medal, p=0.8, list=FALSE)
training <- df_model[ Train, ]
testing <- df_model[ -Train, ]


## logit modelling 
## cant use this one as I wanted to see if some other features would help 
# mod_fit <- train(as.factor(medal) ~ as.factor(gender) + as.factor(nation) + bw_diff + age + diff_snatch + diff_jerk + as.factor(bomb) + as.factor(category),  
#                  data=training, method="glm", family="binomial")
# logit1 <- predict(mod_fit, newdata=testing, type="prob")
# logit1$prob <- ifelse(logit1$`1`>.5, 1, 0)
# confusionMatrix(data = as.factor(logit1$prob), testing$medal)

mod_fit2 <- train(as.factor(medal) ~ as.factor(gender) + as.factor(nation) + bw_diff + age + as.factor(category),  
                  data=training, method="glm", family="binomial")
logit2 <- predict(mod_fit2, newdata=testing, type="prob")
logit2$prob <- ifelse(logit2$`1`>.5, 1, 0)
confusionMatrix(data = as.factor(logit2$prob), testing$medal)







