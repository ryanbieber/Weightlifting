##Modelling
Train <- createDataPartition(df_model$medal, p=0.8, list=FALSE)
training <- df_model[ Train, ]
testing <- df_model[ -Train, ]

## logit modelling 

mod_fit <- train(as.factor(medal) ~ as.factor(gender) + as.factor(nation) + bw + age + diff_snatch + diff_jerk + as.factor(bomb),  data=training, method="glm", family="binomial")
#exp(coef(mod_fit$finalModel))
predict(mod_fit, newdata=testing, type="prob")
pred = predict(mod_fit, newdata=testing)
accuracy <- table(pred, testing[,"medal"])
sum(diag(accuracy))/sum(accuracy)
accuracy


mod_fit2 <- train(as.factor(medal) ~ as.factor(gender) + as.factor(nation) + bw + age ,  data=training, method="glm", family="binomial")
predict(mod_fit2, newdata=testing, type="prob")
pred2 = predict(mod_fit2, newdata=testing)
accuracy2 <- table(pred2, testing[,"medal"])
sum(diag(accuracy2))/sum(accuracy2)
accuracy2


