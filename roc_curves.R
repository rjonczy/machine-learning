# http://blog.yhathq.com/posts/roc-curves.html
# ROC Curves in Python and R

# check ... for more details
# https://en.wikipedia.org/wiki/Sensitivity_and_specificity

library(ggplot2)

diamonds$is_expensive <- diamonds$price > 2400
is_test <- runif(nrow(diamonds)) > 0.75
train <- diamonds[is_test==FALSE,]
test <- diamonds[is_test==TRUE,]

fit <- glm(is_expensive ~ carat + cut + clarity, data=train)
summary(fit)


table(train$is_expensive)
length(train$is_expensive[train$is_expensive == TRUE])
length(train$is_expensive[train$is_expensive == FALSE])


library(ROCR)

prob <- predict(fit, newdata = test, type="response")
pred <- prediction(prob, test$is_expensive)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))



