getwd()
bp  = read.csv('Book_2.csv',header= TRUE )
library(rpart)
library(dplyr)
library(caret)
library(ggplot2)
library(Metrics)
set.seed(12345)
# Create train and test datasets. 
train_score_rows <- createDataPartition(bp$Scores,
                                        p = 0.3,
                                        list=FALSE)
train_score <- bp[train_score_rows,]
summary(train_score)
test_score <- bp[-train_score_rows,]  

x= train_score$Hours
y=train_score$Scores

plot(x, y, main = "Hours-Scores",
     xlab = "hours", 
     ylab = "scores")
# Add regression line
abline(lm(y ~ x, data = train_score), col = "blue")
cor(x,y)
# Linear Regression
lm_score <- lm(Scores ~ Hours, data = train_score)
p <-  data.frame(Hours = 9.25)
predict(lm_score, p)
#calculate MAE between predicted values and observed values
mae(train_score$Scores,predict(lm_score))

# To evaluate the model we can also use R^2
pred_linreg <- predict(lm_score,test_score)
Y_test<- test_score$Scores
error <- Y_test - pred_linreg
R2=1-sum(error^2)/sum((Y_test- mean(Y_test))^2)
R2
  ##an r-squared of 93% reveals that 93% of the data fit the regression model

##Also we can use adjusted R^2 to evaluate the model

mean_squared_error <- mse(test_score$Scores,pred_linreg)
mean_squared_error
Adj_R2 <- 1-(mean_squared_error/var(Y_test))
Adj_R2
   




