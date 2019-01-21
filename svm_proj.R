library(ggplot2)
library(caTools)
library(e1071)


loans <- read.csv('loan_data.csv')


#Factorize categorical fields
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

#Exploratory Analysis
pl <- ggplot(loans, aes(fico)) +
  geom_histogram(aes(fill = not.fully.paid), color = 'black', bins = 40, alpha = 0.5) +
  theme_bw() +
  scale_fill_manual(values = c('green','red'))

pl2 <- ggplot(loans, aes(x = factor(purpose))) +
  geom_bar(aes(fill = not.fully.paid), position = 'dodge') +
  theme_bw()

pl3 <- ggplot(loans, aes(int.rate, fico)) +
  geom_point(aes(color = not.fully.paid, alpha = 0.4)) +
  theme_bw()

#Partition data
set.seed(101)
sample <- sample.split(loans$not.fully.paid, 0.7)
train <- subset(loans, sample = T)
test <- subset(loans, sample = F)

#SVM Model
model <- svm(not.fully.paid ~., data = train)
print(summary(model))
pred <- predict(model, test[1:13])
table(pred, test$not.fully.paid)

#Optimization
tuning <- tune(svm, train.x = not.fully.paid ~ ., data = train,
               kernel = 'radial', ranges = list(cost = c(100, 200), gamma = c(0.1)))
print(summary(tuning))

#Optimized model
tuned.model <- svm(not.fully.paid ~., data = train, cost = 100, gamma = 0.1)
tuned.pred <- predict(tuned.model, test[1:13])
table(tuned.pred, test$not.fully.paid)

#Metrics
misclass <- mean(test$not.fully.paid != tuned.pred)
acc <- 1 - misclass