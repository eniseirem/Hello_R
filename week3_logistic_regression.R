library(readr)
library(caret)
library(naniar)
library(ggplot2)


url <- "C:/Users/Lenovo/Downloads/adult_income.csv"
work <- read.csv(url, header=T)

summary(work)


work[work == " ?"]<-NA


work$race <- as.factor(work$race)
work$sex <- as.factor(work$sex)
work$workclass <- as.factor(work$workclass)
work$marital_status <- as.factor(work$marital_status)
work$occupation <- as.factor(work$occupation)
work$education_num <- as.factor(work$education_num)


#I'm checking the data itself.
xtabs(~ income_high + sex, data=work)
xtabs(~ income_high + race, data=work)
xtabs(~ income_high + occupation, data=work)
xtabs(~ income_high + workclass, data=work)

#I try with only sex first to see. 
logistic <- glm (income_high ~ sex, data=work, family = "binomial")
summary(logistic)

#check with all data
#logistic2 <- glm (income_high ~ ., data=work, family = "binomial")
#summary(logistic2)

#some of them has high p-values

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic2$null.deviance/-2
ll.proposed <- logistic2$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic2$coefficients)-1))

# predicted.data <- data.frame(
#   probability.of.income=logistic$fitted.values,
#   sex=work$sex)
# 
# ggplot(data=predicted.data, aes(x=sex, y=probability.of.income)) +
#   geom_point(aes(color=sex), size=5) +
#   xlab("Sex") +
#   ylab("Predicted probability of high income")

logistic3 <- glm(income_high ~ sex+race+age+workclass+education_num+income_high, data=work, family="binomial")

predicted.data <- data.frame(
  probability.of.income=logistic3$fitted.values,
  income=work$income_high)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.income, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.income)) +
  geom_point(aes(color=work$income_high), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of having high income")



#I did manage to reach results with my first prediction however my last model was not good 
