
install.packages("tree")
install.packages("ISLR")
library (ISLR)
library (tree)


churn <- read.csv("C:/Users/Lenovo/Desktop/Churn_Modelling.csv", header=T)

#Let's see out dataset variables first

head(churn)

#first need to clean to data so will drop the surnname, rownumber, customerid
#they are not gonna effect our result. 

churnNew = churn[, -c(1:3)]

#lets see our dataset details forbetter understanding of data distrubition

summary(churnNew)

Exited1 <- ifelse(churnNew$Exited== 0, "No", "Yes")

Cdata = data.frame(churnNew, Exited1)
head(Cdata)

#select random rows 

trainrows = sample(1:nrow(Cdata), 500)
traindata = Cdata[trainrows,]
testdata = Cdata[-trainrows,]

treeModel = tree(formula = Exited1 ~., data = traindata)
summary(treeModel)


plot(treeModel)
text(treeModel, pretty=0)
testpredictions = predict(treeModel, testdata, type ="class")

#accuracy

cd =table(testpredictions, testdata$Exited1)
accuracy = (cd[1,1] + cd[2,2]) / (cd[1,1] + cd[2,2] + cd[1,2] + cd[2,1])
print(accuracy)

#accuracy is stated as 1

