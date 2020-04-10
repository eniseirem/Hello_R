library(dplyr)

setwd('C:/Users/Lenovo/Downloads')

data=read.csv('Forddata.csv', header=T)
head(data)

#I realized header not seem ok so before I change it I try with declaring separater and it worked.

data=read.csv('Forddata.csv', sep=';')
head(data)

#lets fix date s format
data$Date.bought <- as.Date(as.character(data$Date.bought),"%d.%m.%Y")

data$Amount <- as.double(data$Amount, ",")
head(data)

#lets see ford and others as bar graph

boughtdata <- table(data$Bought)
barplot(boughtdata)


#we can clearly see that customers choose other ones almost twice as ford. I'll change ford as 1 other as 0 
data$Bought <- ifelse(data$Bought == "ford",1,0)
head(data)

data$Date.bought <- as.Date(data$Date.bought)
class(data$Date.bought)

data$last_date <- as.Date("20-10-2012")

#find the last date of data

data <- data %>% mutate("last_date" = max(Date.bought))

data$recency <- difftime(data$last_date, data$Date.bought, units = "days")
#as we can see our last date is in 2012
tail(data)

#lets see our customer details
data <- data %>% arrange(Customer) 
head(data)

data <- data %>% group_by(Customer) %>% mutate("frequency" = length(Bought))

data <- data %>% group_by(Customer) %>% mutate("monetary" = sum(Amount))

#data <- data[!(duplicated(data$Customer)),] will be only one customer but date will be according to first purchase

frequency <- as.data.frame(table(data[,2]))

#lets create our rfm 

data$r <- ntile(data$recency,4)
data$f <- ntile(data$frequency,4)
data$m<- ntile(data$monetary,4)

head(data)
#I'm going to play with segments since I want best user to be 111
#for f

data$f <- with(data, ifelse(f == 1,4, ifelse(f == 2,3, ifelse(f == 3,2, ifelse(f == 4,1, "other")))))
data$f <- as.integer(data$f)
#for m

data$m <- with(data, ifelse(m == 1,4, ifelse(m == 2,3, ifelse(m == 3,2, ifelse(m == 4,1, "other")))))
data$m <- as.integer(data$m)

#we can merge them for betterlook and calculate our total score also

data$rfm_class <- with(data, paste(r,f,m,sep = ""))
head(data)

tail(data)
data$Total_Score <- c(100*data$recency + 10*data$frequency+data$monetary)


#forgot to change rfm_class and recency to numeric

data$recency<- as.numeric(data$recency)
data$rfm_class<- as.numeric(data$rfm_class)
data$Total_Score<-as.numeric(data$Total_Score)
#now we can see our results 
par(mfrow = c(1,4))
hist(data$recency)
hist(data$frequency)
hist(data$monetary)
hist(data$Total_Score)

#as we design, I want to target customers who is recency is high first I try with rfm 
target <- data[data$rfm_class>=441,]
dim(target)
head(target)
#then score
target <- data[data$Total_Score>=700000,]
dim(target)
head(target)


