#add the csv first 

citiesData <- read.csv("", header=T)

head.citiesData

#show attributes
attributes(citiesData)

plot(citiesData)
with(citiesData,plot(BlackPercent, PerCapitalIncome ylab= ,xlab= ,))

model <- kmeans(citiesData[-1],3) #♪k = 3
summary(model)
attributes(model)


model$cluster

#cluster size
model$size

#assing cluster model number to the original data
citiesData <- cbind(citiesData.Cluster=model$cluster)

model$withinss #within cluster sum of square error  #larger error = cluster large, small = not far between items, similar
                #we want to minimize the error maximize the distance between clusters so it depends the k number
                #sharp drop of the error, breakpoint is the goodpoint for number of clusters (elbow criteria TODO: SEARCH)

#wssplot( citiesData[-1],nc=5) from the pic I took

clusplot() #2 dimensional

###############################
#Hierarchial cluster
###############################
d<-dist(citiesData[-1], method="euclidean")
H_model <- hclust(d, method="ward.D")


groups<-
  
  table<-


