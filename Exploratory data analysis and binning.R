require(caret)
library(smbinning)

#load the data
german_credit <- read.csv("~/german_credit2.csv")
train <-createDataPartition(y=german_credit$Creditability,p=0.75,list=FALSE)
train2 <- german_credit[train, ]
test2 <- german_credit[-train, ]

#Explore data distribution 
ggplot(german_credit, aes(x = duration,y = ..count..,)) + geom_histogram(fill = "blue", colour = "grey60", size = 0.2, alpha = 0.2,binwidth = 5)
ggplot(german_credit, aes(x = Amount,y = ..count..,)) + geom_histogram(fill = "blue", colour = "grey60", size = 0.2, alpha = 0.2,binwidth = 1000)
ggplot(german_credit, aes(x = Age,y = ..count..,)) + geom_histogram(fill = "blue", colour = "grey60", size = 0.2, alpha = 0.2,binwidth = 5)
ggplot(german_credit, aes(x =Creditability,y = ..count..,)) + geom_histogram(fill = "blue", colour = "grey60" , alpha = 0.2,stat="count")

#Optimal Binning
Durationresult=smbinning(df=train,y="Creditability",x="Duration",p=0.05)
CreditAmountresult=smbinning(df=train2,y="Creditability",x="CreditAmount",p=0.05) 
Ageresult=smbinning(df=train2,y="Creditability",x="Age",p=0.05) 

smbinning.plot(CreditAmountresult,option="WoE",sub="CreditAmount") 
smbinning.plot(Durationresult,option="WoE",sub="Duration")
smbinning.plot(Ageresult,option="WoE",sub="Age")