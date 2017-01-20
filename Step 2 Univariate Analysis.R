#Univariate Analysis
AccountBalancewoe=woe(train2, "AccountBalance",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
ggplot(AccountBalancewoe, aes(x = BIN, y = -WOE)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "AccountBalance") 
ValueSavingswoe=woe(train2, "ValueSavings",Continuous = F, "Creditability",C_Bin = 5,Good = "1",Bad = "0")
ggplot(ValueSavingswoe, aes(x = BIN, y = -WOE)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "ValueSavings") 
ValueSavingswoe
Lengthofcurrentemploymentwoe=woe(train2, "Lengthofcurrentemployment",Continuous = F, "Creditability",C_Bin = 4,Good = "1",Bad = "0")
ggplot(Lengthofcurrentemploymentwoe, aes(x = BIN, y = -WOE)) + geom_bar(stat = "identity",fill = "blue", colour = "grey60",size = 0.2, alpha = 0.2)+labs(title = "Lengthofcurrentemployment") 

#combine some bins
for(i in 1:750){
  if(train$ValueSavings[i]==1){train2$ValueSavings[i]=2}
}
for(i in 1:750){
  if(train2$Lengthofcurrentemployment[i]==5){train2$Lengthofcurrentemployment[i]=4}
}
