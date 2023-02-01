library(naivebayes)

data=read.csv("C:/Projects/R Projects/data.csv")
data = subset(data,select=-id)
data$Grade = factor(data$Grade)

xtabs(~ Grade, data=data)

myList <- list()
for (x in 1:100) {
  idx=sample(2,nrow(data),replace=T,prob = c(0.8,0.2))
  idx
  
  train=data[idx==1,]
  test=data[idx==2,]
  
  model=naive_bayes(Grade ~ .,data=train)
  plot(model)
  
  p=predict(model,train)
  tab=table(p,train$Grade)
  tab
  sum(diag(tab))/sum(tab)
  

  p1=predict(model,test)
  tab1=table(p1,test$Grade)
  tab1
  s = sum(diag(tab1))/sum(tab1)
  
  myList <- append(myList,s)
  
  cat("x", x, ": ", s)
  print("")
}

srednia = mean(unlist(myList))
srednia
odchylenie = sd(unlist(myList))
odchylenie