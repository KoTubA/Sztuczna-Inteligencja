library("party")
res = c()

data=read.csv("C:/Projects/R Projects/rice_classification.csv")
data = subset(data,select=-id)
data$Class = factor(data$Class)

for(i in 1:100) {
  idx=sample(1:nrow(data),0.8*nrow(data))
  
  train = data[idx,]
  test = data[-idx,]
  
  model = ctree(Class ~ ., data=train)
  p=predict(model,test)
  cm <- table(p, test$Class)
  quality = sum(diag(cm))/sum(cm)
  res = append(res, quality)
  
  cat("i", i, ": ", quality)
  print("")

}

mean_res <- mean(res)
sd_res <- sd(res)

cat("Srednia arytmetyczna:", mean_res, "\n")
cat("Odchylenie Standardowe:", sd_res, "\n")
cm
