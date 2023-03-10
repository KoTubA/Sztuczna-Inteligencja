library(class)

data=read.csv("C:/Projects/R Projects/data.csv")
data = subset(data,select=-id)
data$Grade = factor(data$Grade)

acc = function(x)
{sum(diag(x))/sum(x)}

nor <- function(x) { (x - min(x)) / (max(x) - min(x)) }

result <- vector()
listk <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
odchyleniaOdK <- vector()

for(k in 1:length(listk)){
  for(i in 1:100){
    idx = sample(1:nrow(data), 0.8*nrow(data))
    
    cl = data$Grade
    
    clTrain = cl[idx]
    clTest = cl[-idx]
    
    data_in = data[, -ncol(data)]
    data_norm = as.data.frame(lapply(data_in, nor))
    
    train = data_norm[idx,]
    test = data_norm[-idx,]
    
    model = knn(train, test, cl=clTrain, k=k)
    
    tab = table(model, clTest)
    
    quality = acc(tab)
    
    result = c(result, quality)
    
    
  }
  cat(k, "Srednia arytmetyczna:", mean(result))
  cat(" Odchylenie Standardowe:", sd(result), "\n")
  
  odchyleniaOdK = c(odchyleniaOdK, sd(result))
}

print(tab)
