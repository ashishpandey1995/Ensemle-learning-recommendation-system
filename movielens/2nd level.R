da <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/output1level.csv")
setwd("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens")
inp <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/Output2level.csv", header=T)


for(i in 1:5) 
{
  print(rmse(da[,i], da[,6]))
}
summary(da)

install.packages('corrplot')
library(corrplot)
corrplot(cor(da[1:5]), method="circle")
cor(da[1:5])

# Data Normalizing
#for(i in 1:ncol(da))
#{
#  da[,i] <-  (da[,i] - mean(da[,i])) / sd(da[,i]) 
#}

# spliting the data by 80/20 rule
smp_size <- floor(4000)
## set the seed to make your partition reproductible
set.seed(123)
 train_ind <- sample(seq_len(nrow(da)), size = smp_size)
 train <- da[-train_ind, ]
 test <- da[train_ind, ]


sol <- data.frame(real=test[,6])

# Random forest
library(randomForest)
fit <- randomForest(real ~ ., train, ntree=200)
pred = predict(fit,test[,1:5])

sol <- cbind(RF=pred,sol)
rmse(sol$real,sol$RF)
mae(sol$real,sol$RF)
mse(sol$real,sol$RF)
# rmse 0.9401431312
# mae 0.744280809
# mse 0.8838691072



#SVM
library(kernlab)
fit <- ksvm(real~., data = train, kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
# make predictions
predictions <- predict(fit, test[,1:5], type="response")

sol <- cbind(SVM=predictions,sol)
rmse(sol$real,predictions)
mae(sol$real,predictions)
mse(sol$real,predictions)
write.csv(sol,"Output2level.csv",row.names=F)
#rmse 0.9344535936
#mae 0.7272334386
#mse  0.8732035186

#Mean
pred <- mean(test[1,1],test[1,2],test[1,3],test[1,4],test[1,5])
pred <- vector(,4000)
for(i in 1:4000)
{
  pred[i] <- mean(test[i,1],test[i,2],test[i,3],test[i,4],test[i,5])
}
rmse(sol$real,pred)
mae(sol$real,pred)
mse(sol$real,pred)
sol <- cbind(mean=pred,sol)
write.csv(sol,"Output2level.csv",row.names=F)
#rmse 1.015392442
#mae 0.802272058
#mse 1.03102181


#LR
fit <- lm(real ~ ., data=train)
pred <- predict(fit,test[,1:5])
rmse(sol$real,pred)
mae(sol$real,pred)
mse(sol$real,pred)
inp <- cbind(lr=pred,inp)
write.csv(inp,"Output2level.csv",row.names=F)
#rmse 0.9301736056
#mae 0.7337661304
#mse 0.8652229365


#NN
install.packages('neuralnet')
library('neuralnet')
net <- neuralnet(real~ UBCF+IBCF+PAM+POP+content, data = train , hidden=2, threshold=0.01, stepmax = 1e06)
#Plot the neural network
plot(net)
#Test the neural network on some training data
pred <- compute(net, test[,1:5]) #Run them through the neural network
# summarize the fit
#test <- data[13001:nrow(data),]
pred <- pred$net.result[1:4000]
length(pred)
rmse(sol$real,pred)
mae(sol$real,pred)
mse(sol$real,pred)
sol <- cbind(nn2=pred,sol)
write.csv(sol,"Output2level.csv",row.names=F)
#rmse 0.9908685871
#mae 0.7898606213
#mse 0.9818205569




library(ggplot2)
dat <- data.frame(
  type = factor(c("1-Level Arch","2-Level Arch","3-Level Arch"),levels = c("1-Level Arch","2-Level Arch","3-Level Arch")),
  prod = c(69.724,73.319,75.1735)
)


bp <- ggplot(data=dat, aes(x=type, y=prod, fill=type))  + ggtitle('Accuracy of Different Architectures') +  geom_bar(colour="black", stat="identity") + xlab('Type of Recommender Systems') + ylab('Accuracy') 
red.bold.italic.text <- element_text(face = "bold.italic", size = 20,color = "black")
bp + coord_cartesian(ylim=c(67, 77)) + theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, axis.text=red.bold.italic.text) + scale_fill_manual(values=c("green","blue","red")) 
