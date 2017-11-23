out <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/Output2level.csv")
inp <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/output1level.csv")
write.csv(out,"Output2level.csv",row.names=F)

rmse <- function(a,b) 
{
  sqrt(sum((a-b)**2)/length(a))
}
mae <- function(a,b)
{
  sum(abs(a-b))/length(a)
}
mse <- function(a,b)
{
  sum((a-b)**2)/length(a)
}

smp_size <- floor(4000)
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(inp)), size = smp_size)
train <- inp[-train_ind, ]
test <- inp[train_ind, ]

# IBCF + PAM => Random Forest
library(randomForest)
fit <- randomForest(real ~ IBCF+PAM, train, ntree=400)
pred = predict(fit,test[,c("IBCF","PAM")])

out <- cbind("IBCF+PAM-RF"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9872963
# mae 0.7847105
# mse 0.974754


# IBCF + PAM => Linear Regression
fit <- lm(real ~ IBCF+PAM, train)
pred = predict(fit,test[,c("IBCF","PAM")])

out <- cbind("IBCF+PAM-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9527195
# mae 0.7523591
# mse 0.9076745


# IBCF + PAM =>  SVM
library(kernlab)
fit <- ksvm(real~IBCF+PAM, data = train, kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
# make predictions
pred <- predict(fit, test[,c("IBCF","PAM")], type="response")

out <- cbind("IBCF+PAM-SVM"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9577308
# mae 0.7477599
# mse 0.9172483


# Content + PAM => Linear Regression
fit <- lm(real ~ content+PAM, train)
pred = predict(fit,test[,c("content","PAM")])

out <- cbind("Content+PAM-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 1.03687
# mae 0.8301739
# mse 1.075099


# Content + PAM => Random Forest
library(randomForest)
fit <- randomForest(real ~ content+PAM, train, ntree=400)
pred = predict(fit,test[,c("content","PAM")])

out <- cbind("content+PAM-RF"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 1.066149
# mae 0.8519627
# mse 1.136674


# IBCF + Content => Linear Regression
fit <- lm(real ~ content+IBCF, train)
pred = predict(fit,test[,c("content","IBCF")])

out <- cbind("IBCF+Content-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9649371
# mae 0.7619325
# mse 0.9311036


# IBCF + POP =>  SVM
library(kernlab)
fit <- ksvm(real~IBCF+POP, data = train, kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
# make predictions
pred <- predict(fit, test[,c("IBCF","POP")], type="response")

out <- cbind("IBCF+POP-SVM"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9384255
# mae 0.7335157
# mse 0.8806424

# IBCF + POP => Linear Regression
fit <- lm(real ~ POP+IBCF, train)
pred = predict(fit,test[,c("POP","IBCF")])

out <- cbind("IBCF+POP-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.932822
# mae  0.7362019
# mse 0.8701568


# IBCF + PAM + Content => Linear Regression
fit <- lm(real ~ IBCF+PAM+content, train)
pred = predict(fit,test[,c("IBCF","PAM","content")])

out <- cbind("IBCF+PAM+content-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.932822
# mae  0.7362019
# mse 0.8701568


# IBCF + PAM + Content => NN
net <- neuralnet(real~ IBCF+PAM+content, data = train , hidden=3, threshold=0.01, stepmax = 1e06)
pred <- compute(net, test[,c("IBCF","PAM","content")]) #Run them through the neural network
pred <- pred$net.result[1:4000]

out <- cbind("IBCF+PAM+content-NN"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9460231845
# mae  0.7471734421
# mse 0.8949598656


# IBCF + POP + UBCF => Linear Regression
fit <- lm(real ~ IBCF+POP+UBCF, train)
pred = predict(fit,test[,c("IBCF","POP","UBCF")])

out <- cbind("IBCF+POP+UBCF-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9327
# mae  0.7364
# mse 0.8700


# POP + PAM + Content => Linear Regression
fit <- lm(real ~ POP+PAM+content, train)
pred = predict(fit,test[,c("POP","PAM","content")])

out <- cbind("POP+PAM+content-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9327
# mae 0.7364
# mse 0.8700


# UBCF + POP + PAM + Content => Linear Regression
fit <- lm(real ~ UBCF+POP+PAM+content, train)
pred = predict(fit,test[,c("UBCF","POP","PAM","content")])

out <- cbind("UBCF+POP+PAM+content-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9610
# mae 0.7574
# mse 0.9235


# IBCF + POP + PAM + Content => Linear Regression
fit <- lm(real ~ IBCF+POP+PAM+content, train)
pred = predict(fit,test[,c("IBCF","POP","PAM","content")])

out <- cbind("IBCF+POP+PAM+content-LR"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 0.9302
# mae 0.7341
# mse 0.8654


# UBCF + Content + PAM => Random Forest
library(randomForest)
fit <- randomForest(real ~ UBCF + content + PAM, train, ntree=400)
pred = predict(fit,test[,c("UBCF","content","PAM")])

out <- cbind("UBCF+content+PAM-RF"=pred,out)
rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# rmse 1.0060
# mae 0.7975
# mse 1.0121
