setwd("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens")
inp <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/output2level.csv")
inp <- inp[,-c(16:20)]
names(inp)
names(inp) <- c("10","14","13","12","11","9","8","15","7","6","5","4","3","2","1","real")
inp <- inp[,c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","real")]
write.csv(inp,"output2level.csv",row.names=F)

library(corrplot)
corrplot(cor(inp[,1:15]), method="circle")
round(cor(inp[,1:15]),4)

#inp <- inp[,c(13,11,10,3,2,16)]
smp_size <- floor(800)
## set the seed to make your partition reproductible
set.seed(1234)
train_ind <- sample(seq_len(nrow(inp)), size = smp_size)
train <- inp[-train_ind, ]
test <- inp[train_ind, ]
#names(inp) <- c("3","5","6","13","14","real") 


out <- data.frame("all" = pred)
write.csv(out,"finalOutput.csv",row.names=F)



# IBCF + PAM => Linear Regression
fit <- lm(real ~  j+n+o, data = train)
pred <- predict(fit, test[,c("j","n","o")])

fit <- ksvm(real ~  e+n+o, data = train, kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
pred <- predict(fit, test[,c("e","n","o")], type="response")

names(test) <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","real")
names(train) <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","real")
# 13,11,10,3,2,16
round(rmse(test$real,pred),4)
round(mae(test$real,pred),4)
round(mse(test$real,pred),4)
# rmse 0.9167
# mae 0.7164
# mse 0.8404

# 7 + 8
0.9188  0.7185  0.8442

# 4 +5 + 7 +10
0.9189 0.7174  0.8444

# 14+15
0.9154  0.707   0.8379