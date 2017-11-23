# Building Genre based recommendation system
# Loading & preprocessing
setwd("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens")
data <- read.csv("movie.item",sep="|", header = F)
str(data)
data$V2 <- data$V3 <- data$V4 <- data$V5 <- NULL 
summary(data)
names(data)[1] <- "movie id"
# removing movie id as it same as row names
data[,1] <- NULL
# writing genres for python
write.table(data,"genres.csv",row.names= F, col.names=F, sep=",")

# Building movie-movie matrix using similarity
# We'll use Jaccard'd coefficient for similarity
# J = 11/10+01+11
# range is [0,1]
jaccard <- function(a,b)
{
  len <- len(a)
  p <- s <- 0
  for(i in 1:len)
  {
    if(a[i]==1 & b[i]==1) {
      p <- p + 1
    }
    else if(a[i]==0 & b[i]==0) {
      s <- s + 1
    }
  }
  p/(len-s)
}

# Creating movie-movie matrix
size <- nrow(data)
mat <- matrix(, nrow = size, ncol = size)
for(i in 1:size)
{
  print(i)
  for(j in 1:size)
  {
    if(i<j) next
    else if(i == j)  mat[i,j] <- 1
    else 
      mat[i,j] <- mat[j,i] <- jaccard(data[i,], data[j,])
  }
}

sim <- read.delim("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/movie-similarity.csv")
sim[,1] <- NULL
write.csv(sim,"movie-similarity.csv", row.names= F)

train <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/train.csv")
train[,1] <- NULL

install.packages('reshape2')
library('reshape2')

rat <- read.delim("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/ratings.data", header=FALSE)
rat[,4] <- NULL
names(rat) <- c("user","item","rating")
m <- dcast(rat, user ~ item, value.var = 'rating')
m[,1] <- NULL

test <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/test.csv")
test[,1] <- NULL
for(i in 1:20000)
{
  m[test[i,1],test[i,2]] <- NA
}
sum(!is.na(m)) # 80000 sweet
# so now matrix m is ready with only training data

# ind is used to know the # movies watch by each user 

ind  <- vector(mode="numeric", length=943)
# watch contains list of movies watch by each user
watch <- matrix(, nrow=943,ncol=600)

for(i in 1:943)
{
  for(j in 1:1682)
  {
    if(!is.na(m[i,j]))
    {
      ind[i] <- ind[i] + 1
      watch[i,ind] <- j 
    }
  }
}
watch[1,]

# max # of movies watched 591 in trainset
# max(ind)
# max # of movies watched 11 in trainset
# min(ind)
# summary(ind)



pred <- vector(mode="numeric",length=20000)
for(i in 1:20000)
{
  u <- test[i,1]
  mov <- test[i,2]
  pred[i] <- sum(sim[mov,watch[u,1:ind[u]]]*m[u,watch[u,1:ind[u]]])/sum(sim[mov,watch[u,1:ind[u]]])
  if(is.nan(pred[i]))
    pred[i] <- mean(m[,mov], na.rm=T)
  if(is.nan(pred[i]))
    pred[i] <- mean(as.numeric(m[u,]),na.rm=T)
}

rmse(pred[1:20000],test[1:20000,3])

output <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/2nd level/Final Output.csv")
output$"content" <- pred
output <- output[c(1,2,3,4,6,5)]
write.csv(output,"output1level.csv",row.names=F)
cor(output)

rmse(output[,5],output[,6])
mse(output[,5],output[,6])
mae(output[,5],output[,6])
save.image()

summary(pred)
