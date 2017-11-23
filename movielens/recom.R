#### Kaggle in Class Problem. ....
####Reference: https://inclass.kaggle.com/c/predict-movie-ratings

# Set data path as per your data file (for example: "c://abc//" )
# setwd("/home/ashokharnal/Documents/")
setwd("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens")

# If not installed, first install following three packages in R
install.packages('recommenderlab')
library(recommenderlab)
library(reshape2)
library(ggplot2)
# Read training file along with header
# tr<-read.csv("train_v2.csv",header=TRUE)
tr <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/ratings.data",sep="\t",header=F)
# Just look at first few lines of this file
head(tr)
# Remove 'id' column. We do not need it
tr<- tr[,-c(4)]
# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
# to make our data sync with the script data
names(tr) <- c("user","movie","rating")
g<-acast(tr, user ~ movie)
# g is user item matrix
# Check the class of g
class(g)
dim(g)
# removing test values from whole matrix
test <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/test.csv")
test[,1] <- NULL

for(i in 1:20000)
{
  g[test[i,1],test[i,2]] <- NA
}
# Convert it as a matrix
R<-as.matrix(g)

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
rm(R,g)

# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(r, main = "Raw Ratings")       
#image(r_m, main = "Normalized Ratings")

# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
rec=Recommender(r[1:nrow(r)],method="POPULAR")
rec=Recommender(r[1:nrow(r)],method="RANDOM",param=list(minrating=1))

# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$nn

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(r) shows there are 6040 users (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom

########## Create submission File from model #######################
# Read test file
# test<-read.csv("test_v2.csv",header=TRUE)
test <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/test.csv")
head(test)
# Get ratings list
rec_list<-as(recom,"list")
ratings<-NULL
# For all lines in test file, one by one
for ( u in 1:length(test[,2]))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- test[u,1]
  movieid<-test[u,2]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
pred <- ratings
summary(pred)
plot(pred)
# UBCF, POP adjusting 
for(i in 1:20000)
{
  if(pred[i] < 1) { pred[i] = 1}
  else if(pred[i] > 5) { pred[i] = 5 }
}

#IBCF adjusting values <1
for(i in 1:20000)
{
  if(pred[i] < 1) 
  {
    pred[i] = mean(tr[tr$movie==test[i,2],3])
  }  
}
# Testing
out <- read.csv("C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens/output1level.csv")
summary(pred)
summary(out$real)
out$POP <- pred
write.csv(out,"output1level.csv",row.names=F)

rmse(out$real,pred)
mae(out$real,pred)
mse(out$real,pred)
# https://ashokharnal.wordpress.com/2014/12/18/using-recommenderlab-for-predicting-ratings-for-movielens-data/
