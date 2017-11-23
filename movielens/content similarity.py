import csv
import os
import sys
import pandas as pd
os.chdir('C:/Users/Bhavya Ghai/Desktop/M.Tech Project/movielens')

def jaccard(a,b) :
  l = len(a)
  p = s = 0
  for i in range(1,l+1):
    if a[i-1]==1 and b[i-1]==1:
      p = p + 1
    elif a[i-1]==0 and b[i-1]==0:
      s = s + 1
  return p/(l-s)

data = pd.read_csv('genres.csv', header= None)

# Creating movie-movie matrix
size = len(data.index)
mat = pd.DataFrame(index=range(size), columns=range(size))
for i in range(size):
  for j in range(size):
    if i<j: continue
    elif i == j:  mat.iloc[i,j] = 1
    else: 
      mat.iloc[i,j] = mat.iloc[j,i] = jaccard(data.iloc[i,:], data.iloc[j,:])

mat.to_csv("movie-similarity.csv", sep='\t')

