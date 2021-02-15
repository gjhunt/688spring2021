# ---
# jupyter:
#   jupytext:
#     formats: ipynb,Rmd,R
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.9.1
#   kernelspec:
#     display_name: ''
#     name: ''
# ---

# # Lab - KNN Classification
# ## Lecture 7 

data(iris)

iris

summary(iris)

library('class')

?knn

dset = iris[,c('Species','Sepal.Length','Sepal.Width')]
head(dset)

set.seed(12093)
randomized = sample(1:nrow(dset))
randomized

train_ss = randomized[1:floor(nrow(dset)/3)]
test_ss = setdiff(1:nrow(dset),train_ss)
head(train_ss)
head(test_ss)

trainx = dset[train_ss,-1]
trainy = dset[train_ss,1]
testx = dset[test_ss,-1]
testy = dset[test_ss,1]

# ### let's build our knn model!

pred_y = knn(train=trainx,testx,cl=trainy,k=5)
head(pred_y)

table(pred_y)

table(testy)

acc = mean(testy==pred_y)
acc


