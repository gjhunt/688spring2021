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
#     display_name: R
#     language: R
#     name: ir
# ---

# # Lab 20 - Random Forests
# ## Lecture 20 

library('bayesreg')

data(spambase)

spambase

?spambase

install.packages('randomForest')

library('randomForest')

?randomForest

dim(spambase)

sqrt(57)

colnames(spambase)[50:55] <- paste0('char.freq.',c('semic','paren','bracket','exclaim','dollar','pound'))

spambase$is.spam <- factor(spambase$is.spam)

spambase$is.spam

rf = randomForest(is.spam~.,data=spambase,ntree=100,mtry=floor(sqrt(57)))

print(rf)

library('rpart')

ctree = rpart(is.spam~.,data=spambase)

mean(spambase$is.spam!=predict(ctree,type='class'))

rf = randomForest(is.spam~.,data=spambase,ntree=500,mtry=floor(sqrt(57)))

print(rf)

names(rf)

plot(rf)

rf$confusion

matplot(rf$err.rate)

rf$importance

rf$importance[order(rf$importance,decreasing=TRUE),]


